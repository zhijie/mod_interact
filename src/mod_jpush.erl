%%%----------------------------------------------------------------------

%%% File    : mod_jpush.erl
%%% Author  : onezeros <onezeros.lee@gmail.com>
%%% Purpose : Forward offline messages by jpush push notification  
%%% Created : 08 Nov 2017 by onezeros <onezeros.lee@gmail.com>
%%%
%%%
%%% Copyright (C) 2017   onezeros
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% configuration： ejabberd.yml
%%% mod_jpush:
%%% 		app_key: "xxx"
%%% 		master_secret: "xxx"
%%% 		jpush_url: "https://api.jpush.cn/v3/push"
%%% note: tested in ejabberd 17.07
%%%----------------------------------------------------------------------

-module(mod_jpush).
-author('onezeros.lee@gmail.com').

-behaviour(gen_mod).

-export([start/2,
	 init/2,
	 stop/1,
	 send_notice/1]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_jpush", [] ),
    register(?PROCNAME,spawn(?MODULE, init, [Host, Opts])),  
    ok.

init(Host, _Opts) ->
    inets:start(),
    ssl:start(),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, send_notice, 10),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_jpush", [] ),
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, send_notice, 10),
    ok.

-spec send_notice({any(), message()}) -> {any(), message()}.
send_notice({_Action, #message{type = Type, body = Body, to = To, from = From}} = Acc) ->
    AppKey = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, app_key, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    MasterSecret = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, master_secret, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    JpushUrl4Cid = "https://api.jpush.cn/v3/push/cid",
    ?INFO_MSG("jpush config: appKey:~s, masterSecret:~s, JpushUrl4Cid:~s", [AppKey, MasterSecret,JpushUrl4Cid]),
	Lastone = lists:last(Body),
	BodyContent = Lastone#text.data,
	io:format("BodyContent ~p~n",[BodyContent]),
	BodyContentStr = binary_to_list(BodyContent),
	io:format("BodyContentStr : ~p~n",[BodyContentStr]),
    if (Type == chat) and (Body /= <<"">>) ->
        R = httpc:request(get,{JpushUrl4Cid, [{"Authorization","Basic YTY4YWQwYWI3MjMzNTAwOGU2OTEyNDA3OmMzZmQ5YjQ1NDU2ZGY1MGY4OGYwNzU5Zg=="}]},[],[]),
        {ok, {{RespondVersion,RespondCode, RespondState}, RespondHead, RespondBody}} = R,
        io:format("RespondVersion : ~p~n",[RespondVersion]),
        io:format("RespondCode : ~p~n",[RespondCode]),
        io:format("RespondState : ~p~n",[RespondState]),
        io:format("RespondHead : ~p~n",[RespondHead]),
        io:format("RespondBody : ~p~n",[RespondBody]),
        io:format("CidJson : ~p~n",[CidJson]),
        IndexStart = string:chr(RespondBody,'['),
        Cid = string:substr(RespondBody,IndexStart + 2, 61),
        io:format("Cid : ~p~n",[Cid]),
        Acc;
      true ->
        Acc
    end.


-spec fire_push({any(), message()}) -> {any(), message()}.
fire_push({_Action, #message{type = Type, body = Body, to = To, from = From}} = Acc) ->
    AppKey = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, app_key, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    MasterSecret = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, master_secret, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    JpushUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, jpush_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    ?INFO_MSG("jpush config: appKey:~s, masterSecret:~s", [AppKey, MasterSecret]),
	Lastone = lists:last(Body),
	BodyContent = Lastone#text.data,
	io:format("BodyContent ~p~n",[BodyContent]),
	BodyContentStr = binary_to_list(BodyContent),
	io:format("BodyContentStr : ~p~n",[BodyContentStr]),
    if (Type == chat) and (Body /= <<"">>) ->
		Sep = "&",
        Post = [
          "{ \"cid\": \"8103a4c62890b98974ec1949-711161d4-5f17-4d2f-a855-5e5a8909b26e\", \"platform\": \"all\", \"audience\": \"all\", \"notification\": {\"android\": {\"alert\": \"Hi, JPush!\",\"title\": \"Send to Android\",\"builder_id\": 1},\"ios\": {\"alert\": \"Hi, JPush!\",\"sound\": \"default\",\"badge\": \"+1\", \"options\": {\"time_to_live\": 60,\"apns_production\": false,\"apns_collapse_id\":\"jiguang_test_201711011100\" }}"],
        ?INFO_MSG("Sending post:~s", [ Post]),
        httpc:request(post, 
        	{binary_to_list(JpushUrl), [], "application/json", list_to_binary(Post)},
        	[{proxy_auth,{binary_to_list(AppKey),binary_to_list(MasterSecret) }}],
        	[{sync, true},{receiver, []}]),
        Acc;
      true ->
        Acc
    end.

%%% The following url encoding code is from the yaws project and retains it's original license.
%%% https://github.com/klacke/yaws/blob/master/LICENSE
%%% Copyright (c) 2006, Claes Wikstrom, klacke@hyber.org
%%% All rights reserved.
url_encode([H|T]) when is_list(H) ->
    [url_encode(H) | url_encode(T)];
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].

integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} -> old_integer_to_hex(I);
        Int         -> Int
    end.

old_integer_to_hex(I) when I < 10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I < 16 ->
    [I-10+$A];
old_integer_to_hex(I) when I >= 16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).


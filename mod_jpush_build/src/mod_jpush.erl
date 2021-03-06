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
%%% note: tested in ejabberd 17.07
%%% use: when a chat message send to a offline user, push jpush notification to this offline user.
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

get_nickname_from_uid(From) ->
    Uid = From#jid.user,
    Host = From#jid.server,
    VcardUrl = "http://" ++ binary_to_list(Host) ++ ":5280/api/get_vcard",
    Post = [
      "{ \"user\": \"", Uid , "\", \"name\": \"NICKNAME\", \"host\": \"" , Host,"\"}"],
    ?INFO_MSG("Sending post:~s", [ Post]),
    ?INFO_MSG("Sending post to Url:~s", [ VcardUrl]),
    RP = httpc:request(post, 
        {VcardUrl, [{"Authorization","Basic "}],
         "application/json",
        list_to_binary(Post)},
        [],
        [{sync, true}]),
    {ok, {{PostRespondVersion,PostRespondCode, PostRespondState}, PostRespondHead, PostRespondBody}} = RP,
    io:format("PostRespondVersion : ~p~n",[PostRespondVersion]),
    io:format("PostRespondCode : ~p~n",[PostRespondCode]),
    io:format("PostRespondState : ~p~n",[PostRespondState]),
    io:format("PostRespondHead : ~p~n",[PostRespondHead]),
    io:format("PostRespondBody : ~p~n",[PostRespondBody]),%%%{"content": "Schubert"}
    Nickname = string:sub_string(PostRespondBody,13,string:length(PostRespondBody) - 2),
    io:format("Nickname got using api : ~p~n",[Nickname]),
    Nickname.

-spec send_notice({any(), message()}) -> {any(), message()}.
send_notice({_Action, Message} = Acc) ->
    ?INFO_MSG("jpush debuging ----------------------------------------------", []),
    #message{type = Type, body = Body, to = To, from = From} = Message,
    io:format("From : ~p~n",[From]),
    io:format("To : ~p~n",[To]),
    io:format("Type : ~p~n",[Type]),
    io:format("Acc : ~p~n",[Acc]),
    AppKey = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, app_key, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    MasterSecret = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, master_secret, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    JpushUrl4Cid = "https://api.jpush.cn/v3/push/cid",
    JpushUrl = "https://api.jpush.cn/v3/push",
    ?INFO_MSG("jpush config: appKey:~s, masterSecret:~s, JpushUrl4Cid:~s", [AppKey, MasterSecret,JpushUrl4Cid]),
    ServerPrefix = string:prefix(From#jid.server,"conference"),
    io:format("ServerPrefix : ~p~n",[ServerPrefix]),
    if (Type == chat) and (Body /= <<"">>) ->
        Nickname = get_nickname_from_uid(From),
		Lastone = lists:last(Body),
		BodyContent = Lastone#text.data,
		io:format("BodyContent ~p~n",[BodyContent]),
		BodyContentStr = binary_to_list(BodyContent),
		io:format("BodyContentStr : ~p~n",[BodyContentStr]),
    	AuthOrg = binary_to_list(AppKey) ++ ":" ++ binary_to_list(MasterSecret),
        io:format("Auth : ~p~n",[AuthOrg]),
    	Auth = base64:encode_to_string(AuthOrg),
        io:format("Auth : ~p~n",[Auth]),
        R = httpc:request(get,{JpushUrl4Cid, [{"Authorization","Basic " ++ Auth}]},[],[]),
        {ok, {{RespondVersion,RespondCode, RespondState}, RespondHead, RespondBody}} = R,
        io:format("RespondVersion : ~p~n",[RespondVersion]),
        io:format("RespondCode : ~p~n",[RespondCode]),
        io:format("RespondState : ~p~n",[RespondState]),
        io:format("RespondHead : ~p~n",[RespondHead]),
        io:format("RespondBody : ~p~n",[RespondBody]),
        CidTail = string:find(RespondBody,"[\"",trailing),		
        io:format("CidTail : ~p~n",[CidTail]),
        Cid = string:slice(CidTail,2,61),
        io:format("Cid : ~p~n",[Cid]),
        Message2Send = Nickname ++ ":" ++ BodyContentStr,
        io:format("Message2Send : ~p~n",[Message2Send]),
        ToUserId =  binary_to_list(To#jid.user),
        io:format("ToUserId : ~p~n",[ToUserId]),
        Audience = "{\"alias\" : [ \"" ++ ToUserId ++ "\"]}",
        %%% Audience = "\"all\"",
        io:format("Audience : ~p~n",[Audience]),
        Post = [
          "{ \"cid\": \"", Cid , "\", \"platform\": \"all\", \"audience\": " , 
          Audience ,", \"notification\": {\"android\": {\"alert\": \"" , 
          Message2Send , "\",\"title\": \"you have a new message\",\"builder_id\": 1},\"ios\": {\"alert\": \"" , 
          Message2Send ,"\",\"sound\": \"default\",\"badge\": \"+1\"}}, \"options\": {\"time_to_live\": 60,\"apns_production\": false}}"],
        ?INFO_MSG("Sending post:~s", [ Post]),
        RP = httpc:request(post, 
        	{JpushUrl, [{"Authorization","Basic " ++ Auth}],
        	 "application/json",
        	list_to_binary(Post)},
        	[],
        	[{sync, true}]),
        {ok, {{PostRespondVersion,PostRespondCode, PostRespondState}, PostRespondHead, PostRespondBody}} = RP,
        io:format("PostRespondVersion : ~p~n",[PostRespondVersion]),
        io:format("PostRespondCode : ~p~n",[PostRespondCode]),
        io:format("PostRespondState : ~p~n",[PostRespondState]),
        io:format("PostRespondHead : ~p~n",[PostRespondHead]),
        io:format("PostRespondBody : ~p~n",[PostRespondBody]),
        Acc;
      (Type == normal) and (ServerPrefix /= nomatch) ->
      	Els = xmpp:get_els(Message),
        io:format("Els : ~p~n",[Els]),
      	[PsEvent|_] = Els,
        io:format("PsEvent : ~p~n",[PsEvent]),
        %%%Xml = fxml:element_to_binary(Message),
        %%%io:format("Xml : ~p~n",[Xml]),
        %%%BodyXml = fxml:get_subtag_cdata(Message, <<"body">>),
        %%%io:format("BodyXml : ~p~n",[BodyXml]),
      	{_, PsItems, _, _, _, _, _} = PsEvent,
        io:format("PsItems : ~p~n",[PsItems]),
      	{_, _, _, Items, _, _, _} = PsItems,
        io:format("Items : ~p~n",[Items]),
		[Item] = Items,
        io:format("Item : ~p~n",[Item]),
		{_, _, _, Xmlels, _, _} = Item,
        io:format("Xmlels : ~p~n",[Xmlels]),
		[Xmlel] = Xmlels,
        io:format("Xmlel : ~p~n",[Xmlel]),
		{_,_, _, MessageList} = Xmlel,
        io:format("MessageList : ~p~n",[MessageList]),
		[_,_, BodyXml] = MessageList,
        io:format("BodyXml : ~p~n",[BodyXml]),
		{_,_, _, BodyTexts} = BodyXml,
        io:format("BodyTexts : ~p~n",[BodyTexts]),
		[BodyText] = BodyTexts,
        io:format("BodyText : ~p~n",[BodyText]),
		{_, BodyContent} = BodyText,
		io:format("BodyContent ~p~n",[BodyContent]),
		BodyContentStr = binary_to_list(BodyContent),
		io:format("BodyContentStr : ~p~n",[BodyContentStr]),
    	AuthOrg = binary_to_list(AppKey) ++ ":" ++ binary_to_list(MasterSecret),
        io:format("Auth : ~p~n",[AuthOrg]),
    	Auth = base64:encode_to_string(AuthOrg),
        io:format("Auth : ~p~n",[Auth]),
        R = httpc:request(get,{JpushUrl4Cid, [{"Authorization","Basic " ++ Auth}]},[],[]),
        {ok, {{RespondVersion,RespondCode, RespondState}, RespondHead, RespondBody}} = R,
        io:format("RespondVersion : ~p~n",[RespondVersion]),
        io:format("RespondCode : ~p~n",[RespondCode]),
        io:format("RespondState : ~p~n",[RespondState]),
        io:format("RespondHead : ~p~n",[RespondHead]),
        io:format("RespondBody : ~p~n",[RespondBody]),
        CidTail = string:find(RespondBody,"[\"",trailing),		
        io:format("CidTail : ~p~n",[CidTail]),
        Cid = string:slice(CidTail,2,61),
        io:format("Cid : ~p~n",[Cid]),
        Message2Send = binary_to_list(From#jid.user) ++ ":" ++ BodyContentStr,
        io:format("Message2Send : ~p~n",[Message2Send]),
        Audience = "{\"alias\" : [ \"" ++ binary_to_list(To#jid.user) ++ "\"]}", 
        %%% TEST : Audience = "\"all\"",
        io:format("Audience : ~p~n",[Audience]),
        Post = [
          "{ \"cid\": \"", Cid , "\", \"platform\": \"all\", \"audience\": " , 
          Audience ,", \"notification\": {\"android\": {\"alert\": \"" , 
          Message2Send , "\",\"title\": \"you have a new message\",\"builder_id\": 1},\"ios\": {\"alert\": \"" , 
          Message2Send ,"\",\"sound\": \"default\",\"badge\": \"+1\"}}, \"options\": {\"time_to_live\": 60,\"apns_production\": false}}"],
        ?INFO_MSG("Sending post:~s", [ Post]),
        RP = httpc:request(post, 
        	{JpushUrl, [{"Authorization","Basic " ++ Auth}],
        	 "application/json",
        	list_to_binary(Post)},
        	[],
        	[{sync, true}]),
        {ok, {{PostRespondVersion,PostRespondCode, PostRespondState}, PostRespondHead, PostRespondBody}} = RP,
        io:format("PostRespondVersion : ~p~n",[PostRespondVersion]),
        io:format("PostRespondCode : ~p~n",[PostRespondCode]),
        io:format("PostRespondState : ~p~n",[PostRespondState]),
        io:format("PostRespondHead : ~p~n",[PostRespondHead]),
        io:format("PostRespondBody : ~p~n",[PostRespondBody]),
      	Acc;
      true ->
        Acc
    end.

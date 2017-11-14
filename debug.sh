git pull
./build.sh
cp ebin/*.beam /usr/local/lib/ejabberd-17.07/ebin/
ejabberdctl stop
ejabberdctl live

-module(hello_tele).
-export([hello_world/0]).

hello_world() ->
    io:format(<<"~s~n"/utf8>>, [<<"Hello, World!"/utf8>>]).


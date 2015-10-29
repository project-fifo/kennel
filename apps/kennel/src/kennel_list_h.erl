-module(kennel_list_h).

-export([init/2]).

init(Req, State) ->
    {ok, Context} = kennel:context(Req),
    io:format("ctx: ~p~n", [Context]),
    Req2 = cowboy_req:reply(
             200, [
                   {<<"content-type">>, <<"application/json">>}
                  ], <<"[]">>, Req),
    {ok, Req2, State}.

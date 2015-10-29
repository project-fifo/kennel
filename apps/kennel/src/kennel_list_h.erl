-module(kennel_list_h).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Context} = kennel:context(Req),
    io:format("ctx: ~p~n", [Context]),
    {ok, Req2} = cowboy_req:reply(200, [
                                        {<<"content-type">>, <<"text/plain">>}
                                       ], <<"[]">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

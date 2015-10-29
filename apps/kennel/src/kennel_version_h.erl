-module(kennel_version_h).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Context} = kennel:context(Req),
    io:format("ctx: ~p~n", [Context]),
    R = [
         {'Version',        <<"1.5.0">>},
         {'Os',             uname("-s")},
         {'KernelVersion',  uname("-r")},
         {'GoVersion',      version()},
         {'GitCommit',      <<"">>},
         {'Arch',           uname("-i")},
         {'ApiVersion',     kennel:api_version()},
         {'Experimental',   true}
        ],
    {ok, Req2} = cowboy_req:reply(200, [
                                        {<<"content-type">>, <<"text/plain">>}
                                       ], jsx:encode(R), Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

uname(Args) ->
    list_to_binary(lib:nonl(os:cmd("uname " ++ Args))).

version() ->
    list_to_binary("erts-" ++ erlang:system_info(version)).

-module(kennel_version_h).

-export([init/2]).


init(Req, Opts) ->
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
    Req2 = cowboy_req:reply(
             200, [
                   {<<"content-type">>, <<"application/json">>}
                  ], jsx:encode(R), Req),
    {ok, Req2, Opts}.

uname(Args) ->
    list_to_binary(lib:nonl(os:cmd("uname " ++ Args))).

version() ->
    list_to_binary("erts-" ++ erlang:system_info(version)).

-module(kennel_version_h).

-export([get/2]).

get(Req, State) ->
    Res = [
         {'Version',        <<"1.5.0">>},
         {'Os',             uname("-s")},
         {'KernelVersion',  uname("-r")},
         {'GoVersion',      version()},
         {'GitCommit',      <<"">>},
         {'Arch',           uname("-i")},
         {'ApiVersion',     kennel:api_version()},
         {'Experimental',   true}
        ],
    {ok, Res, Req, State}.

uname(Args) ->
    list_to_binary(lib:nonl(os:cmd("uname " ++ Args))).

version() ->
    list_to_binary("erts-" ++ erlang:system_info(version)).

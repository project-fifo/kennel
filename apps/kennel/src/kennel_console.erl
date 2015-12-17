%% @doc Interface for howl-admin commands.
-module(kennel_console).
-export([connections/1, get_nets/1, set_public/1, set_private/1]).
-ignore_xref([connections/1, get_nets/1, set_public/1, set_private/1]).


connections(["snarl"]) ->
    io:format("Snarl endpoints.~n"),
    print_endpoints(libsnarl:servers());

connections(["sniffle"]) ->
    io:format("Snarl endpoints.~n"),
    print_endpoints(libsniffle:servers());

connections([]) ->
    connections(["snarl"]),
    connections(["sniffle"]).

set_public([OrgS, NetS]) ->
    Org = list_to_binary(OrgS),
    Net = list_to_binary(NetS),
    K = [<<"fifo">>, <<"docker">>, <<"networks">>, <<"public">>],
    ls_org:set_metadata(Org, [{K, Net}]).

set_private([OrgS, NetS]) ->
    Org = list_to_binary(OrgS),
    Net = list_to_binary(NetS),
    K = [<<"fifo">>, <<"docker">>, <<"networks">>, <<"private">>],
    ls_org:set_metadata(Org, [{K, Net}]).

get_nets([OrgS]) ->
    case ls_org:get(list_to_binary(OrgS)) of
        {ok, Org} ->
            M = ft_org:metadata(Org),
            io:format("Private: ~s~n", [get_scope(M, <<"private">>)]),
            io:format("Public:  ~s~n", [get_scope(M, <<"public">>)]),
            ok;
        _ ->
            io:format("Could not find org: ~s", [OrgS]),
            error
    end.

get_scope(M, Scope) ->
    jsxd:get([<<"fifo">>, <<"docker">>, <<"networks">>, Scope],
             <<"not set">>, M).

print_endpoints(Es) ->
    io:format("Hostname            "
              "                    "
              " Node               "
              " Errors    ~n"),
    io:format("--------------------"
              "----------"
              " --------------------"
              " ---------------~n", []),
    [print_endpoint(E) || E <- Es].

print_endpoint({{Hostname, [{port, Port}, {ip, IP}]}, _, Fails}) ->
    HostPort = <<IP/binary, ":", Port/binary>>,
    io:format("~30s ~-24s ~9b~n", [Hostname, HostPort, Fails]).

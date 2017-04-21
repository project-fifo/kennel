-module(kennel_create_h).
-behaviour(kennel_h).

-export([permission/1, post/2]).

permission(_) ->
    [<<"cloud">>, <<"docker">>, <<"create">>].

post(Req, #{user := User} = State) ->
    lager:warning("[TODO] The ID should be generated in sniffle?"),
    ID = kennel:docker_id(),
    {ok, Body, Req1} = cowboy_req:body(Req),
    lager:info("[create]: ~s~n", [Body]),
    #{
       <<"OpenStdin">>  := OpenStdin,
       <<"Tty">>        := Tty,
       <<"Image">>      := Image,
       <<"Hostname">>   := Hostname,
       <<"HostConfig">> := HostConfig
     } = JSON = jsone:decode(Body),

    {ok, Package} = find_package(HostConfig),
    Alias = Hostname,
    {ok, U} = ls_user:get(User),
    lager:warning("[TODO] How to handle firewall rules here"),
    Networks0 = build_network(U, HostConfig),
    Networks = maps:from_list(Networks),
    %% We should let a user define what host/bridge and use -p/P to decide if
    %% we want private only or public
    %% this is just stupid but we need to configm with vmadm ...
    Docker0 = mk_docker(JSON),
    Docker = lists:sort([{<<"id">>, ID},
                         {<<"open_stdin">>, OpenStdin},
                         {<<"tty">>, Tty}
                         | Docker0]),
    Config = [
              %%{<<"autoboot">>, false},
              {<<"owner">>, User},
              {<<"alias">>, Alias},
              {<<"docker">>, Docker},
              {<<"networks">>, Networks}
             ],
    Warnings = case ls_vm:create(Package, {docker, Image}, Config) of
                   {ok, UUID} ->

                       [<<"This VM will get the UUID: ", UUID/binary>>];
                   E ->
                       lager:error("Docker creation failed: ~p", [E]),
                       [<<"This blew up in your face.">>]
               end,
    Res = #{
      <<"Id">> => ID,
      <<"Warnings">> => Warnings
     },
    {ok, Res, Req1, State}.

find_package(#{
           <<"CpuQuota">>   := CpuCap,
           <<"Memory">>     := Ram
          }) ->
    %% The goal is to find the smalrest fitting package
    Rules = [
             %% We only look at packages that have at least
             %% the requested ammount of memory and cpu
             {must, '>=', <<"cpu_cap">>, CpuCap},
             %% Ram comes in bytes but packages use megabyte so
             %% we need to convert that.
             {must, '>=', <<"ram">>, Ram div (1024 * 1024)},
             %% Memory is the driving factor by ranking
             %% based on memory the packages with
             %% the least memory will be returned 'first'
             {scale, <<"ram">>, 0, 10000},
             %% We use CPU as a tie-breaker in case we have multiple
             %% pacakges with the same ram
             {scale, <<"cpu_cap">>, 0, 100}
            ],
    {ok, Res} = ls_package:list(Rules, false),
    case Res of
        [{_, Pkg} | _] ->
            {ok, Pkg};
        _ ->
            {error, no_pkg}
    end.

build_network(User, C) ->
    OrgID = ft_user:active_org(User),
    {ok, Org} = ls_org:get(OrgID),
    build_network1(Org, C).

build_network1(Org, #{<<"PublishAllPorts">> := true}) ->
    both(Org);

build_network1(Org, #{<<"PortBindings">> := #{}}) ->
    net(<<"net0">>, Org, <<"private">>);

build_network1(Org, #{<<"PortBindings">> := _Ports}) ->
    both(Org).

both(Org) ->
    net(<<"net0">>, Org, <<"public">>) ++ net(<<"net1">>, Org, <<"private">>).

%% Gets network mappings for the org.
-spec net(binary(), ft_org:org(), binary()) ->
                 [{binary(), binary()}].

net(NIC, Org, Scope) ->
    M = ft_org:metadata(Org),
    case jsxd:get([<<"fifo">>, <<"docker">>, <<"networks">>, Scope], M) of
        {ok, N}  ->
            [{NIC, N}];
        _ ->
            []
    end.

mk_docker(#{<<"Entrypoint">> := Entrypoint,
            <<"User">>       := User,
            <<"WorkingDir">> := WorkingDir,
            <<"Cmd">>        := Cmd,
            <<"Env">>        := Env}) ->
    case Env of
        [] ->
            [];
        _ ->
            [{<<"env">>, jsone:encode(Env)}]
    end
        ++     case Cmd of
                   null ->
                       [];
                   _ ->
                       [{<<"cmd">>, jsone:encode(Cmd)}]
               end

        ++ v(<<"entrypoint">>, Entrypoint)
        ++ v(<<"user">>, User)
        ++ v(<<"workdir">>, WorkingDir).

v(_, null) ->
    [];
v(_, <<>>) ->
    [];
v(K, V) ->
    [{K, V}].

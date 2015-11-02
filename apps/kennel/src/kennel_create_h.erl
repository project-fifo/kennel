-module(kennel_create_h).

-export([post/2]).

post(Req, #{ctx := Context} = State) ->
    lager:warning("[TODO] The ID should be generated in sniffle?"),
    ID = kennel:docker_id(),
    {ok, Body, Req1} = cowboy_req:body(Req),
    #{
       <<"Image">> := Image,
       <<"Cmd">> := Cmd,
       <<"Hostname">> := Hostname,
       <<"HostConfig">> := #{
         <<"CpuQuota">> := CpuCap,
         <<"Memory">> := Ram
        }
     } = jsone:decode(Body),

    {ok, Package} = find_package(Ram, CpuCap),
    {ok, Owner} = jsxd:get([<<"resource_owner">>], Context),
    Alias = Hostname,
    lager:warning("[TODO] Need to figure out the network ID, but HOW?!?!"),
    Network = <<"fd060fd4-b5cd-4fef-911b-a5023d8b3bb4">>,
    %% this is just stupid but we need to configm with vmadm ...
    CmdStr = jsone:encode(Cmd),
    Config = [
              {<<"owner">>, Owner},
              {<<"alias">>, Alias},
              {<<"docker">>,
               [{<<"cmd">>, CmdStr},
                {<<"id">>, ID}]},
              {<<"networks">>, [{<<"net0">>, Network}]}
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

find_package(_Ram, _CpuCap) ->
    lager:warning("[TODO] Need to actually look up a package"),
    {ok, <<"42bf38f3-5632-49cb-82ce-973a04c3a8f6">>}.

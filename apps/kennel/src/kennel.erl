-module(kennel).

-export([api_version/0, context/1, docker_id/0, to_docker/1]).
%% This is horrible :/ but we need the socket

context(Req) ->
    Cert = cowboy_req:cert(Req),
    Fingerprint = crypto:hash(sha, Cert),
    ls_oauth:verify_access_token(Fingerprint).

api_version() ->
    <<"1.20">>.

docker_id() ->
    base16:encode(crypto:strong_rand_bytes(6)).

to_docker(VM) ->
    case ft_vm:vm_type(VM) of
        docker ->
            Docker = ft_vm:docker(VM),
            [Command | _] = case jsxd:get([<<"cmd">>], Docker) of
                                {ok, <<"null">>} ->
                                    [<<"">>];
                                {ok, Commands} ->
                                    jsone:decode(Commands);
                                _ ->
                                    [<<"<image>">>]
                            end,
            {ok, ID} = jsxd:get([<<"id">>], Docker),
            Config = ft_vm:config(VM),
            lager:warning("[TODO] WTF are names?"),
            Names1 = case ft_vm:alias(VM) of
                        <<>> -> [];
                        A -> [A]
                    end,
            Names = case jsxd:get([<<"hostname">>], Config) of
                        {ok, <<>>} ->
                            Names1;
                        {ok, HN} ->
                            [HN | Names1];
                        _ ->
                            Names1
                    end,
            lager:warning("[TODO] What the hell do exit statuses mean?"),
            Status = case ft_vm:state(VM) of
                         failed ->
                             <<"Exit 1">>;
                         _ ->
                             <<"Exit 0">>
                     end,
            lager:warning("[TODO] How to get teh follwing VM values?"),
            Ports = [],
            Labels = #{},
            SizeRw = 1,
            SizeRootFs = jsxd:get([<<"quota">>], 0, Config),
            #{
               <<"Id">>         => ID,
               <<"Names">>      => Names,
               <<"Image">>      => ft_vm:dataset(VM),
               <<"Command">>    => Command,
               <<"Created">>    => ft_vm:created_at(VM),
               <<"Status">>     => Status,
               <<"Ports">>      => Ports,
               <<"Labels">>     => Labels,
               <<"SizeRw">>     => SizeRw,
               <<"SizeRootFs">> => SizeRootFs

           };
        _ ->
            no_docker
    end.

-module(kennel_get_h).

-export([get/2]).

get(Req, State = #{vm := VM}) ->
    #{<<"Created">> := T} = R1 = kennel:to_docker(VM),
    R2 = R1#{
           <<"Path">> => <<"/bin/sh">>,
           <<"ProcessLabel">> => <<"">>,
           %% This is just unbelivable, the get request enforces a string for
           %% time while the list request enforces a second timestamp, how does
           %% this even work?!?
           <<"Created">> => list_to_binary(qdate:to_string("c.0\\Z", "GMT", T)),
           <<"State">> => vm_state(VM),
           <<"Config">> => vm_config(VM),
           <<"Hostname">> => <<"ba033ac44011">>
          },
    {ok, R2, Req, State}.


vm_state(VM) ->
    State = ft_vm:state(VM),
    Running = (State == <<"running">>),
    Paused = (State == <<"stopped">>),
    lager:warning("[TODO] Fill out rest of the VM state"),
    #{
       <<"Running">> => Running,
       <<"Paused">> => Paused,
       <<"Restarting">> => not (Running or Paused)
     }.

vm_config(VM) ->
    Docker = ft_vm:docker(VM),
    lager:warning("[TODO] Fill out rest of the VM config"),
    #{
       <<"Tty">> => jsxd:get([<<"tty">>], false, Docker)
     }.

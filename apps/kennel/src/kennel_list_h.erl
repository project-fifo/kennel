-module(kennel_list_h).

-export([get/2]).

get(Req, State) ->
    lager:warning("[TODO] Query smarter?"),
    Conditions = [],
    {ok, VMs} = ls_vm:list(Conditions, true),
    VMs1 = [kennel:to_docker(VM) || {_, VM} <- VMs],
    Res = [VM || VM <- VMs1, VM =/= no_docker],
    io:format("Res: ~p~n", [Res]),
    {ok, Res, Req, State}.

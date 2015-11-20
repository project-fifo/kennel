-module(kennel_list_h).
-behaviour(kennel_h).

-export([permission/1, get/2]).

permission(_) ->
    [<<"cloud">>, <<"vms">>, <<"list">>].

get(Req, State = #{user := User}) ->
    {ok, Permissions} = ls_user:cache(User),
    Conditions = [{must, '=:=', <<"vm_type">>, docker},
                  {must, 'allowed',
                   [<<"vms">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                   Permissions}],
    {ok, VMs} = ls_vm:list(Conditions, true),
    Res = [kennel:to_docker(VM) || {_, VM} <- VMs],
    {ok, Res, Req, State}.

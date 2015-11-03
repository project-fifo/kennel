-module(kennel_delete_h).

-export([delete/2, permission/1]).

permission(#{uuid := VM}) ->
    [<<"vms">>, VM, <<"delete">>].

delete(Req, State = #{uuid := VM}) ->
    ok = ls_vm:delete(VM),
    {no_content, Req, State}.

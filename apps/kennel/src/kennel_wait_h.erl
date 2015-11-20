-module(kennel_wait_h).
-behaviour(kennel_h).

-export([permission/1, post/2]).

permission(#{uuid := VM}) ->
    [<<"vms">>, VM, <<"get">>].

post(Req, State = #{uuid := UUID} ) ->
    {ok, VM} = ls_vm:get(UUID),
    case ft_vm:creating(VM) of
        true ->
            timer:sleep(1000),
            post(Req, State);
        _ ->
            {ok, #{<<"StatusCode">> => 0}, Req, State}
    end.

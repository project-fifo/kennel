-module(kennel_wait_h).

-export([permission/1, post/2]).

permission(#{uuid := VM}) ->
    [<<"vms">>, VM, <<"get">>].

post(Req, State = #{uuid := UUID} ) ->
    {ok, VM} = ls_vm:get(UUID),
    Creatig = ft_vm:creating(VM),
    if
         Creatig == true ->
            timer:sleep(1000),
            post(Req, State);
        true ->
            {ok, #{<<"StatusCode">> => 0}, Req, State}
    end.

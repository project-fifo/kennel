-module(kennel_wait_h).

-export([post/2]).

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

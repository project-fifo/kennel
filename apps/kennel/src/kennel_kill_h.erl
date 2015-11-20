-module(kennel_kill_h).
-behaviour(kennel_h).

-export([permission/1, post/2]).

permission(#{uuid := VM}) ->
    [<<"vms">>, VM, <<"stop">>].

post(Req, State = #{uuid := UUID}) ->
    lager:warning("[TODO] How to handle signal?"),
    ok = ls_vm:stop(UUID, [force]),
    Req1 = cowboy_req:reply(204, [], <<>>, Req),
    {ok, Req1, State}.

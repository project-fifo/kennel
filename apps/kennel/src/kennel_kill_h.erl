-module(kennel_kill_h).

-export([post/2]).

post(Req, State = #{uuid := UUID}) ->
    lager:warning("[TODO] How to handle signal?"),
    ok = ls_vm:stop(UUID, true),
    Req1 = cowboy_req:reply(204, [], <<>>, Req),
    {ok, Req1, State}.

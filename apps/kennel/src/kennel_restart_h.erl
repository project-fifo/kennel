-module(kennel_restart_h).

-export([post/2]).

post(Req, State = #{uuid := UUID}) ->
    Fields = [{t, int}],
    case cowboy_req:match_qs(Fields, Req) of
        #{t := _T} ->
            lager:warning("[TODO] implement force after after.");
        _ ->
            ok
    end,
    ok = ls_vm:reboot(UUID),
    Req1 = cowboy_req:reply(204, [], <<>>, Req),
    {ok, Req1, State}.

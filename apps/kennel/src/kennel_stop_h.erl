-module(kennel_stop_h).

-export([post/2]).

post(Req, State = #{uuid := UUID}) ->
    Fields = [{t, int}],
    case cowboy_req:match_qs(Fields, Req) of
        #{t := _T} ->
            lager:warning("[TODO] implement kill after");
        _ ->
            ok
    end,
    ok = ls_vm:stop(UUID),
    Req1 = cowboy_req:reply(204, [], <<>>, Req),
    {ok, Req1, State}.

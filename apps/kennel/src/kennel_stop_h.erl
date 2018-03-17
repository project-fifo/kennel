-module(kennel_stop_h).
-behaviour(kennel_h).

-export([permission/1, post/2]).

permission(#{uuid := VM}) ->
    [<<"vms">>, VM, <<"stop">>].

post(Req, State = #{uuid := UUID}) ->
    Fields = [{t, int}],
    case cowboy_req:match_qs(Fields, Req) of
        #{t := _T} ->
            lager:warning("[TODO] implement kill after");
        _ ->
            ok
    end,
    ok = ls_vm:stop(UUID),
    Req1 = cowboy_req:reply(204, #{}, <<>>, Req),
    {ok, Req1, State}.

-module(kennel_start_h).

-export([permission/1, post/2]).

permission(#{uuid := VM}) ->
    [<<"vms">>, VM, <<"start">>].

post(Req, State = #{uuid := UUID}) ->
    ok = loop(UUID),
    Req1 = cowboy_req:reply(204, [], <<>>, Req),
    {ok, Req1, State}.

loop(UUID)  ->
    case ls_vm:start(UUID) of
        ok ->
            ok;
        {error, creating} ->
            timer:sleep(1000),
            loop(UUID);
        {error, not_deployed} ->
            timer:sleep(1000),
            loop(UUID);
        E ->
            E
    end.

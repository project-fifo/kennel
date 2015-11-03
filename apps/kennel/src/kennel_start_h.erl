-module(kennel_start_h).

-export([post/2]).

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
        E ->
            E
    end.

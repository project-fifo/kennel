-module(kennel_rename_h).
-behaviour(kennel_h).

-export([permission/1, post/2]).

permission(#{uuid := VM}) ->
    [<<"vms">>, VM, <<"edit">>].

post(Req, State) ->
    Req1 = cowboy_req:reply(204, #{}, <<>>, Req),
    {ok, Req1, State}.

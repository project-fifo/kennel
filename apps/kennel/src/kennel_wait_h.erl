-module(kennel_wait_h).

-export([post/2]).

post(Req, State) ->
    cowboy_req:bindings(
    {ok, #{<<"StatusCode">> => 0}, Req, State}.

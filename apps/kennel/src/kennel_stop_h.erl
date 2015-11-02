-module(kennel_stop_h).

-export([post/2]).

post(Req, State) ->
    Req1 = cowboy_req:reply(204, [], <<>>, Req),
    {ok, Req1, State}.

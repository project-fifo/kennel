-module(kennel_list_h).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).


%% This is horrible :/ but we need the socket
-record(http_req, {
          %% Transport.
          socket = undefined :: any(),
          transport = undefined :: undefined | module(),
          connection = keepalive :: keepalive | close,

          %% Request.
          pid = undefined :: pid(),
          method = <<"GET">> :: binary(),
          version = 'HTTP/1.1' :: cowboy:http_version(),
          peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
          host = undefined :: undefined | binary(),
          host_info = undefined :: undefined | cowboy_router:tokens(),
          port = undefined :: undefined | inet:port_number(),
          path = undefined :: binary(),
          path_info = undefined :: undefined | cowboy_router:tokens(),
          qs = undefined :: binary(),
          qs_vals = undefined :: undefined | list({binary(), binary() | true}),
          bindings = undefined :: undefined | cowboy_router:bindings(),
          headers = [] :: cowboy:http_headers(),
          p_headers = [] :: [any()],
          cookies = undefined :: undefined | [{binary(), binary()}],
          meta = [] :: [{atom(), any()}],

          %% Request body.
          body_state,
          buffer = <<>> :: binary(),
          multipart = undefined :: undefined | {binary(), binary()},

          %% Response.
          resp_compress = false :: boolean(),
          resp_state = waiting :: locked | waiting | waiting_stream
                                | chunks | stream | done,
          resp_headers = [] :: cowboy:http_headers(),
          resp_body,

          %% Functions.
          onresponse = undefined :: undefined | already_called
                                  | cowboy:onresponse_fun()
         }).

-record(state, {
          socket = undefined :: any()
         }).

init(_Transport, Req = #http_req{socket = Socket}, []) ->
    {ok, Req, #state{socket = Socket}}.

handle(Req, State = #state{socket = Socket}) ->
    case ssl:peercert(Socket) of
        {error, no_peercert} ->
            io:format("init: no_peercert~n"),
            false;
        {ok, Cert} ->
            Fingerprint = crypto:hash(sha, Cert),
            io:format("fingerprint: ~p~n", [Fingerprint])
    end,
    {ok, Req2} = cowboy_req:reply(200, [
                                        {<<"content-type">>, <<"text/plain">>}
                                       ], <<"[]">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

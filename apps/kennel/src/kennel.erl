-module(kennel).

-export([api_version/0, context/1]).
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


context(#http_req{socket = Socket}) ->
    case ssl:peercert(Socket) of
        {error, E} ->
                {error, E};
        {ok, Cert} ->
            Fingerprint = crypto:hash(sha, Cert),
            ls_oauth:verify_access_token(Fingerprint)
    end.


api_version() ->
    <<"1.20">>.

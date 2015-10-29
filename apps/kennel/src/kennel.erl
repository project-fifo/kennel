-module(kennel).

-export([api_version/0, context/1]).
%% This is horrible :/ but we need the socket

-record(http_req, {
          %% Transport.
          socket = undefined,
          transport = undefined,
          connection = keepalive,

          %% Request.
          pid = undefined,
          method = <<"GET">>,
          version = 'HTTP/1.1',
          peer = undefined,
          host = undefined,
          host_info = undefined,
          port = undefined,
          path = undefined,
          path_info = undefined,
          qs = undefined,
          bindings = undefined,
          headers = [],
          meta = [],

          %% Request body.
          body_state = waiting,
          buffer = <<>>,
          multipart = undefined,

          %% Response.
          resp_compress = false,
          resp_state = waiting,
          resp_headers = [],
          resp_body = <<>>,

          %% Functions.
          onresponse = undefined
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

%%%-------------------------------------------------------------------
%% @doc kennel public API
%% @end
%%%-------------------------------------------------------------------

-module('kennel_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Acceptors} = application:get_env(kennel, acceptors),
    {ok, SSLPort} = application:get_env(kennel, ssl_port),
    {ok, SSLCA} = application:get_env(kennel, ssl_cacertfile),
    {ok, SSLCert} = application:get_env(kennel, ssl_certfile),
    {ok, SSLKey} = application:get_env(kennel, ssl_keyfile),
    V = "/v" ++ binary_to_list(kennel:api_version()),
    DPRules = [
               {V ++ "/containers/json", kennel_list_h, []},
               {V ++ "/version", kennel_version_h, []}
              ],
    Dispatch = cowboy_router:compile([{'_', DPRules}]),
    {ok, _} = cowboy:start_https(https, Acceptors,
                                 [{port, SSLPort},
                                  {cacertfile, SSLCA},
                                  {certfile, SSLCert},
                                  {keyfile, SSLKey}
                                 %%, {fail_if_no_peer_cert, true}
                                 , {verify, verify_peer}
                                 ],
                                 [{env, [{dispatch, Dispatch}]}]),
    'kennel_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================



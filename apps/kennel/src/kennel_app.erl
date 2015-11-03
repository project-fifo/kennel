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
               {V ++ "/containers/json", kennel_h, #{handler => kennel_list_h}},
               {V ++ "/containers/create", kennel_h, #{handler => kennel_create_h}},
               {V ++ "/containers/:id", kennel_h, #{handler => kennel_delete_h}},
               {V ++ "/containers/:id/json", kennel_h, #{handler => kennel_get_h}},
               {V ++ "/containers/:id/start", kennel_h, #{handler => kennel_start_h}},
               {V ++ "/containers/:id/stop", kennel_h, #{handler => kennel_stop_h}},
               {V ++ "/containers/:id/restart", kennel_h, #{handler => kennel_restart_h}},
               {V ++ "/containers/:id/kill", kennel_h, #{handler => kennel_kill_h}},
               {V ++ "/containers/:id/pause", kennel_h, #{handler => kennel_pause_h}},
               {V ++ "/containers/:id/rename", kennel_h, #{handler => kennel_rename_h}},

               {V ++ "/containers/:id/attach", kennel_h, #{handler => kennel_attach_h}},

               {V ++ "/containers/:id/wait", kennel_h, #{handler => kennel_wait_h}},

               {V ++ "/version", kennel_h, #{handler => kennel_version_h}}
              ],
    Handlers =
        [kennel_list_h, kennel_create_h, kennel_version_h, kennel_attach_h,
         kennel_get_h, kennel_start_h, kennel_stop_h, kennel_restart_h,
         kennel_kill_h, kennel_rename_h, kennel_pause_h, kennel_wait_h,
         kennel_delete_h],
    [H:module_info() || H <- Handlers],
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


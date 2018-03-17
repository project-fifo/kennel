%%%-------------------------------------------------------------------
%% @doc kennel public API
%% @end
%%%-------------------------------------------------------------------
-module('kennel_app').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

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
    C = V ++ "/containers",
    DPRules =
        [
         {C ++ "/json", kennel_h, #{handler => kennel_list_h}},
         {C ++ "/create", kennel_h, #{handler => kennel_create_h}},
         {C ++ "/:id", kennel_h, #{handler => kennel_delete_h}},
         {C ++ "/:id/json", kennel_h, #{handler => kennel_get_h}},
         {C ++ "/:id/start", kennel_h, #{handler => kennel_start_h}},
         {C ++ "/:id/stop", kennel_h, #{handler => kennel_stop_h}},
         {C ++ "/:id/restart", kennel_h, #{handler => kennel_restart_h}},
         {C ++ "/:id/kill", kennel_h, #{handler => kennel_kill_h}},
         {C ++ "/:id/pause", kennel_h, #{handler => kennel_pause_h}},
         {C ++ "/:id/rename", kennel_h, #{handler => kennel_rename_h}},

         {C ++ "/:id/attach", kennel_h, #{handler => kennel_attach_h}},

         {C ++ "/:id/wait", kennel_h, #{handler => kennel_wait_h}},

         {V ++ "/version", kennel_h, #{handler => kennel_version_h}}
        ],
    Handlers =
        [kennel_list_h, kennel_create_h, kennel_version_h, kennel_attach_h,
         kennel_get_h, kennel_start_h, kennel_stop_h, kennel_restart_h,
         kennel_kill_h, kennel_rename_h, kennel_pause_h, kennel_wait_h,
         kennel_delete_h],
    [H:module_info() || H <- Handlers],
    Dispatch = cowboy_router:compile([{'_', DPRules}]),
    Opts = [{port, SSLPort},
            {cacertfile, SSLCA},
            {certfile, SSLCert},
            {keyfile, SSLKey}
            %%, {fail_if_no_peer_cert, true}
           , {verify, verify_peer}
           ],
    Opts1 = case application:get_env(kennel, ssl_depth) of
                {ok, Depth}
                  when is_integer(Depth),
                       Depth >= 0,
                       Depth =< 255 ->
                    [ {depth, Depth} | Opts];
                _ ->
                    Opts
            end,
    {ok, _} = cowboy:start_tls(https,
                               Opts1,
                               #{env => #{dispatch => Dispatch}}),
    'kennel_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

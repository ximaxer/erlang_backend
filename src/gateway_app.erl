-module(gateway_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    {ok, HttpPort} = application:get_env(gateway, http_port),
    %cache:create_table(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/",test_handler, []},
            {"/join_game", join_game_handler, []},
            {"/leave_game", leave_game_handler, []},
            {"/disconnect", disconnect_player_handler, []},
            {"/create_game", create_game_handler, []},
            {"/broadcast", broadcast_handler, []},
            {"/broadcast_without_log", broadcast_no_log_handler, []},
            {"/get_logs", logs_handler, []},
            {"/start_game", start_game_handler, []},
            {"/change_profile", change_user_profile_handler, []},
            {"/heartbeat", player_heartbeat_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(http,[{port, HttpPort}], #{env => #{dispatch => Dispatch}}),

    gateway_sup:start_link().

stop(_State) ->
    ok.



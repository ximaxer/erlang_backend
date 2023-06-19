-module(gameSession).
-import(string,[concat/2]).
-compile(export_all).

start(ServerName) ->
    case sessionUtils:start(ServerName,{gameSession,session_handler,[maps:new(),dict:new(),ServerName, 0,[],[],"false"]}) of
        ok -> 
            io:format("Game already ocurring"),
            0;
        _ -> 
            MonitorName = list_to_atom(lists:concat([ServerName,"_hearbeat_monitor"])),
            MonitorPid = sessionUtils:start(MonitorName,{gameSession,heartbeat_Monitor,[ServerName]}),
            link(MonitorPid),
            io:format("~p~n",[global:registered_names()]),
            1
    end.

stop(ServerName) ->
    sessionUtils:stop(ServerName).

disconnectPlayer(PlayerName,Players)->
    case dict:find(PlayerName, Players) of
        {ok, ["online"]} ->
            dict:update(PlayerName, fun(_) -> ["offline"] end, Players);
        {ok, ["waiting"]} ->
            dict:update(PlayerName, fun(_) -> ["offline"] end, Players);
        _ ->
            Players
    end.


session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing) ->
    {ok, MaxPlayers} = application:get_env(gateway, max_players_per_game),
    receive
        {register_player, PlayerName, ClientAvatar, ClientNickname, HandlerId} ->
            PlayerNumber = dict:size(Players),
            if (PlayerNumber>=MaxPlayers) -> 
                io:format("Max Number of Players is ~p!~n",[MaxPlayers]),
                io:format("~p~n",[Players]),
                HandlerId ! {error},
                session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing);
            true ->
                UpdatedDict= dict:store(PlayerName,["online"],Players),
                UpdatedPlayerDetails= maps:put(PlayerName,{ClientNickname,ClientAvatar},PlayerDetails),
                io:format("Host: ~p~n",[HostId]),
                io:format("~p~n",[dict:to_list(UpdatedDict)]),
                Message_to_publish = lists:concat(["200|",PlayerName,"|",ClientNickname,"|", ClientAvatar]),
                mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                HandlerId ! {connected, UpdatedPlayerDetails, HostId},
                _hostIdSize = string:len(HostId),
                if (_hostIdSize == 0) -> 
                    session_handler(UpdatedPlayerDetails, UpdatedDict, SessionId, PlayersOnline+1, utils:changeHost(PlayerName,UpdatedDict), GameState, OnGoing);
                true -> 
                    session_handler(UpdatedPlayerDetails, UpdatedDict, SessionId, PlayersOnline+1, HostId, GameState, OnGoing)
                end
            end;
        {unregister_player, PlayerName}->
            case dict:find(PlayerName, Players) of
                {ok, _} ->
                    if(PlayersOnline-1 < 1) ->
                        stop(SessionId);
                    true ->
                        BooleanValue = string:equal(PlayerName, HostId),
                        if BooleanValue ->
                            UpdatedPlayers = disconnectPlayer(PlayerName, Players),
                            NewHost = utils:changeHost(PlayerName,UpdatedPlayers),
                            Message_to_publish = lists:concat(["206|",PlayerName,"|",NewHost]),
                            mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                            session_handler(maps:remove(PlayerName, PlayerDetails), dict:erase(PlayerName, UpdatedPlayers),SessionId, PlayersOnline-1, NewHost, GameState, OnGoing);
                        true->
                            Message_to_publish = lists:concat(["201|",PlayerName]),
                            mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                            session_handler(maps:remove(PlayerName, PlayerDetails), dict:erase(PlayerName, Players),SessionId, PlayersOnline-1, HostId, GameState, OnGoing)
                        end
                    end;
                error ->
                    io:format("Error! Unknown player: ~p~n", [PlayerName]),
                    session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing)
            end;
        {connect_player, PlayerName} ->
            % lacking implementation
            % action code 202
            case dict:find(PlayerName,Players) of
                {ok,_} ->
                    Message_to_publish = lists:concat(["202|",PlayerName]),
                    mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                    session_handler(PlayerDetails, dict:update(PlayerName, fun(["offline"]) -> ["online"] end, Players),SessionId, PlayersOnline+1, HostId, GameState, OnGoing);
                error ->
                    io:format("Error! Unknown player: ~p~n", [PlayerName]),
                    session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing)
            end;
        {player_heartbeat, PlayerName, CurrentGameState, HeartBeatHandlerPid} ->
            case dict:find(PlayerName,Players) of
                {ok,_} ->
                    HeartBeatHandlerPid ! {success},
                    session_handler(PlayerDetails, dict:update(PlayerName, fun(_) -> ["online"] end, Players),SessionId, PlayersOnline, HostId, CurrentGameState, OnGoing);
                error ->
                    io:format("Error! Unknown player: ~p~n", [PlayerName]),
                    HeartBeatHandlerPid ! {failure},
                    session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing)
            end;
        {heartbeat_loop} ->
            [NewHost,OnlinePlayers] = utils:find_number_of_online_players(SessionId,Players,HostId,OnGoing),
            if(OnlinePlayers == 0) -> 
                _ = dict:map(fun(_, Value) ->
                        case Value of
                            ["online"] ->
                                ["waiting"];
                            ["waiting"] ->
                                ["offline"];
                            _ ->
                                Value
                        end
                    end, Players),
                global:send(localDB,{save_game, SessionId, GameState, Players, false}),
                io:format("No Players updated their state. Shutting Down.~n"),
                stop(SessionId);
            true ->
                HeartbeatDict = dict:map(fun(_, Value) ->
                        case Value of
                            ["online"] ->
                                ["waiting"];
                            ["waiting"] ->
                                ["offline"];
                            _ ->
                                Value
                        end
                    end, Players),
                io:format("~p~n",[dict:to_list(HeartbeatDict)]),
                session_handler(PlayerDetails, HeartbeatDict, SessionId, OnlinePlayers, NewHost, GameState, OnGoing)
            end;
        {disconnect_player, PlayerName, CurrentGameState, HandlerId} ->
            case dict:find(PlayerName,Players) of
                {ok,["online"]} ->
                % change the ended state
                    if(PlayersOnline-1 < 1) ->
                        global:send(localDB,{save_game, SessionId, CurrentGameState, Players, false}),
                        HandlerId ! {disconnected},
                        stop(SessionId);
                    true ->
                        BooleanValue = string:equal(PlayerName, HostId),
                        if BooleanValue ->
                            UpdatedPlayers = disconnectPlayer(PlayerName, Players),
                            NewHost = utils:changeHost(PlayerName,UpdatedPlayers),
                            if(OnGoing =:= "true")->
                                Message_to_publish = lists:concat(["207|",PlayerName,"|",NewHost]);
                            true -> 
                                Message_to_publish = lists:concat(["206|",PlayerName,"|",NewHost])
                            end,
                            mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                            HandlerId ! {disconnected},
                            session_handler(PlayerDetails, UpdatedPlayers,SessionId, PlayersOnline-1, NewHost, GameState, OnGoing);
                        true->
                            Message_to_publish = lists:concat(["203|",PlayerName]),
                            mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                            HandlerId ! {disconnected},
                            session_handler(PlayerDetails, disconnectPlayer(PlayerName, Players),SessionId, PlayersOnline-1, HostId, GameState, OnGoing)
                        end
                    end;
                {ok,["waiting"]} ->
                    % change the ended state
                        if(PlayersOnline-1 < 1) ->
                            global:send(localDB,{save_game, SessionId, CurrentGameState, Players, false}),
                            HandlerId ! {disconnected},
                            stop(SessionId);
                        true ->
                            BooleanValue = string:equal(PlayerName, HostId),
                            if BooleanValue ->
                                UpdatedPlayers = disconnectPlayer(PlayerName, Players),
                                NewHost = utils:changeHost(PlayerName,UpdatedPlayers),
                                if(OnGoing =:= "true")->
                                    Message_to_publish = lists:concat(["207|",PlayerName,"|",NewHost]);
                                true -> 
                                    Message_to_publish = lists:concat(["206|",PlayerName,"|",NewHost])
                                end,
                                mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                                HandlerId ! {disconnected},
                                session_handler(PlayerDetails, UpdatedPlayers,SessionId, PlayersOnline-1, NewHost, GameState, OnGoing);
                            true->
                                Message_to_publish = lists:concat(["203|",PlayerName]),
                                mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                                HandlerId ! {disconnected},
                                session_handler(PlayerDetails, disconnectPlayer(PlayerName, Players),SessionId, PlayersOnline-1, HostId, GameState, OnGoing)
                            end
                        end;
                _ ->
                    io:format("Error! Unknown player: ~p~n", [PlayerName]),
                    HandlerId ! {error},
                    session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing)
        end;
        {get_messages, HandlerId}->
            try
                Messages = localDB:db_get_logs(SessionId),
                HandlerId ! {success, Messages}
            catch
                _Error:_Reason ->
                HandlerId ! _Reason
            end,
            session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing);
        {broadcast, Message, Broadcast_handler_pid}->
            % PlayerID,City,Action~
            try
                [PlayerId, SimValues, Region, ActionId, MessageBody] = string:tokens(Message, "|"),
                PlayerVerification = lists:member(PlayerId, dict:fetch_keys(Players)),
                if PlayerVerification == false ->
                    throw("Not part of this game");
                true ->
                    Message_to_publish = lists:concat([ActionId,"|",Region,"|",PlayerId,"|",MessageBody]),
                    global:send(localDB,{save_msg, SessionId, SimValues, PlayerId, Region, ActionId, MessageBody}),
                    mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                    Broadcast_handler_pid ! {success, Message_to_publish}
                end
            catch 
                _Reason ->
                    Broadcast_handler_pid ! _Reason
            end,
            session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing);
        {broadcast_no_log, Message, Broadcast_handler_pid}->
            % PlayerID,City,Action~
            try
                [PlayerId, Region, ActionId, MessageBody] = string:tokens(Message, "|"),
                PlayerVerification = lists:member(PlayerId, dict:fetch_keys(Players)),
                if PlayerVerification == false ->
                    throw("Not part of this game");
                true ->
                    Message_to_publish = lists:concat([ActionId,"|",Region,"|",PlayerId,"|",MessageBody]),
                    mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                    Broadcast_handler_pid ! {success, MessageBody}
                end
            catch 
                _Reason ->
                    Broadcast_handler_pid ! _Reason
            end,
            session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing);
        {change_profile, Message, Change_profile_handler_pid}->
            try
                [PlayerId, NewNickname, NewAvatar] = string:tokens(Message, "|"),
                PlayerVerification = lists:member(PlayerId, dict:fetch_keys(Players)),
                if PlayerVerification == false ->
                    throw("Not part of this game");
                true ->
                    UpdatedPlayerDetails = maps:update(PlayerId, {NewNickname, NewAvatar}, PlayerDetails),
                    Message_to_publish = lists:concat(["204|",PlayerId,"|",NewNickname,"|",NewAvatar]),
                    mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                    io:format("Updated Profile: ~p~n",[UpdatedPlayerDetails]),
                    Change_profile_handler_pid ! {success, Message_to_publish}
                end
            catch 
                _Reason -> 
                    Change_profile_handler_pid ! _Reason
            end,
            session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing);
        {start_game, UserId, Start_game_handler_pid}->
            if UserId =:= HostId ->
                try
                    RegionGenerator = rand:uniform(5) - 1,
                    Message_to_publish = lists:concat(["205|",RegionGenerator]),
                    mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish),
                    Start_game_handler_pid ! {success, Message_to_publish},
                    session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, "true")
                catch _Error:_Reason -> 
                    Start_game_handler_pid ! _Reason
                end;
            true ->
                Start_game_handler_pid ! "Not Host"
            end,
            session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing);
        shutdown ->
            HeartbeatMonitorName = global:whereis_name(list_to_atom(lists:concat([SessionId,"_heartbeat_monitor"]))),
            exit(HeartbeatMonitorName,shutdown),
            io:format("Shutting down~n");
        {puback,_} ->
            session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing);
        Oops ->
            io:format("Warning! Received: ~p~n", [Oops]),
            session_handler(PlayerDetails, Players, SessionId, PlayersOnline, HostId, GameState, OnGoing)
    end.

heartbeat_Monitor(Server) ->
    {ok, Milliseconds} = application:get_env(gateway, heartbeat_interval),
    % Send heartbeat message to the target process
    io:format("hearbeat process has come to sniff  ~n"),
    timer:sleep(500),
    global:send(Server, {heartbeat_loop}),
    timer:sleep(Milliseconds),
    heartbeat_Monitor(Server).
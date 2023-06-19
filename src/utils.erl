
-module(utils).
-import(string,[concat/2]).

-compile([export_all]).

fill_list_with_null(Players) ->
    case length(Players) of
        4 -> 
            io:format("~p~n",[length(Players)]),
            Players;
        _ -> 
            io:format("~p~n",[length(Players)]),
            NewPlayers = lists:append(Players, ["null"]),
            fill_list_with_null(NewPlayers)
    end.

find_possible_new_server_name(Number) ->
    ServerName = list_to_atom(concat("Server_",integer_to_list(Number))),
    case global:whereis_name(ServerName) of
        undefined ->
            Number;
        _ ->
            find_possible_new_server_name(Number+1)
    end.

changeHost(PlayerName,Players)->
    try
        CurrentPlayerName = lists:last(dict:fetch_keys(Players)),
        case dict:fetch(CurrentPlayerName, Players) of
        ["waiting"] -> 
            utils:changeHost(PlayerName, dict:erase(CurrentPlayerName, Players));
        ["offline"] -> 
            utils:changeHost(PlayerName, dict:erase(CurrentPlayerName, Players));
        ["online"] -> 
            io:format("New Host:~p~n",[CurrentPlayerName]),
            CurrentPlayerName;
        _ -> io:format("no online/offline~n"),
        []
    end
    catch
        _:_ ->
            io:format("no online/offline~n"),
            []
    end.
    

find_number_of_online_players(SessionId,Players,HostId,OnGoing) ->
    case dict:fetch(HostId, Players) of
        ["waiting"] ->
            NewHost = changeHost(HostId,Players);
        ["offline"] -> 
            NewHost = HostId;
        _ ->
            NewHost = HostId
    end,
    [NewHost, dict:fold(fun(Client, Value, Acc) ->
        case Value of
            ["online"] -> 
                Acc + 1;
            ["waiting"] ->
                BooleanValue = string:equal(Client, HostId),
                if BooleanValue ->
                    if OnGoing =:= "true" ->
                        Message_to_publish = lists:concat(["207|",Client,"|",NewHost]);
                    true ->
                        Message_to_publish = lists:concat(["207|",Client,"|",NewHost])
                    end,
                    mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish);
                true ->
                    Message_to_publish = lists:concat(["203|",Client]),
                    mqttMessenger:publish(atom_to_list(SessionId),Message_to_publish)
                end,    
                Acc;
            _-> Acc
        end
    end, 0, Players)].
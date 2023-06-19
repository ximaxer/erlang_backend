-module(localDB).

-compile([export_all]).
-include_lib("stdlib/include/qlc.hrl").
-import(exoffice, [new_workbook/0, add_worksheet/2, write_workbook/2]).
-import(exoffice_worksheet, [set_column_width/3, set_row/3, set_cell/4]).

-define(SERVER, localDB).


-record(game_actions, {playerId, sessionId, simValues, region, actionId, messageBody, creationDay, creationHour}).
-record(game_sessions, {sessionId, gameSessionFile, player1, player2, player3, player4, ended, created_on}).
 % TODO TABELA DE FEEDBACK - FALAR 1º COM DANIELA E LICINIO

save_message(GameSessionId, PlayerId, City, Body) ->
  global:send(?SERVER, {save_msg, GameSessionId, PlayerId, City, Body}).

find_messages(GameSessionId) ->
  global:send(?SERVER, {find_msgs, GameSessionId, self()}),
  receive
    {ok, Messages} ->
      Messages
  end.

start() ->
    sessionUtils:start(?SERVER, {localDB, run, [true]}).

stop() ->
    sessionUtils:stop(?SERVER).

run(FirstTime) ->
  if
    FirstTime == true ->
      init_DB(),
      run(false);
    true ->
      receive
        {save_msg, GameSessionId, SimValues, PlayerId, Region, ActionId, MessageBody} ->
          db_write_log(GameSessionId, SimValues, PlayerId, Region, ActionId, MessageBody),
          run(FirstTime);
        {save_game, GameSessionId, GameFile, Players, Ended} ->
          db_save_game(GameSessionId, GameFile, Players, Ended),
          run(FirstTime);
        {find_msgs, GameSessionId} ->
          Messages = db_get_logs(GameSessionId),
          lists:foreach(fun(Msg) -> io:format("~p~n",[Msg]) end, Messages),
          run(FirstTime);
        shutdown ->
          io:format("Shutting down local DB...~n")
      end
  end.

db_delete_all(Messages) ->
  F = fun() ->
	  lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Messages) end,
  mnesia:transaction(F).

db_get_logs(GameSessionId) ->
  F = fun() ->
    Query = qlc:q([M || M <- mnesia:table(game_actions),
                        M#game_actions.sessionId =:= GameSessionId]),
    Results = qlc:e(Query),
    lists:map(fun(Msg) -> 
      lists:concat(["\"",Msg#game_actions.creationDay,"|",Msg#game_actions.creationHour,"\":\"",Msg#game_actions.actionId,"|", Msg#game_actions.playerId,"-",Msg#game_actions.region,"-",Msg#game_actions.messageBody,"\"\n\t",Msg#game_actions.simValues])
      end, 
      Results)
  end,
  case mnesia:transaction(F) of
    {atomic,Messages} -> Messages;
    {aborted,Reason} -> io:format("Mnesia error:~p~n",[Reason])
  end.


db_get_logs() ->
  F = fun() ->
    Query = qlc:q([M || M <- mnesia:table(game_actions)]),
    Results = qlc:e(Query),
    lists:map(fun(Msg) -> 
      lists:concat([Msg#game_actions.creationDay,",",Msg#game_actions.creationHour,",",Msg#game_actions.sessionId,",",Msg#game_actions.playerId,",", Msg#game_actions.region,",",Msg#game_actions.actionId,",",Msg#game_actions.messageBody,",",Msg#game_actions.simValues])
      end, 
      Results)
  end,
  case mnesia:transaction(F) of
    {atomic,Messages} -> Messages;
    {aborted,Reason} -> io:format("Mnesia error:~p~n",[Reason])
  end.

db_wipe_logs() ->
  F = fun() ->
    Query = qlc:q([M || M <- mnesia:table(game_actions)]),
    Results = qlc:e(Query),
    db_delete_all(Results)
  end,
  case mnesia:transaction(F) of
    {atomic,_} -> io:format("All Messages Deleted!~n");
    {aborted,Reason} -> io:format("Mnesia error:~p~n",[Reason])
  end.
  

db_write_log(GameSessionId, SimValues, PlayerId, Region, ActionId, MessageBody) ->
  F = fun() ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:universal_time(),
    CreationDay = lists:concat([Day,"/",Month,"/",Year]),
    CreationHour = lists:concat([Hour,":",Minute,":",Second]),
    mnesia:write(#game_actions{playerId=PlayerId, sessionId=GameSessionId, simValues=SimValues, region=Region, actionId=ActionId, messageBody=MessageBody, creationDay=CreationDay, creationHour=CreationHour})
   
  end,
  case mnesia:transaction(F) of
    {atomic,_} -> io:format("Log saved correctly~n");
    {aborted,Reason} -> io:format("Mnesia error:~p~n",[Reason])
  end.

db_save_game(GameSessionId, GameFile, Players, Ended) ->
  MyPlayers = utils:fill_list_with_null(dict:fetch_keys(Players)),
  F = fun() ->
    CreatedOn = erlang:monotonic_time(seconds),
    mnesia:write(#game_sessions{sessionId=GameSessionId, gameSessionFile = GameFile, player1=lists:nth(1,MyPlayers), player2=lists:nth(2,MyPlayers), player3=lists:nth(3,MyPlayers), player4=lists:nth(4,MyPlayers), ended=Ended, created_on=CreatedOn}) end,
    case mnesia:transaction(F) of
      {atomic,_} -> io:format("Game Inserted Successfuly!~n");
      {aborted,Reason} -> io:format("Mnesia error:~p~n",[Reason])
    end.

db_load_game(GameSessionId) ->
  F = fun() ->
    Query = qlc:q([M || M <- mnesia:table(game_sessions),
                        M#game_sessions.sessionId =:= GameSessionId]),
    Results = qlc:e(Query),
    Results end,
  {atomic, GameFile} = mnesia:transaction(F),
  GameFile.

db_get_games() ->
  F = fun() ->
    Query = qlc:q([M || M <- mnesia:table(game_sessions)]),
    Results = qlc:e(Query),
    lists:map(fun(Msg) -> 
      JsonString = Msg#game_sessions.gameSessionFile,
      JsonBinary = binary_to_list(JsonString),
      lists:concat(["Completed: ",Msg#game_sessions.ended," ==> ",Msg#game_sessions.sessionId,"  ",JsonBinary,", players: ",Msg#game_sessions.player1,",",Msg#game_sessions.player2,",",Msg#game_sessions.player3,",",Msg#game_sessions.player4])
      end, Results)
  end,
  case mnesia:transaction(F) of
    {atomic,Messages} -> Messages;
    {aborted,Reason} -> io:format("Mnesia error:~p~n",[Reason])
  end.

db_get_last_game_number() ->
  F = fun() ->
    Query = qlc:q([M || M <- mnesia:table(game_sessions)]),
    Results = qlc:e(Query),
    lists:map(fun(Msg) -> 
      [_,Server_number] = string:tokens(atom_to_list(Msg#game_sessions.sessionId), "_"),
      list_to_integer(Server_number)
      end, 
      Results)
  end,
  case mnesia:transaction(F) of
    {atomic,GameIds} -> 
      case length(GameIds) of 
        0 -> 
          0;
        _ -> 
        lists:last(lists:sort(GameIds))
      end;
    {aborted,Reason} -> io:format("Mnesia error:~p~n",[Reason])
  end.

db_wipe_games() ->
  F = fun() ->
    Query = qlc:q([M || M <- mnesia:table(game_sessions)]),
    Results = qlc:e(Query),
    db_delete_all(Results)
  end,
  case mnesia:transaction(F) of
    {atomic,_} -> io:format("All Games Deleted!~n");
    {aborted,Reason} -> io:format("Mnesia error:~p~n",[Reason])
  end.

db_is_user_in_game(ClientId,GameId) ->
  F = fun() ->
    Query = qlc:q([M || M <- mnesia:table(game_sessions),
      M#game_sessions.ended =:= false,
      M#game_sessions.sessionId =:= GameId]),
    Results = qlc:e(Query),
    lists:map(fun(Msg) ->
      MyList_empty = [],
      MyList1 = lists:append(MyList_empty,[Msg#game_sessions.player1]),
      MyList2 = lists:append(MyList1,[Msg#game_sessions.player2]),
      MyList3 = lists:append(MyList2,[Msg#game_sessions.player3]),
      MyList4 =lists:append(MyList3,[Msg#game_sessions.player4]),
      MyList4
      end, 
      Results)
  end,
  case mnesia:transaction(F) of
    {atomic,Value} -> lists:member(ClientId, lists:last(Value));
    {aborted,Reason} -> io:format("Mnesia error:~p~n",[Reason])
  end.

db_get_user_games(ClientId) ->
  F = fun() ->
    Query = qlc:q([M || M <- mnesia:table(game_sessions), 
                  M#game_sessions.ended =:= false,
                  lists:member(ClientId, [M#game_sessions.player1, M#game_sessions.player2, M#game_sessions.player3, M#game_sessions.player4])]),
    Results = qlc:e(Query),
    lists:map(fun(Msg) ->
      lists:concat([Msg#game_sessions.sessionId])
      end, 
      Results)
  end,
  case mnesia:transaction(F) of
    {atomic,Value} -> Value;
    {aborted,Reason} -> io:format("Mnesia error:~p~n",[Reason])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%                EXCEL FUNCTIONS                %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export_logs() ->
  RawData= db_get_logs(),
  {ok, FilePath} = application:get_env(gateway, log_file_path),
  {ok, Device} = file:open(FilePath, [write]),

  Headers = string:join(["Day", "Hour", "Server", "Player", "Region", "ActionId", "Body", "Simulation Values"],","),
  io:format(Device, "~s~n", [Headers]),
  write_csv(Device, RawData),
  file:close(Device).

write_csv(Device, [Row | Rows]) ->
    io:format(Device, "~s~n", [format_row(Row)]),
    write_csv(Device, Rows);
  write_csv(_, []) -> ok.

format_row(Row) ->
    lists:flatten([format_cell(Cell) || Cell <- Row]).

format_cell(Cell) when is_list(Cell) ->
    "\"" ++ Cell ++ "\"";
format_cell(Cell) ->
    Cell.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init_DB() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  io:format("mnesia starting...~n"),
  try
    mnesia:table_info(game_actions, type),
    io:format("found games table...~n")
  catch
    exit: _ ->
      io:format("creating games table...~n"),
      mnesia:create_table(game_actions, [{attributes, record_info(fields, game_actions)},
          {type, bag},
          {disc_copies, [node()]}])
  end,
  try
    mnesia:table_info(game_sessions, type),
    io:format("found games table...~n")
  catch
    exit: _ ->
      io:format("creating games table...~n"),
      mnesia:create_table(game_sessions, [{attributes, record_info(fields, game_sessions)},
					 {type, bag},
					 {disc_copies, [node()]}])
  end.

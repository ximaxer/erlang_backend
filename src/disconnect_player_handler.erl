-module(disconnect_player_handler).
-import(string,[concat/2]).

-export([init/2]).

init(Req0, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"*">>, Req2),
    Req4 = try
        <<"POST">> = cowboy_req:method(Req3), % Assert supported type
        true = cowboy_req:has_body(Req3),
        cowboy_req:read_body(Req3) of
        {ok, PostBody, Req5} ->
            processBody(PostBody, Req5)
    catch
        _Error:_Reason ->
            Output= list_to_binary(lists:concat(["Bad Request,",<<_Reason>>])),
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Output, Req3)
    end,
    {ok, Req4, State}.


processBody(PostBody, Req0) ->
    case jsone:decode(PostBody, [{object_format, map}]) of
    #{<<"clientId">> := ClientId,<<"serverId">> := ServerId,<<"gameFile">> := GameFile} ->
        disconnect_from_game(list_to_atom(binary_to_list(ServerId)),binary_to_list(ClientId),GameFile,Req0);
    _ ->
        cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, <<"Invalid or missing parameter">>, Req0)
    end.

disconnect_from_game(SessionName, ClientId, GameFile, Req0) ->
    case global:whereis_name(SessionName) of
        undefined ->
            cowboy_req:reply(200,
            #{<<"content-type">> => <<"application/json; charset=utf-8">>},
            "{\"Exists\": false,\n\t\"Error\": \"This game no longer exists.\"}", Req0);
        _->
            global:send(SessionName,{disconnect_player,ClientId,GameFile,self()}),
            receive
                {disconnected}->
                    Reply = list_to_binary(lists:concat(["{\"Left\": \"",ClientId,"\"}"])),
                    cowboy_req:reply(200,#{<<"content-type">> => <<"application/json; charset=utf-8">>}, Reply, Req0);
                _ ->
                    cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json; charset=utf-8">>},
                    "{\"Error\": \"Couldn't abandon current game.\"}", Req0)
            end
        end.
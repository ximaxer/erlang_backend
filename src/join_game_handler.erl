-module(join_game_handler).
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
            Output= list_to_binary(lists:concat(["Bad Request,",_Reason])),
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Output, Req3)
    end,
    {ok, Req4, State}.


processBody(PostBody, Req0) ->
    case jsone:decode(PostBody, [{object_format, map}]) of
    #{<<"clientId">> := ClientId,<<"avatar">> := ClientAvatar,<<"nickname">> := ClientNickname,<<"serverId">> := ServerId} ->
        join_game(list_to_atom(binary_to_list(ServerId)),binary_to_list(ClientId),binary_to_list(ClientAvatar),binary_to_list(ClientNickname),Req0);
    _ ->
        cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>},
            "{\"Error\": \"Invalid or missing parameter\"}", Req0)
    end.

join_game(SessionName, ClientId, ClientAvatar, ClientNickname, Req0) ->
    case global:whereis_name(SessionName) of
        undefined ->
            cowboy_req:reply(200,
            #{<<"content-type">> => <<"application/json; charset=utf-8">>},
            "{\"Exists\": false,\n\t\"Error\": \"This game no longer exists.\"}", Req0);
        _ ->
            global:send(SessionName,{register_player, ClientId, ClientAvatar, ClientNickname, self()}),
            receive
                {connected, ClientDetails, HostId}->
                    NicknamesString = get_client_nicknames(ClientDetails),
                    AvatarString = get_client_avatars(ClientDetails),
                    ClientIdsString = get_client_ids(ClientDetails),
                    Reply = list_to_binary(lists:concat(["{\"Exists\": true,\n\t\"Host\": \"",HostId,"\",\n\t\"ClientIds\": \"",ClientIdsString,"\",\n\t\"Nicknames\": \"",NicknamesString,"\",\n\t\"Avatars\": \"",AvatarString,"\"}"])),
                    cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json; charset=utf-8">>},
                        Reply, Req0);
                _ ->
                    cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json; charset=utf-8">>},
                    "{\"Error\": \"The desired game is already full.\"}", Req0)
            end
    end.


get_client_avatars(ClientDict)->
    Avatars = lists:map(fun({_, Avatar}) -> Avatar end, maps:values(ClientDict)),
    AvatarsString = string:join(Avatars, "|"),
    AvatarsString.

get_client_nicknames(ClientDict)->
    Nicknames = lists:map(fun({Nickname, _}) -> Nickname end, maps:values(ClientDict)),
    NicknamesString = string:join(Nicknames, "|"),
    NicknamesString.

get_client_ids(ClientDict)->
    Ids = maps:keys(ClientDict),
    IdsString = string:join(Ids, "|"),
    IdsString.
-module(create_game_handler).
-import(string,[concat/2]).

-export([init/2]).

init(Req0, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"*">>, Req2),
    Req5 = try
        <<"POST">> = cowboy_req:method(Req3), % Assert supported type
        true = cowboy_req:has_body(Req3),
        cowboy_req:read_body(Req3) of
        {ok, PostBody, Req4} ->
            processBody(PostBody, Req4)
    catch
        _Error:_Reason ->
            Output= list_to_binary(lists:concat(["Bad Request,",<<_Reason>>])),
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Output, Req3)
    end,
    {ok, Req5, State}.


processBody(PostBody, Req0) ->
    case jsone:decode(PostBody, [{object_format, map}]) of
        #{<<"clientId">> := ClientId,<<"avatar">> := ClientAvatar,<<"nickname">> := ClientNickname} ->
            create_game(ClientId, ClientAvatar, ClientNickname, Req0);
        _ ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, <<"Invalid or missing parameter">>, Req0)
    end.

create_game(_clientId, _clientAvatar, _clientNickname, Req0) ->

    % Create a ServerId
    % Requires a ServerId to make sure he's in a game
    %NewGameID = localDB:db_get_last_game_number(),

    %localDB:db_is_user_in_game(binary_to_list(_clientId),ServerId),


    NewServerNumber = utils:find_possible_new_server_name(localDB:db_get_last_game_number()+1),
    SessionName = list_to_atom(concat("Server_",integer_to_list(NewServerNumber))),
    % Verify against captcha
    case gameSession:start(SessionName) of
        0 -> cowboy_req:reply(200,
            #{<<"content-type">> => <<"application/json; charset=utf-8">>},
            <<"{\"Error creating game\": \"Game Already Exists\"}">> , Req0);
        1 ->
            global:send(SessionName,{register_player,binary_to_list(_clientId),binary_to_list(_clientAvatar), binary_to_list(_clientNickname),self()}),
            Response = list_to_binary(lists:concat(["{\"Success\": \"",atom_to_list(SessionName),"\"}"])),
            cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json; charset=utf-8">>},
                Response, Req0)
    end.
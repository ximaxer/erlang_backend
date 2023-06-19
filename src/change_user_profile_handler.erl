-module(change_user_profile_handler).
-import(string,[concat/2]).

-export([init/2]).

init(Req0, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"*">>, Req2),
    Req5 = try
        <<"POST">> = cowboy_req:method(Req2), % Assert supported type
        true = cowboy_req:has_body(Req2),
        cowboy_req:read_body(Req2) of
        {ok, PostBody, Req4} ->
            processBody(PostBody, Req4)
    catch
        _Error:_Reason ->
            Output= list_to_binary(lists:concat(["Bad Request,",_Reason])),
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Output, Req3)
    end,
    {ok, Req5, State}.


processBody(PostBody, Req0) ->
    case jsone:decode(PostBody, [{object_format, map}]) of
    #{<<"serverId">> := ServerId,<<"clientId">> := ClientId, <<"nickname">> := Nickname, <<"avatar">> := Avatar} ->
        Message = lists:concat([binary_to_list(ClientId),"|",binary_to_list(Nickname),"|",binary_to_list(Avatar)]),
        change_profile(list_to_atom(binary_to_list(ServerId)),Message,Req0);
    _ ->
        cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, <<"Invalid or missing parameter">>, Req0)
    end.

change_profile(ServerName, Message,Req0) -> 
    case global:whereis_name(ServerName) of
        undefined ->
            cowboy_req:reply(400,
            #{<<"content-type">> => <<"application/json; charset=utf-8">>},
            "{\"Error\": \"This game no longer exists.\"}", Req0);
        _->
            global:send(ServerName,{change_profile,Message,self()}),
            receive
                {success, Published_Message} ->
                    Reply = list_to_binary(lists:concat(["{\"Broadcasted\": \"",Published_Message,"\"}"])),
                    cowboy_req:reply(200,#{<<"content-type">> => <<"application/json; charset=utf-8">>},Reply , Req0);
                ErrorCode ->
                    Output= list_to_binary(lists:concat(["{\"Error\": \"",ErrorCode,"\"}"])),
                    cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Output, Req0)
            end
    end.
-module(broadcast_handler).
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
        % playerId, sessionId, region, {SimulationValues}, actionId, messageBody
    #{<<"serverId">> := ServerId,<<"simValues">> := SimValues,<<"clientId">> := ClientId, <<"region">> := Region, <<"actionCode">> := ActionId, <<"messageBody">> := Body} ->
        Message = lists:concat([binary_to_list(ClientId),"|",binary_to_list(SimValues),"|",binary_to_list(Region),"|",integer_to_list(ActionId),"|",binary_to_list(Body)]),
        broadcast(list_to_atom(binary_to_list(ServerId)),Message,Req0);
    _ ->
        cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, <<"Invalid or missing parameter">>, Req0)
    end.
    
broadcast(ServerName, Message,Req0) ->
    case global:whereis_name(ServerName) of
        undefined ->
            cowboy_req:reply(200,
            #{<<"content-type">> => <<"application/json; charset=utf-8">>},
            "{\"Exists\": false,\n\t\"Error\": \"This game no longer exists.\"}", Req0);
        _->
            global:send(ServerName,{broadcast,Message, self()}),
            receive
            {success, Published_Message} ->
                Reply = list_to_binary(lists:concat(["{\"Broadcasted\": \"",Published_Message,"\"}"])),
                cowboy_req:reply(200,#{<<"content-type">> => <<"application/json; charset=utf-8">>},Reply , Req0);
            Oops ->
                    Output= list_to_binary(lists:concat(["{\"Error\": \"",Oops,"\"}"])),
                    cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Output, Req0)
            end
    end.
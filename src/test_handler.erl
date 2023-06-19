-module(test_handler).
-import(string,[concat/2]).

-export([init/2]).

init(Req0, State) ->    
Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req0),
Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"*">>, Req2),
Req4 = try
        io:format("test~n"),
        cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, <<"Success">>, Req3)
    catch
        _Error:_Reason ->
            io:format("test~n"),
            Output= list_to_binary(lists:concat(["Bad Request,",<<_Reason>>])),
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Output, Req3)
    end,
    {ok, Req4, State}.

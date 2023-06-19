-module(mqttMessenger).
-import(string,[concat/2]).
-compile([export_all]).

publish(SessionName,Message)->
    {ok, BrokerIp} = application:get_env(gateway, mqtt_broker_address),
    {ok, BrokerPort} = application:get_env(gateway, mqtt_broker_port),
    {ok, TopicRoot} = application:get_env(gateway, topic_root),
    {ok, ConnPid} = emqtt:start_link([{host, BrokerIp},{port, BrokerPort},{clientid, SessionName}]),
            {ok, _} = emqtt:connect(ConnPid),
            Topic = list_to_binary(concat(TopicRoot,SessionName)),
            % remove subsription, leave publish
            %{ok, _, _ReasonCodes} = emqtt:subscribe(ConnPid, {Topic, 1}),
            io:format("========================================================~nPublished in: ~p => ~p~n========================================================~n", [Topic, Message]),
            {ok, _PktId} = emqtt:publish(ConnPid, Topic, Message, 1),
            %{ok, _, _ReasonCode} = emqtt:unsubscribe(ConnPid, Topic),
        
            ok = emqtt:disconnect(ConnPid).
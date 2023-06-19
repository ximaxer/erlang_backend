# User Guide

The way the server is structured is as follows:

1. The Client send a request to the server.
2. The Server receives the Request and processes it, some requests will give back responses to the client, others will broadcast something on the MQTT's desired topic.
3. The Client receives the MQTT's address whenever he creates a server (e.g. the response Server\_1 will have the topic _"topic\_root/Server\_1"_ given that **topic\_root** has been properly changed on the configuration file).
4. After the Client receives the MQTT's address and the client subscribes to the MQTT topic, the client is now able to request the server for specific broadcasts using /broadcast

## Example:

It's recommended that the developers keep an excel file or some sort of table which has all the action codes registered.\
During an ongoing game, the player wants to Open a box (let say this is action code 12, and that the box's ID is 5):

1.  The player sends a broadcast with the following body:

    ```json
    {
        "serverId": "Server_1",
        "clientId": "client1",
        "region": "box_region",
        "simValues": "[]",
        "actionCode": 12,
        "messageBody": "Box 5"
    }
    ```
2.  All the connected clients which are subscribed to this topic (connected to this server) will receive the following message on MQTT:

    ```json
    "12|box_region|client1|Box 5"
    ```
3. The clients will now parse the message, splitting it by "|", for which they'll know which action to execute, in what region, who executes the action and which box they're interacting with.

## Erlang commands

If the user decides to wipe their game files from erlang's database **Mnesia**, run:

```erlang
localDB:db_wipe_games().
```

If the user decides to wipe their game logs after exporting them, run:

```erlang
localDB:db_wipe_logs().
```

If the user wants to export their current logs to the file specified on the configuration, run:

```erlang
localDB:export_logs().
```

To see every server and process running inside the erlang console run:

```erlang
global:registered_names().
```

To stop the console run:

```erlang
halt().
```

## Observations:

The game servers consist of small servers each holding up to the configurated max\_players\_per\_game, these are saved and handled on a dictionary, for which /leave\_game removes them from the dictionary, while /disconnect just makes them offline.

Keep in mind the endpoint responses are temporary, the broadcast messages are there to help the user with debugging, these will be taken out later on

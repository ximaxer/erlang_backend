---
description: Endpoint used to keep the server informed that the user is still there
---

# /heartbeat

The endpoint requires the following JSON:

```json
{
    "clientId": "client1",
    "serverId": "Server_1",
    "gameFile": "{\"Example_Json_File\"}"
}
```

In case of success, the reply is a <mark style="color:green;">200 OK</mark>, with the response:

```json
{
    "Heartbeat": "Sent"
}
```

Otherwise the following responses will be given:

1.  <mark style="color:green;">200 OK</mark>,

    ```json
    {
        "Exists": false,
        "Error": "This game no longer exists."
    }
    ```
2.  <mark style="color:red;">400 Bad Request</mark>,

    ```json
    {
        "Error": "Invalid or missing parameter"
    }
    ```
3.  <mark style="color:red;">400 Bad Request</mark>,

    ```json
    {
        "Heartbeat": "Failed"
    }
    ```

---
description: Endpoint used to broadcast a message and save it on the log table
---

# /broadcast

The endpoint requires the following JSON:

```json
{
    "serverId": "Server_1",
    "clientId": "client1",
    "region": "example_region",
    "simValues": "[]",
    "actionCode": 0,
    "messageBody": "Broadcast Example"
}
```

In case of success, the reply is a <mark style="color:green;">200 OK</mark>, with the response:

```json
{
    "Broadcasted": "0|example_region|client1|Broadcast Example"
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
2.  &#x20;<mark style="color:red;">400 Bad Request</mark>,

    ```json
    {
        "Error": "Invalid or missing parameter"
    }
    ```
3.  <mark style="color:red;">400 Bad Request</mark>, &#x20;

    ```json
    {
        "Error": "Not part of this game"
    }
    ```

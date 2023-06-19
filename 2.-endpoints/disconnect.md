---
description: Endpoint used to disconnect from an ongoing game
---

# /disconnect

The endpoint requires the following JSON:

```json
{
    "clientId": "client2",
    "serverId": "Server_1",
    "gameFile": "{\"Example_Json_File\"}"
}
```

In case of success, the reply is a <mark style="color:green;">200 OK</mark>, with the response:

```json
{
    "Left": "client2"
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
        "Error": "Couldn't abandon current game."
    }
    ```

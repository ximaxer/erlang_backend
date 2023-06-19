---
description: Endpoint used to join an existing lobby
---

# /join\_game

The endpoint requires the following JSON:

```json
{
    "clientId": "client2",
    "serverId": "Server_1",
    "avatar": "client2_avatar_link_example",
    "nickname": "client2_nickname_example"
}
```

In case of success, the reply is a <mark style="color:green;">200 OK</mark>, with the response:

```json
{
    "Exists": true,
    "Host": "client1",
    "ClientIds": "client1|client2",
    "Nicknames": "client1_nickname_example|client2_nickname_example",
    "Avatars": "client1_avatar_link_example|client2_avatar_link_example"
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
        "Error": "The desired game is already full."
    }
    ```
3.  <mark style="color:red;">400 Bad Request</mark>,

    ```json
    {
        "Error": "Invalid or missing parameter"
    }
    ```

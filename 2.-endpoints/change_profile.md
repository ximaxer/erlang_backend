---
description: Endpoint used to change a user's profile
---

# /change\_profile

The endpoint requires the following JSON:

```json
{
    "clientId": "client1",
    "serverId": "Server_1",
    "avatar": "updated_client1_avatar_link_example",
    "nickname": "updated_client1_nickname_example"
}
```

In case of success, the reply is a <mark style="color:green;">200 OK</mark>, with the response:

```json
{
    "Broadcasted": "204|client1|updated_client1_nickname_example|updated_client1_avatar_link_example"
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
        "Error": "Not part of this game"
    }
    ```
3.  &#x20;<mark style="color:red;">400 Bad Request</mark>,

    ```json
    {
        "Error": "Invalid or missing parameter"
    }
    ```

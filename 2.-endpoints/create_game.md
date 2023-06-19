---
description: Endpoint used to create a new lobby
---

# /create\_game

The endpoint requires the following JSON:

```json
{
    "clientId": "client1",
    "avatar": "client1_avatar_link_example",
    "nickname": "client1_nickname_example"
}
```

In case of success, the reply is a <mark style="color:green;">200 OK</mark>, with the response:

```json
{
    "Success": "Server_1"
}
```

Otherwise the following response will be given:

1.  <mark style="color:red;">400 Bad Request</mark>,

    ```json
    {
        "Error": "Invalid or missing parameter"
    }
    ```

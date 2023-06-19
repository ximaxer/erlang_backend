---
description: Endpoint used to get a specific game's Logs
---

# /get\_logs

The endpoint requires the following JSON:

```json
{
    "serverId": "Server_1"
}
```

Considering the game had logs stored, which come from /broadcast, the response will be a <mark style="color:green;">200 OK</mark>, with the following response:

```json
{
    "18/6/2023|14:10:26": "0|client1-Coimbra-example_1st_broadcast"
    []
    "18/6/2023|14:10:29": "0|client1-Coimbra-example_2nd_broadcast"
    []
    "18/6/2023|14:10:33": "0|client1-Coimbra-example_3rd_broadcast"
    []
}
```

Otherwise the following responses will be given:

1.  <mark style="color:red;">400 Bad Request</mark>,

    ```json
    {
        "Error": "Invalid or missing parameter"
    }
    ```
2.  <mark style="color:red;">400 Bad Request</mark>,

    ```json
    {
        "Error": "Couldn't retrieve game logs"
    }
    ```

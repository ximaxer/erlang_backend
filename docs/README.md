# Setup

## Pre-Requisites

To be able to run this erlang backend server, there's dependencies that the user must have before running it:

1. Must own a private MQTT broker, or have access to a public broker of his choice.
2. When using or developing a browser game, make sure the MQTT brokers supports WebSocket (WS) or WebSocket Secure (WSS).
3. An erlang release must be installed on the machine running the server, for installing erlang go to [https://www.erlang.org/downloads](https://www.erlang.org/downloads), further guidance will be provided in the following section.
4. A rebar3 release must be installed on the machine running the server, for installing erlang go to [https://rebar3.org/](https://rebar3.org/), further guidance will be provided in the following section.

## Pre-Requisites guide

To host his own MQTT broker, the user can follow the instructions at [https://github.com/eclipse/mosquitto](https://github.com/eclipse/mosquitto).

For Erlang, depending if it's Windows or Linux the steps will differ:

* For **Linux,** the installation is pretty much straightforward, the typical apt-get will get you started correctly.
* For **Windows**, the installation might give some errors, this is because the user needs to add the new erlang OTP to their environment variables such as: **C:\Program Files\Erlang OTP\bin**.

For Rebar3, depending it it's Windows or Linux the steps will differ:

* For Linux, the installation will not have problems.
*   For Windows, the user must have an Erlang OTP installed, and run the following commands:

    ```
    $ git clone https://github.com/erlang/rebar3.git
    $ cd rebar3
    $ ./bootstrap
    ```

    If the environment variable is not created then add a new variable such as: **C:\Program Files\rebar3**.\
    If another difficulty arises, consult [https://github.com/erlang/rebar3](https://github.com/erlang/rebar3) 's Getting Started section.

## Configuration

The configuration file is named **gateway.app.src**, and the values which the user may change are inside the {**env,value**} pair such as:

```erlang
{env, [
      {http_port, 9003}, % numerical value, defines the port for the requests
      {max_players_per_game, 4}, % numerical value defines max players per game
      {mqtt_broker_address, "my.broker.url"}, % string value, this is the ip or url of your mqtt broker
      {mqtt_broker_port, 9005}, % numerical value, this is the port in which your broker is running (can be ws or IoT port)
      {topic_root, "my_game/"}, % string value, it defines the root directory of your mqtt topic
      {heartbeat_interval, 29500}, % numeric value, represents the delay in milliseconds
      % optional:
      {log_file_path, "../log_data.csv"} % string value, this is the path to the document which is created when logs are exported
]},
```

## Build

To compile and run the server, head to the root folder of the project (the one containing rebar.config) and run the following commands:

```sh
$ rebar3 compile
$ rebar3 shell
```

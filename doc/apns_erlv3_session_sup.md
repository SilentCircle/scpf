

# Module apns_erlv3_session_sup #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`supervisor`](supervisor.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_child_pid-1">get_child_pid/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_child_alive-1">is_child_alive/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_child-2">start_child/2</a></td><td>Start a child session.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td><code>Sessions</code> looks like this:.</td></tr><tr><td valign="top"><a href="#stop_child-1">stop_child/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_child_pid-1"></a>

### get_child_pid/1 ###

`get_child_pid(Name) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="is_child_alive-1"></a>

### is_child_alive/1 ###

`is_child_alive(Name) -> any()`

<a name="start_child-2"></a>

### start_child/2 ###

`start_child(Name, Opts) -> any()`

Start a child session.


### <a name="Parameters">Parameters</a> ###


* `Name` - Session name (atom)

* `Opts` - Options, see [`apns_erlv3_session`](apns_erlv3_session.md) for more details


<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Sessions) -> any()`

`Sessions` looks like this:

```
  [
      [
          {name, 'apns-com.example.MyApp'},
          {config, [
              {host, "api.push.apple.com" | "api.development.push.apple.com"},
              {port, 443 | 2197},
              {app_id_suffix, <<"com.example.MyApp">>},
              {apns_env, prod},
              {apns_topic, <<"com.example.MyApp">>},
              {retry_delay, 1000},
              {disable_apns_cert_validation, false},
              {ssl_opts, [
                      {certfile, "/some/path/com.example.MyApp.cert.pem"},
                      {keyfile, "/some/path/com.example.MyApp.key.unencrypted.pem"},
                      {honor_cipher_order, false},
                      {versions, ['tlsv1.2']},
                      {alpn_preferred_protocols, [<<"h2">>]}
                  ]
              }
           ]}
      ] %, ...
  ]
```

<a name="stop_child-1"></a>

### stop_child/1 ###

`stop_child(Name) -> any()`


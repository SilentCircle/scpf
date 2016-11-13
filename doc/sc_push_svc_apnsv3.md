

# Module sc_push_svc_apnsv3 #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Apple Push Notification Service (APNS) API.

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##

This is the API to the Apple Push Notification Service Provider.


### <a name="Synopsis">Synopsis</a> ###


#### <a name="Starting_a_session">Starting a session</a> ####

See [`apns_erlv3_session`](apns_erlv3_session.md) for a description of `Opts`.

```
   Opts = [
           {name, 'apnsv3-com.example.FakeApp.voip'},
           {token, "ca6a7fef1...4f1dc2"},
           {config,
            [
             {host, <<"api.development.push.apple.com">>},
             {port, 443},
             {apns_env, prod},
             {apns_topic, <<"com.example.FakeApp.voip">>},
             {app_id_suffix, <<"com.example.FakeApp.voip">>},
             {team_id, <<"6F44JJ9SDF">>},
             {disable_apns_cert_validation, true},
             {ssl_opts, [
                         {cacertfile, "/etc/ssl/certs/ca-certificates.crt"},
                         {certfile, "com.example.FakeApp.cert.pem"},
                         {keyfile, "com.example.FakeApp.key.pem"},
                         {verify, verify_peer},
                         {honor_cipher_order, false},
                         {versions, ['tlsv1.2']},
                         {alpn_advertised_protocols, [<<"h2">>]}]}
            ]}
          ],
   {ok, Pid} = sc_push_svc_apnsv3:start_session(my_push_tester, Opts).
```


#### <a name="Sending_an_alert_via_the_API">Sending an alert via the API</a> ####

```
   Notification = [{alert, Alert}, {token, <<"e7b300...a67b">>}],
   {ok, ParsedResp} = sc_push_svc_apnsv3:send(my_push_tester, Notification).
```


#### <a name="Sending_an_alert_via_a_session_(for_testing_only)">Sending an alert via a session (for testing only)</a> ####

```
   JSON = get_json_payload(), % See APNS docs for format
   Nf = [{token, Token}, {json, JSON}],
   {ok, ParsedResp} = apns_erlv3_session:send(my_push_tester, Nf).
```


#### <a name="Stopping_a_session">Stopping a session</a> ####

```
   ok = sc_push_svc_apnsv3:stop_session(my_push_tester).
```

<a name="types"></a>

## Data Types ##




### <a name="type-alert_prop">alert_prop()</a> ###


<pre><code>
alert_prop() = {title, binary()} | {body, binary()} | {'title-loc-key', binary() | null} | {'title-loc-args', [binary()] | null} | {'action-loc-key', binary() | null} | {'loc-key', binary()} | {'loc-args', [binary()]} | {'launch-image', binary()}
</code></pre>




### <a name="type-alert_proplist">alert_proplist()</a> ###


<pre><code>
alert_proplist() = [<a href="#type-alert_prop">alert_prop()</a>]
</code></pre>




### <a name="type-async_send_opt">async_send_opt()</a> ###


<pre><code>
async_send_opt() = {from_pid, pid()} | {callback, <a href="apns_erlv3_session.md#type-send_callback">apns_erlv3_session:send_callback()</a>}
</code></pre>




### <a name="type-async_send_opts">async_send_opts()</a> ###


<pre><code>
async_send_opts() = [<a href="#type-async_send_opt">async_send_opt()</a>]
</code></pre>




### <a name="type-async_send_reply">async_send_reply()</a> ###


<pre><code>
async_send_reply() = <a href="apns_erlv3_session.md#type-async_send_reply">apns_erlv3_session:async_send_reply()</a>
</code></pre>




### <a name="type-fsm_ref">fsm_ref()</a> ###


<pre><code>
fsm_ref() = <a href="apns_erlv3_session.md#type-fsm_ref">apns_erlv3_session:fsm_ref()</a>
</code></pre>




### <a name="type-gen_proplist">gen_proplist()</a> ###


<pre><code>
gen_proplist() = <a href="sc_types.md#type-proplist">sc_types:proplist</a>(atom(), term())
</code></pre>

--------------------------------------------------------------------
Defines
--------------------------------------------------------------------



### <a name="type-nf_prop">nf_prop()</a> ###


<pre><code>
nf_prop() = {alert, binary() | <a href="#type-alert_proplist">alert_proplist()</a>} | {badge, integer()} | {sound, binary()} | {'content-available', 0 | 1} | {category, binary()} | {extra, <a href="apns_json.md#type-json_term">apns_json:json_term()</a>}
</code></pre>




### <a name="type-notification">notification()</a> ###


<pre><code>
notification() = [<a href="#type-nf_prop">nf_prop()</a>]
</code></pre>




### <a name="type-session">session()</a> ###


<pre><code>
session() = [<a href="#type-session_opt">session_opt()</a>]
</code></pre>




### <a name="type-session_config">session_config()</a> ###


<pre><code>
session_config() = <a href="apns_erlv3_session.md#type-options">apns_erlv3_session:options()</a>
</code></pre>




### <a name="type-session_opt">session_opt()</a> ###


<pre><code>
session_opt() = {name, atom()} | {mod, atom()} | {config, <a href="#type-session_config">session_config()</a>}
</code></pre>




### <a name="type-sessions">sessions()</a> ###


<pre><code>
sessions() = [<a href="#type-session">session()</a>]
</code></pre>




### <a name="type-sync_send_reply">sync_send_reply()</a> ###


<pre><code>
sync_send_reply() = <a href="apns_erlv3_session.md#type-sync_send_reply">apns_erlv3_session:sync_send_reply()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_send-2">async_send/2</a></td><td>Equivalent to <a href="#async_send-3"><tt>async_send(Name, Notification, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#async_send-3">async_send/3</a></td><td>Asynchronously sends a notification specified by proplist
<code>Notification</code> to <code>Name</code> with proplist <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Equivalent to <a href="#send-3"><tt>send(Name, Notification, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#send-3">send/3</a></td><td>Send a notification specified by proplist <code>Notification</code>
to <code>Name</code> with options <code>Opts</code> (currently unused).</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td><code>Sessions</code> is a list of proplists.</td></tr><tr><td valign="top"><a href="#start_session-2">start_session/2</a></td><td>Start named session for specific host and certificate as
supplied in the proplist <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#stop_session-1">stop_session/1</a></td><td>Stop named session.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_send-2"></a>

### async_send/2 ###

<pre><code>
async_send(Name, Notification) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = <a href="#type-fsm_ref">fsm_ref()</a></code></li><li><code>Notification = <a href="#type-notification">notification()</a></code></li><li><code>Result = <a href="#type-async_send_reply">async_send_reply()</a></code></li></ul>

Equivalent to [`async_send(Name, Notification, [])`](#async_send-3).

<a name="async_send-3"></a>

### async_send/3 ###

<pre><code>
async_send(Name, Notification, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = <a href="#type-fsm_ref">fsm_ref()</a></code></li><li><code>Notification = <a href="#type-notification">notification()</a></code></li><li><code>Opts = <a href="#type-async_send_opts">async_send_opts()</a></code></li><li><code>Result = <a href="#type-async_send_reply">async_send_reply()</a></code></li></ul>

Asynchronously sends a notification specified by proplist
`Notification` to `Name` with proplist `Opts`.

Returns `{ok, Result}` or `{error, term()}`, where `Result` can be either
`{queued, uuid()}` or `{submitted, uuid()}`. The difference between
`queued` and `submitted` is the HTTP/2 session connection status. If the
HTTP/2 session is connected at the time the request is made, `submitted`
is used. If the session is not connected, the request is queued internally
until the session connects and `queued` is used to show this.

It is possible that a queued notification could be lost if the session dies
before it can connect to APNS.


### <a name="Opts">Opts</a> ###

`Opts` is a proplist that can take the following properties.



<dt><code>{from_pid, pid()}</code></dt>




<dd>The <code>pid</code> that will be passed to the callback function as the <code>from</code>
property of the notification proplist (the first parameter of the
callback function). If omitted, the <code>pid</code> is set to <code>self()</code>. It will
be ignored unless the <code>callback</code> property is also provided.</dd>




<dt><code>{callback, apns_erlv3_session:send_callback()}</code></dt>




<dd>The callback function to be invoked when the asynchronous call
completes. If omitted, the standard asynchronous behavior is
used, and <code>from_pid</code> - if provided - is ignored.</dd>




#### <a name="Callback_function">Callback function</a> ####

The callback function type signature is
[`apns_erlv3_session:send_callback()`](apns_erlv3_session.md#type-send_callback). The parameters
are described in apns_erlv3_session:async_send_cb/4.

```
  fun(NfPL, Req, Resp) -> any().
```


* `NfPL` is the notification proplist as received by the session.

* `Req` is the HTTP/2 request as sent to APNS.

* `Resp` is the HTTP/2 response received from APNS, parsed into a
proplist.



#### <a name="Example_callback_function">Example callback function</a> ####

```
  -spec example_callback(NfPL, Req, Resp) -> ok when
        NfPL :: apns_erlv3_session:send_opts(),
        Req :: apns_erlv3_session:cb_req(),
        Resp :: apns_erlv3_session:cb_result().
  example_callback(NfPL, Req, Resp) ->
      case proplists:get_value(from, NfPL) of
          Caller when is_pid(Caller) ->
              UUID = proplists:get_value(uuid, NfPL),
              Caller ! {user_defined_cb, #{uuid => UUID,
                                           nf => NfPL,
                                           req => Req,
                                           resp => Resp}},
              ok;
          undefined ->
              log_error("Cannot send result, no caller info: ~p", [NfPL])
      end.
```


#### <a name="Example_parameters_to_callback_function">Example parameters to callback function</a> ####

```
  NfPL = [{uuid,<<"44e83e09-bfb6-4f67-a281-f437a7450c1a">>},
          {expiration,2147483647},
          {token,<<"de891ab30fc96af54406b22cfcb2a7da09628c62236e374f044bd49879bd8e5a">>},
          {topic,<<"com.example.FakeApp.voip">>},
          {json,<<"{\"aps\":{\"alert\":\"Hello, async user callback\"}}">>},
          {from,<0.1283.0>},
          {priority,undefined}].
  Req = {[{<<":method">>,<<"POST">>},
          {<<":path">>, <<"/3/device/de891ab30fc96af54406b22cfcb2a7da09628c62236e374f044bd49879bd8e5a">>},
          {<<":scheme">>,<<"https">>},
          {<<"apns-topic">>, <<"com.example.FakeApp.voip">>},
          {<<"apns-expiration">>, <<"2147483647">>},
          {<<"apns-id">>, <<"44e83e09-bfb6-4f67-a281-f437a7450c1a">>}
         ], <<"{\"aps\":{\"alert\":\"Hello, async user callback\"}}">>
        }.
  Resp = {ok,[{uuid,<<"44e83e09-bfb6-4f67-a281-f437a7450c1a">>},
              {status,<<"200">>},
              {status_desc,<<"Success">>}]}.
```


__See also:__ [apns_erlv3_session:async_send_cb/4](apns_erlv3_session.md#async_send_cb-4).

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(Name, Notification) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = <a href="#type-fsm_ref">fsm_ref()</a></code></li><li><code>Notification = <a href="#type-notification">notification()</a></code></li><li><code>Result = <a href="#type-sync_send_reply">sync_send_reply()</a></code></li></ul>

Equivalent to [`send(Name, Notification, [])`](#send-3).

<a name="send-3"></a>

### send/3 ###

<pre><code>
send(Name, Notification, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = <a href="#type-fsm_ref">fsm_ref()</a></code></li><li><code>Notification = <a href="#type-notification">notification()</a></code></li><li><code>Opts = <a href="#type-gen_proplist">gen_proplist()</a></code></li><li><code>Result = <a href="#type-sync_send_reply">sync_send_reply()</a></code></li></ul>

Send a notification specified by proplist `Notification`
to `Name` with options `Opts` (currently unused).

Set the notification to expire in a very very long time.


#### <a name="Example">Example</a> ####

Send an alert with a sound and extra data:

```
  Name = 'com.example.AppId',
  Notification = [
     {alert, <<"Hello">>},
     {token, <<"ea3f...">>},
     {aps, [
       {sound, <<"bang">>},
       {extra, [{a, 1}]}]}
  ],
  sc_push_svc_apnsv3:send(Name, Notification).
```


<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Sessions) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Sessions = <a href="#type-sessions">sessions()</a></code></li><li><code>Result = {ok, pid()} | {error, term()}</code></li></ul>

`Sessions` is a list of proplists.
Each proplist is a session definition containing
`name`, `mod`, and `config` keys.

__See also:__ [apns_erlv3_session:start_link/1](apns_erlv3_session.md#start_link-1).

<a name="start_session-2"></a>

### start_session/2 ###

<pre><code>
start_session(Name, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = atom()</code></li><li><code>Opts = <a href="#type-session">session()</a></code></li><li><code>Result = {ok, pid()} | {error, already_started} | {error, term()}</code></li></ul>

Start named session for specific host and certificate as
supplied in the proplist `Opts`.

__See also:__ [apns_erlv3_session_sup:start_child/2](apns_erlv3_session_sup.md#start_child-2).

<a name="stop_session-1"></a>

### stop_session/1 ###

<pre><code>
stop_session(Name) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = atom()</code></li><li><code>Result = ok | {error, Reason}</code></li><li><code>Reason = any()</code></li></ul>

Stop named session.


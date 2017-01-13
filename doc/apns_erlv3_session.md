

# Module apns_erlv3_session #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

APNS V3 (HTTP/2) server session.

Copyright (c) (C) 2012-2016 Silent Circle LLC

__Behaviours:__ [`gen_fsm`](gen_fsm.md).

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##

There must be one session per App ID Suffix and certificate (Development or
Production) combination. Sessions must have unique (i.e. they are
registered) names within the node.

When connecting or disconnecting, all the notifications received by the
session are put in the input queue. When the connection is established, all
the queued notifications are sent to the APNS server.

When failing to connect, the delay between retries will grow exponentially
up to a configurable maximum value. The delay is reset when successfully
reconnecting.

When the session receives an error from APNS servers, it unregisters the
token if the error was due to a bad token.

Process configuration:


<dt><code>host</code></dt>




<dd>The hostname of the APNS HTTP/2 service as a binary string. This may
be omitted as long as <code>apns_env</code> is present. In this case, the code will
choose a default host using <code>apns_lib_http2:host_port/1</code> based on the
environment.
</dd>



<dt><code>port</code></dt>




<dd>The port of the APNS HTTP/2 service as a positive integer. This may
be omitted as long as <code>apns_env</code> is present. In this case, the code will
choose a default port using <code>apns_lib_http2:host_port/1</code> based on the
environment.
</dd>



<dt><code>apns_env</code></dt>




<dd>The push notification environment. This can be either <code>'dev'</code>
or <code>'prod'</code>. This is *mandatory*.
</dd>



<dt><code>team_id</code></dt>




<dd>The 10-character Team ID as a binary.  This is
used for one of two purposes:
<ul>
<li>To validate the APNS certificate unless
<code>disable_apns_cert_validation</code> is <code>true</code>.</li>
<li>When JWT authentication is active, it will be
used as the Issuer (iss) in the JWT.</li>
</ul>
</dd>



<dt><code>app_id_suffix</code></dt>




<dd>The AppID Suffix as a binary, usually in reverse DNS format.  This is
used to validate the APNS certificate unless
<code>disable_apns_cert_validation</code> is <code>true</code>.</dd>



<dt><code>apns_jwt_info</code></dt>




<dd><code>{Kid :: binary(), KeyFile :: binary()} | undefined</code>.<code>Kid</code> is the
key id corresponding to the signind key.<code>KeyFile</code> is the name of the
PEM-encoded JWT signing key to be used for authetication. This value is
mutually exclusive with <code>ssl_opts</code>.  If this value is provided and is not
<code>undefined</code>, <code>ssl_opts</code> will be ignored.</dd>



<dt><code>apns_topic</code></dt>




<dd>The <b>default</b> APNS topic to which to push notifications. If a
topic is provided in the notification, it always overrides the default.
This must be one of the topics in <code>certfile</code>, otherwise the notifications
will all fail, unless the topic is explicitly provided in the
notifications.
<br />
If this is omitted and the certificate is a multi-topic certificate, the
notification will fail unless the topic is provided in the actual push
notification. Otherwise, with regular single-topic certificates, the
first app id suffix in <code>certfile</code> is used.
<br />
Default value:
<ul>
<li>If multi-topic certificate: none (notification will fail)</li>
<li>If NOT multi-topic certificate: First app ID suffix in <code>certfile</code></li>
</ul>
</dd>



<dt><code>retry_strategy</code></dt>




<dd>The strategy to be used when reattempting connection to APNS.
Valid values are <code>exponential</code> and <code>fixed</code>.
<ul>
<li>When <code>fixed</code>, the same <code>retry_delay</code> is used for every reconnection attempt.</li>
<li>When <code>exponential</code>, the <code>retry_delay</code> is multiplied by 2 after each
unsuccessful attempt, up to <code>retry_max</code>.</li>
</ul>
<br />
Default value: <code>exponential</code>.
</dd>



<dt><code>retry_delay</code></dt>




<dd>The minimum time in milliseconds the session will wait before
reconnecting to APNS servers as an integer; when reconnecting multiple
times this value will be multiplied by 2 for every attempt if
<code>retry_strategy</code> is <code>exponential</code>.
<br />
Default value: <code>1000</code>.
</dd>



<dt><code>retry_max</code></dt>




<dd>The maximum amount of time in milliseconds that the session will wait
before reconnecting to the APNS servers. This serves to put an upper
bound on <code>retry_delay</code>, and only really makes sense if using a non-fixed
strategy.
<br />
Default value: <code>60000</code>.
</dd>



<dt><code>disable_apns_cert_validation</code></dt>




<dd><code>true</code> if APNS certificate validation against its app id suffix and
team ID should be disabled, <code>false</code> if the validation should be done.
This option exists to allow for changes in APNS certificate layout
without having to change code.
<br />
Default value: <code>false</code>.
</dd>



<dt><code>jwt_max_age_secs</code></dt>




<dd>The number of seconds, measured from the "issued at" (iat)
JWT POSIX time, after which the JWT should be preemptively reissued.
The reissuance occurs just prior to the next notification transmission.
<br />
Default value: 3300 seconds (55 minutes).
</dd>



<dt><code>keepalive_interval</code></dt>




<dd>The number of seconds after which a PING should be sent to the
peer host.
<br />
Default value: 300 seconds (5 minutes).
</dd>



<dt><code>ssl_opts</code></dt>




<dd>The property list of SSL options including the certificate file path.
See <a href="http://erlang.org/doc/man/ssl.md" target="_top"><tt>http://erlang.org/doc/man/ssl.html</tt></a>.
</dd>




#### <a name="Example_configuration">Example configuration</a> ####


```
   [{host, <<"api.push.apple.com">>},
    {port, 443},
    {apns_env, prod},
    {apns_topic, <<"com.example.MyApp">>},
    {apns_jwt_info, {<<"KEYID67890">>, <<"/path/to/private/key.pem">>}},
    {app_id_suffix, <<"com.example.MyApp">>},
    {team_id, <<"6F44JJ9SDF">>},
    {retry_strategy, exponential},
    {retry_delay, 1000},
    {retry_max, 60000},
    {disable_apns_cert_validation, false},
    {jwt_max_age_secs, 3300},
    {keepalive_interval, 300},
    {ssl_opts,
     [{certfile, "/some/path/com.example.MyApp.cert.pem"},
      {keyfile, "/some/path/com.example.MyApp.key.unencrypted.pem"},
      {cacertfile, "/etc/ssl/certs/ca-certificates.crt"},
      {honor_cipher_order, false},
      {versions, ['tlsv1.2']},
      {alpn_preferred_protocols, [<<"h2">>]}].
     ]}
   ]
```


<a name="types"></a>

## Data Types ##




### <a name="type-async_send_reply">async_send_reply()</a> ###


<pre><code>
async_send_reply() = {ok, <a href="#type-async_send_result">async_send_result()</a>} | {error, term()}
</code></pre>




### <a name="type-async_send_result">async_send_result()</a> ###


<pre><code>
async_send_result() = <a href="#type-queued_result">queued_result()</a> | <a href="#type-submitted_result">submitted_result()</a>
</code></pre>




### <a name="type-bstrtok">bstrtok()</a> ###


<pre><code>
bstrtok() = binary()
</code></pre>

 binary of string rep of APNS token.



### <a name="type-caller">caller()</a> ###


<pre><code>
caller() = pid() | {pid(), term()}
</code></pre>




### <a name="type-cb_req">cb_req()</a> ###


<pre><code>
cb_req() = <a href="apns_lib_http2.md#type-http2_req">apns_lib_http2:http2_req()</a>
</code></pre>




### <a name="type-cb_result">cb_result()</a> ###


<pre><code>
cb_result() = {ok, {<a href="#type-uuid">uuid()</a>, <a href="apns_lib_http2.md#type-parsed_rsp">apns_lib_http2:parsed_rsp()</a>}} | {error, term()}
</code></pre>




### <a name="type-flush_strategy_opt">flush_strategy_opt()</a> ###


<pre><code>
flush_strategy_opt() = on_reconnect | debug_clear
</code></pre>




### <a name="type-fsm_ref">fsm_ref()</a> ###


<pre><code>
fsm_ref() = atom() | pid()
</code></pre>




### <a name="type-option">option()</a> ###


<pre><code>
option() = {host, binary()} | {port, non_neg_integer()} | {app_id_suffix, binary()} | {team_id, binary()} | {apns_env, prod | dev} | {apns_topic, binary()} | {disable_apns_cert_validation, boolean()} | {jwt_max_age_secs, non_neg_integer()} | {keepalive_interval, non_neg_integer()} | {ssl_opts, list()} | {retry_delay, non_neg_integer()} | {retry_max, pos_integer()} | {retry_strategy, fixed | exponential} | {flush_strategy, <a href="#type-flush_strategy_opt">flush_strategy_opt()</a>} | {requeue_strategy, <a href="#type-requeue_strategy_opt">requeue_strategy_opt()</a>}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-queued_result">queued_result()</a> ###


<pre><code>
queued_result() = {queued, <a href="#type-uuid">uuid()</a>}
</code></pre>




### <a name="type-reply_fun">reply_fun()</a> ###


<pre><code>
reply_fun() = fun((<a href="#type-caller">caller()</a>, <a href="#type-uuid">uuid()</a>, <a href="#type-cb_result">cb_result()</a>) -&gt; none())
</code></pre>




### <a name="type-requeue_strategy_opt">requeue_strategy_opt()</a> ###


<pre><code>
requeue_strategy_opt() = always | debug_never
</code></pre>




### <a name="type-send_callback">send_callback()</a> ###


<pre><code>
send_callback() = fun((list(), <a href="#type-cb_req">cb_req()</a>, <a href="#type-cb_result">cb_result()</a>) -&gt; term())
</code></pre>




### <a name="type-send_opt">send_opt()</a> ###


<pre><code>
send_opt() = {token, <a href="#type-bstrtok">bstrtok()</a>} | {topic, binary()} | {uuid, <a href="#type-uuid_str">uuid_str()</a>} | {priority, integer()} | {expiry, integer()} | {json, binary()} | {authorization, binary()}
</code></pre>




### <a name="type-send_opts">send_opts()</a> ###


<pre><code>
send_opts() = [<a href="#type-send_opt">send_opt()</a>]
</code></pre>




### <a name="type-submitted_result">submitted_result()</a> ###


<pre><code>
submitted_result() = {submitted, <a href="#type-uuid">uuid()</a>}
</code></pre>




### <a name="type-sync_result">sync_result()</a> ###


<pre><code>
sync_result() = {<a href="#type-uuid">uuid()</a>, <a href="apns_lib_http2.md#type-parsed_rsp">apns_lib_http2:parsed_rsp()</a>}
</code></pre>




### <a name="type-sync_send_reply">sync_send_reply()</a> ###


<pre><code>
sync_send_reply() = {ok, <a href="#type-sync_send_result">sync_send_result()</a>} | {error, term()}
</code></pre>




### <a name="type-sync_send_result">sync_send_result()</a> ###


<pre><code>
sync_send_result() = <a href="#type-sync_result">sync_result()</a>
</code></pre>




### <a name="type-uuid">uuid()</a> ###


<pre><code>
uuid() = binary()
</code></pre>

128-bit raw UUID



### <a name="type-uuid_str">uuid_str()</a> ###


<pre><code>
uuid_str() = <a href="apns_lib_http2.md#type-uuid_str">apns_lib_http2:uuid_str()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_send-2">async_send/2</a></td><td>Asynchronously send notification in <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#async_send-3">async_send/3</a></td><td>Asynchronously send notification in <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#async_send_callback-3">async_send_callback/3</a></td><td>Standard async callback function.</td></tr><tr><td valign="top"><a href="#async_send_cb-4">async_send_cb/4</a></td><td>Asynchronously send notification in <code>Opts</code> with user-defined
callback function.</td></tr><tr><td valign="top"><a href="#flush-1">flush/1</a></td><td>Flush (retransmit) any queued notifications.</td></tr><tr><td valign="top"><a href="#get_state-1">get_state/1</a></td><td>Get the current state of the FSM.</td></tr><tr><td valign="top"><a href="#get_state_name-1">get_state_name/1</a></td><td>Get the name of the current state of the FSM.</td></tr><tr><td valign="top"><a href="#is_connected-1">is_connected/1</a></td><td>Return <code>true</code> if the session is connected, <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#quiesce-1">quiesce/1</a></td><td>Quiesce a session.</td></tr><tr><td valign="top"><a href="#reconnect-1">reconnect/1</a></td><td>Immediately disconnect the session and reconnect.</td></tr><tr><td valign="top"><a href="#reconnect-2">reconnect/2</a></td><td>Immediately disconnect the session and reconnect after <code>Delay</code> ms.</td></tr><tr><td valign="top"><a href="#resume-1">resume/1</a></td><td>Resume a quiesced session.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send a notification specified by <code>Nf</code> with options <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#send_cb-3">send_cb/3</a></td><td>Send a notification specified by <code>Nf</code> and a user-supplied callback
function.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Start a named session as described by the options <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>Start a named session as described by the options <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop session.</td></tr><tr><td valign="top"><a href="#sync_send_callback-3">sync_send_callback/3</a></td><td>Standard sync callback function.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_send-2"></a>

### async_send/2 ###

<pre><code>
async_send(FsmRef, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>FsmRef = <a href="#type-fsm_ref">fsm_ref()</a></code></li><li><code>Opts = <a href="#type-send_opts">send_opts()</a></code></li><li><code>Result = <a href="#type-async_send_reply">async_send_reply()</a></code></li></ul>

Asynchronously send notification in `Opts`.
If `id` is not provided in `Opts`, generate a UUID.  When a response is
received from APNS, send it to the caller's process as a message in the
format `{apns_response, v3, {uuid(), Resp :: cb_result()}}`.

<a name="async_send-3"></a>

### async_send/3 ###

<pre><code>
async_send(FsmRef, ReplyPid, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>FsmRef = <a href="#type-fsm_ref">fsm_ref()</a></code></li><li><code>ReplyPid = pid()</code></li><li><code>Opts = <a href="#type-send_opts">send_opts()</a></code></li><li><code>Result = <a href="#type-async_send_reply">async_send_reply()</a></code></li></ul>

Asynchronously send notification in `Opts`.
If `id` is not provided in `Opts`, generate a UUID.  When a response is
received from APNS, send it to the caller's process as a message in the
format `{apns_response, v3, {uuid(), Resp :: cb_result()}}`.

__See also:__ [async_send/1](#async_send-1).

<a name="async_send_callback-3"></a>

### async_send_callback/3 ###

<pre><code>
async_send_callback(NfPL, Req, Resp) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>NfPL = [{atom(), term()}]</code></li><li><code>Req = <a href="#type-cb_req">cb_req()</a></code></li><li><code>Resp = <a href="#type-cb_result">cb_result()</a></code></li><li><code>Result = <a href="#type-cb_result">cb_result()</a></code></li></ul>

Standard async callback function. This is not normally needed
outside of this module.

__See also:__ [async_send_cb/1](#async_send_cb-1).

<a name="async_send_cb-4"></a>

### async_send_cb/4 ###

<pre><code>
async_send_cb(FsmRef, ReplyPid, Opts, Callback) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>FsmRef = <a href="#type-fsm_ref">fsm_ref()</a></code></li><li><code>ReplyPid = pid()</code></li><li><code>Opts = <a href="#type-send_opts">send_opts()</a></code></li><li><code>Callback = <a href="#type-send_callback">send_callback()</a></code></li><li><code>Result = <a href="#type-async_send_reply">async_send_reply()</a></code></li></ul>

Asynchronously send notification in `Opts` with user-defined
callback function. If `id` is not provided in `Opts`, generate a UUID.
When the request has completed, invoke the user-defined callback
function.

Note that there are different returns depending on the state of the
session.

If the session has been quiesced by calling quiesce/1, all
subsequent attempts to send notifications will receive `{error, quiesced}`
responses.

If the session is busy connecting to APNS (or disconnecting from APNS),
attempts to send will receive a response, `{ok, {queued, UUID ::
binary()}}`. The `queued` status means that the notification is being held
until the session is able to connect to APNS, at which time it will be
submitted. Queued notifications can be lost if the session is stopped
without connecting.

If the session is already connected (and not quiesced), and the notification
passes preliminary validation, attempts to send will receive a response
`{ok, {submitted, UUID :: binary()}}`.  This means that the notification has
been accepted by the HTTP/2 client for asynchronous processing, and the
callback function will be invoked at completion.


### <a name="Timeouts">Timeouts</a> ###

There are multiple kinds of timeouts.

If the session itself is so busy that the send request cannot be processed in time,
a timeout error will occur. The default Erlang call timeout is applicable here.

An asynchronous timeout feature is planned but not currently implemented.


### <a name="Callback_function">Callback function</a> ###

The callback function provided must have the following form:

```
  fun(NfPL, Req, Resp) -> any().
```

The function return value is ignored. Throw an exception or raise an error
to indicate callback failure instead.


#### <a name="Function_Parameters">Function Parameters</a> ####



<dt><code>NfPL :: [{atom(), binary() | non_neg_integer() | {pid(), any()}</code></dt>




<dd>The notification data as a proplist. See below for a description.</dd>




<dt><code>Req :: {Headers :: [{binary(), binary()}], Body :: binary()}</code></dt>




<dd>The original request headers and data</dd>




<dt><code>Resp :: {ok, {uuid(), ParsedRsp}} | {error, any()}</code></dt>




<dd>The APNS response.  See "Parsed Response Property List" below for
more detail.</dd>



**Sample request headers**

```
  [
   {<<":method">>, <<"POST">>},
   {<<":path">>, <<"/3/device/ca6a7fef19bcf22c38d5bee0c29f80d9461b2848061f0f4f0c0d361e4c4f1dc2">>},
   {<<":scheme">>, <<"https">>},
   {<<"apns-topic">>, <<"com.example.FakeApp.voip">>},
   {<<"apns-expiration">>, <<"2147483647">>},
   {<<"apns-id">>, <<"519d99ac-1bb0-42df-8381-e6979ce7cd32">>}
  ]
```


#### <a name="Notification_Property_List">Notification Property List</a> ####

```
   [{uuid, binary()},              % Canonical UUID string
    {expiration, non_neg_integer()},
    {token, binary()},             % Hex string
    {topic, binary()},             % String
    {json, binary()},              % JSON string
    {from, {pid(), Tag :: any()}}, % Caller
    {priority, non_neg_integer()}
    ].
```

**Sample JSON string (formatted)**

```
  {
    "aps": {
      "alert": "Some alert string",
      "content-available": 1
    }
  }
```


#### <a name="Parsed_Response_Property_List">Parsed Response Property List</a> ####

The properties present in the list depend on the status code.

* __Always present__: `uuid`, `status`, `status_desc`.

* __Also present for 4xx, 5xx status__: `reason`, `reason_desc`,
`body`.

* __Also present, but only for 410 status__: `timestamp`,
`timestamp_desc`.


See `apns_lib_http2:parsed_rsp()`.

```
  [
   {uuid, binary()},            % UUID string
   {status, binary()},          % HTTP/2 status string, e.g. <<"200">>.
   {status_desc, binary()},     % Status description string
   {reason, binary()},          % Reason string
   {reason_desc, binary()},     % Reason description
   {timestamp, non_neg_integer()},
   {timestamp_desc, binary()},  % Timestamp description
   {body, term()}               % Parsed APNS response body
  ]
```

**Sample success return**

```
  [
   {uuid,
    <<"d013d454-b1d0-469a-96d3-52e0c5ec4281">>},
   {status,<<"200">>},
   {status_desc,<<"Success">>}
  ]
```

**Sample status 400 return**

```
  [
   {uuid,<<"519d99ac-1bb0-42df-8381-e6979ce7cd32">>},
   {status,<<"400">>},
   {status_desc,<<"Bad request">>},
   {reason,<<"BadDeviceToken">>},
   {reason_desc,<<"The specified device token was bad...">>},
   {body,[{<<"reason">>,<<"BadDeviceToken">>}]}
  ]
```

**Sample status 410 return**

```
  [
   {uuid,<<"7824c0f2-a5e6-4c76-9699-45ac477e64d2">>},
   {status,<<"410">>},
   {status_desc,<<"The device token is no longer active for the topic.">>},
   {reason,<<"Unregistered">>},
   {reason_desc,<<"The device token is inactive for the specified topic.">>},
   {timestamp,1475784832119},
   {timestamp_desc,<<"2016-10-06T20:13:52Z">>},
   {body,[{<<"reason">>,<<"Unregistered">>},
          {<<"timestamp">>,1475784832119}]}
  ]
```


<a name="flush-1"></a>

### flush/1 ###

<pre><code>
flush(FsmRef) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>FsmRef = term()</code></li></ul>

Flush (retransmit) any queued notifications.

<a name="get_state-1"></a>

### get_state/1 ###

`get_state(FsmRef) -> any()`

Get the current state of the FSM.

<a name="get_state_name-1"></a>

### get_state_name/1 ###

`get_state_name(FsmRef) -> any()`

Get the name of the current state of the FSM.

<a name="is_connected-1"></a>

### is_connected/1 ###

`is_connected(FsmRef) -> any()`

Return `true` if the session is connected, `false` otherwise.

<a name="quiesce-1"></a>

### quiesce/1 ###

<pre><code>
quiesce(FsmRef) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>FsmRef = <a href="#type-fsm_ref">fsm_ref()</a></code></li><li><code>Result = ok | {error, term()}</code></li></ul>

Quiesce a session. This put sthe session into a mode
where all subsequent requests are rejected with `{error, quiesced}`.

<a name="reconnect-1"></a>

### reconnect/1 ###

`reconnect(FsmRef) -> any()`

Immediately disconnect the session and reconnect.

<a name="reconnect-2"></a>

### reconnect/2 ###

`reconnect(FsmRef, Delay) -> any()`

Immediately disconnect the session and reconnect after `Delay` ms.

<a name="resume-1"></a>

### resume/1 ###

<pre><code>
resume(FsmRef) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>FsmRef = <a href="#type-fsm_ref">fsm_ref()</a></code></li><li><code>Result = ok | {error, term()}</code></li></ul>

Resume a quiesced session.

__See also:__ [quiesce/1](#quiesce-1).

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(FsmRef, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>FsmRef = <a href="#type-fsm_ref">fsm_ref()</a></code></li><li><code>Opts = <a href="#type-send_opts">send_opts()</a></code></li><li><code>Result = <a href="#type-sync_send_reply">sync_send_reply()</a></code></li></ul>

Send a notification specified by `Nf` with options `Opts`.

<a name="send_cb-3"></a>

### send_cb/3 ###

<pre><code>
send_cb(FsmRef, Opts, Callback) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>FsmRef = <a href="#type-fsm_ref">fsm_ref()</a></code></li><li><code>Opts = <a href="#type-send_opts">send_opts()</a></code></li><li><code>Callback = <a href="#type-send_callback">send_callback()</a></code></li><li><code>Result = <a href="#type-sync_send_reply">sync_send_reply()</a></code></li></ul>

Send a notification specified by `Nf` and a user-supplied callback
function.

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(Name, Opts) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Name = atom()</code></li><li><code>Opts = <a href="#type-options">options()</a></code></li><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>

Start a named session as described by the options `Opts`.  The name
`Name` is registered so that the session can be referenced using the name to
call functions like send/3. Note that this function is only used
for testing; see start_link/2.

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Name, Opts) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Name = atom()</code></li><li><code>Opts = <a href="#type-options">options()</a></code></li><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>

Start a named session as described by the options `Opts`.  The name
`Name` is registered so that the session can be referenced using the name to
call functions like send/3.

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(FsmRef) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>FsmRef = <a href="#type-fsm_ref">fsm_ref()</a></code></li></ul>

Stop session.

<a name="sync_send_callback-3"></a>

### sync_send_callback/3 ###

<pre><code>
sync_send_callback(NfPL, Req, Resp) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>NfPL = [{atom(), term()}]</code></li><li><code>Req = <a href="#type-cb_req">cb_req()</a></code></li><li><code>Resp = <a href="#type-cb_result">cb_result()</a></code></li><li><code>Result = <a href="#type-cb_result">cb_result()</a></code></li></ul>

Standard sync callback function. This is not normally needed
outside of this module.

__See also:__ [send_cb/1](#send_cb-1).


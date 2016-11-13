

# Module gcm_erl_session #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

GCM server session.

Copyright (c) 2015, 2016 Silent Circle

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##

There must be one session per API key and sessions must have unique (i.e.
they are registered) names within the node.


### <a name="Request">Request</a> ###

```
   Nf = [{id, sc_util:to_bin(RegId)},
         {data, [{alert, sc_util:to_bin(Msg)}]}],
   {ok, Res} = gcm_erl_session:send('gcm-com.example.MyApp', Nf).
```

Note that the above notification is semantically identical to

```
   Nf = [{registration_ids, [sc_util:to_bin(RegId)]},
         {data, [{alert, sc_util:to_bin(Msg)]}].
```


#### <a name="DEPRECATED_until_future_enhancement">DEPRECATED until future enhancement</a> ####

It follows that you should be able to send to multiple registration ids,
as shown below, but this functionality is not working correctly yet.
The notification will be sent, but the session will crash (deliberately)
with an internal error.

Consider this future functionality until it is fixed and this notice is
removed.

```
   BRegIds = [sc_util:to_bin(RegId) || RegId <- RegIds],
   Nf = [{registration_ids, BRegIds},
         {data, [{alert, sc_util:to_bin(Msg)}]}
   ],
   Rsps = gcm_erl_session:send('gcm-com.example.MyApp', Nf).
```


#### <a name="JSON">JSON</a> ####

This is an example of the JSON sent to GCM:

```
   {
     "to": "dQMPBffffff:APA91bbeeff...8yC19k7ULYDa9X",
     "priority": "high",
     "collapse_key": "true",
     "data": {"alert": "Some text"}
   }
```


<a name="types"></a>

## Data Types ##




### <a name="type-notification">notification()</a> ###


<pre><code>
notification() = <a href="gcm_json.md#type-notification">gcm_json:notification()</a>
</code></pre>




### <a name="type-opt">opt()</a> ###


<pre><code>
opt() = {uri, string()} | {api_key, binary()} | {restricted_package_name, binary()} | {max_req_ttl, non_neg_integer()} | {max_backoff_secs, non_neg_integer()} | {max_attempts, non_neg_integer()} | {retry_interval, non_neg_integer()} | {ssl_opts, [<a href="ssl.md#type-ssloption">ssl:ssloption()</a>]} | {httpc_opts, list()}
</code></pre>




### <a name="type-start_opts">start_opts()</a> ###


<pre><code>
start_opts() = [<a href="gcm_erl_session.md#type-opt">gcm_erl_session:opt()</a>]
</code></pre>




### <a name="type-uuid">uuid()</a> ###


<pre><code>
uuid() = <a href="/home/efine/work/sc/scpf/_build/default/lib/uuid/doc/uuid.md#type-uuid">uuid:uuid()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_send-2">async_send/2</a></td><td>Equivalent to <a href="#async_send-3"><tt>async_send(SvrRef, Nf, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#async_send-3">async_send/3</a></td><td>Asynchronously send a notification specified by
<code>Nf</code> via <code>SvrRef</code>, with options <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#async_send_callback-3">async_send_callback/3</a></td><td>Standard callback function for an asynchronous send.</td></tr><tr><td valign="top"><a href="#async_send_cb-5">async_send_cb/5</a></td><td>Asynchronously send a notification specified by
<code>Nf</code> via <code>SvrRef</code> with options <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Equivalent to <a href="#send-3"><tt>send(SvrRef, Nf, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#send-3">send/3</a></td><td>Synchronously send a notification specified by <code>Nf</code> via <code>SvrRef</code>, with
options <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Start a named session as described by the <code>StartOpts</code>.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>Start a named session as described by the options <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop session.</td></tr><tr><td valign="top"><a href="#sync_send_callback-3">sync_send_callback/3</a></td><td>Callback function to simulate a synchronous send.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_send-2"></a>

### async_send/2 ###

<pre><code>
async_send(SvrRef, Nf) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>SvrRef = term()</code></li><li><code>Nf = <a href="#type-notification">notification()</a></code></li><li><code>Result = {ok, {submitted, UUID}} | {error, Reason}</code></li><li><code>UUID = <a href="#type-uuid">uuid()</a></code></li><li><code>Reason = term()</code></li></ul>

Equivalent to [`async_send(SvrRef, Nf, [])`](#async_send-3).

<a name="async_send-3"></a>

### async_send/3 ###

<pre><code>
async_send(SvrRef, Nf, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>SvrRef = term()</code></li><li><code>Nf = <a href="#type-notification">notification()</a></code></li><li><code>Opts = list()</code></li><li><code>Result = {ok, {submitted, UUID}} | {error, Reason}</code></li><li><code>UUID = <a href="#type-uuid">uuid()</a></code></li><li><code>Reason = term()</code></li></ul>

Equivalent to [`async_send_cb(SvrRef, Nf, Opts, self(),fun async_send_callback/3)`](#async_send_cb-5).

Asynchronously send a notification specified by
`Nf` via `SvrRef`, with options `Opts`.

The immediate response will be either `{ok, {submitted, UUID}}`, or
`{error, term()}`. `UUID` is either generated for the caller, or is the
value of the property `{uuid, UUID}` if present in the notification
property list.

Note that the UUID must be a binary in standard UUID string format, e.g.
`d611dcf3-bd70-453d-9fdd-94bc66cea7f7`. It is converted internally to
a 128-bit binary on both storage and lookup, so it is case-insensitive.

<a name="async_send_callback-3"></a>

### async_send_callback/3 ###

`async_send_callback(NfPL, Req, Resp) -> any()`

Standard callback function for an asynchronous send. This is
the callback function used by async_send/3.

<a name="async_send_cb-5"></a>

### async_send_cb/5 ###

`async_send_cb(SvrRef, Nf, Opts, ReplyPid, Cb) -> any()`

Asynchronously send a notification specified by
`Nf` via `SvrRef` with options `Opts`. Respond immediately with the status of
the call, and when the call completes asynchronously, run the callback
function `Cb` and send the response `Resp` to `ReplyPid`.


### <a name="Parameters">Parameters</a> ###



<dt><code>Nf</code></dt>




<dd>The notification proplist.</dd>




<dt><code>ReplyPid</code></dt>




<dd>A <code>pid</code> to which asynchronous responses are to be sent.</dd>




<dt><code>Cb</code></dt>




<dd><p>A function to be called when the asynchronous operation is complete.
Its function spec is</p><p></p><pre>    -spec callback(NfPL, Req, Resp) -> any() when
          NfPL :: proplists:proplist(), % Nf proplist
          Req  :: proplists:proplist(), % Request data
          Resp :: {ok, ParsedResp} | {error, term()},
          ParsedResp :: proplists:proplist().</pre>
</dd>



<a name="send-2"></a>

### send/2 ###

<pre><code>
send(SvrRef, Nf) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>SvrRef = term()</code></li><li><code>Nf = <a href="#type-notification">notification()</a></code></li><li><code>Result = {ok, {UUID, Response}} | {error, Reason}</code></li><li><code>UUID = <a href="#type-uuid">uuid()</a></code></li><li><code>Response = term()</code></li><li><code>Reason = term()</code></li></ul>

Equivalent to [`send(SvrRef, Nf, [])`](#send-3).

<a name="send-3"></a>

### send/3 ###

<pre><code>
send(SvrRef, Nf, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>SvrRef = term()</code></li><li><code>Nf = <a href="#type-notification">notification()</a></code></li><li><code>Opts = list()</code></li><li><code>Result = {ok, {UUID, Response}} | {error, Reason}</code></li><li><code>UUID = <a href="#type-uuid">uuid()</a></code></li><li><code>Response = term()</code></li><li><code>Reason = term()</code></li></ul>

Synchronously send a notification specified by `Nf` via `SvrRef`, with
options `Opts`. `SvrRef` can be the session name atom, a pid, or any other
valid `gen_server` destination.


### <a name="Opts">Opts</a> ###



<dt><code>http_headers :: [{string(), string()}]</code></dt>




<dd>Extra HTTP headers to include with a request. These will be merged
with any internally-generated headers, and will override internally
generated headers, so caution is advised. Avoiding the <code>Authorization</code>
header is recommended. Currently only used for testing with the GCM
simulator.</dd>




### <a name="Caveats">Caveats</a> ###

Note that sending a notification synchronously is not recommended, because
the duration of the call is unpredictable. The call may time out, leaving the
status of the notification in doubt. Timeouts can occur for a number of
reasons, such as the need for this session to retry sending the notification
to GCM.

It is better to use the asynchronous interface and handle the responses
sent to the mailbox of the calling process, or provide a user-defined
callback function. The callback function is spawned into its own process.


### <a name="More_information">More information</a> ###

For JSON format and other information, see [ GCM
Connection Server Reference](https://developers.google.com/cloud-messaging/http-server-ref).

__See also:__ [async_send/3](#async_send-3), [gcm_json:make_notification/1](gcm_json.md#make_notification-1), [gcm_json:notification/0](gcm_json.md#notification-0).

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(Name::atom(), StartOpts::<a href="#type-start_opts">start_opts()</a>) -&gt; term()
</code></pre>
<br />

Start a named session as described by the `StartOpts`.
`Name` is registered so that the session can be referenced using
the name to call functions like [`send/2`](#send-2).  Note that this
function is only used for testing.

* For `ssl_opts` see ssl:ssloptions/0 in ssl:connect/2.

* For `httpc_opts`, see httpc:set_options/1.


__See also:__ [start_link/2](#start_link-2).

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Name::atom(), Opts::<a href="#type-start_opts">start_opts()</a>) -&gt; term()
</code></pre>
<br />

Start a named session as described by the options `Opts`.  The name
`Name` is registered so that the session can be referenced using
the name to call functions like [`send/2`](#send-2).


### <a name="Parameters">Parameters</a> ###


* `Name` - Session name (atom)

* `Opts` - Options



<dt><code>{api_key, binary()}</code></dt>




<dd>Google API Key, e.g.
<code><<"AIzafffffffffffffffffffffffffffffffffaA">></code></dd>




<dt><code>{max_attempts, pos_integer()|infinity}</code></dt>




<dd>The maximum number of times to attempt to send the
message when receiving a 5xx error.</dd>




<dt><code>{retry_interval, pos_integer()}</code></dt>




<dd>The initial number of seconds to wait before reattempting to
send the message.</dd>




<dt><code>{max_req_ttl, pos_integer()}</code></dt>




<dd>The maximum time in seconds for which this request
will live before being considered undeliverable and
stopping with an error.</dd>




<dt><code>{max_backoff_secs, pos_integer()}</code></dt>




<dd>The maximum backoff time in seconds for this request.
This limits the exponential backoff to a maximum
value.</dd>




<dt><code>{restricted_package_name, binary()}</code></dt>




<dd>A string containing the package name of your
application. When set, messages will only be sent to
registration IDs that match the package name.
Optional.</dd>




<dt><code>{uri, string()}</code></dt>




<dd>GCM URI, defaults to
<code>https://gcm-http.googleapis.com/gcm/send</code>. Optional.</dd>




<dt><code>{collapse_key, string()}</code></dt>




<dd>Arbitrary string use to collapse a group of like
messages into a single message when the device is offline.
Optional.</dd>






<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(SvrRef::term()) -&gt; term()
</code></pre>
<br />

Stop session.

<a name="sync_send_callback-3"></a>

### sync_send_callback/3 ###

`sync_send_callback(NfPL, Req, Resp) -> any()`

Callback function to simulate a synchronous send. This is the
callback function used by send/3.


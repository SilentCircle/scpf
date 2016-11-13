

# Module sc_push_svc_gcm #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Google Cloud Messaging Push Notification Service (gcm) API.

Copyright (c) 2015 Silent Circle

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##

This is the API to the Google Cloud Messaging Service Provider. It
runs as a supervisor, so should be started either from an application
module or a supervisor.


### <a name="Session_Configuration">Session Configuration</a> ###

Start the API by calling start_link/1 and providing a list of
sessions to start. Each session is a proplist as shown below:

```
   [
       {name, 'gcm-com.example.MyApp'},
       {config, [
               {api_key, <<"AIzaffffffffffffffffffffffffffffffff73w">>},
               {ssl_opts, [{verify, verify_none}]},
               {uri, "https://gcm-http.googleapis.com/gcm/send"},
               {max_attempts, 5},
               {retry_interval, 1},
               {max_req_ttl, 600}
           ]}
   ]
```

Note that the session _must_ be named `gcm-{AppId}`,
where `{AppId}` is the Android App ID, such as
`com.example.MyApp`.


### <a name="Sending_an_alert_via_the_sc_push_API">Sending an alert via the sc_push API</a> ###

At the top level, alerts are sent via sc_push. Here is an example of
an alert that directly specifies the receivers (only one in this case)
and some gcm-specific options:

```
   Alert = <<"Hello, Android!">>,
   RegId = <<"dQMPBffffff:APA91bbeeff...8yC19k7ULYDa9X">>,
   AppId = <<"com.example.MyApp">>,
   Nf = [{alert, Alert},
         {receivers,[{svc_appid_tok, [{gcm, AppId, RegId}]}]},
         {gcm, [{priority, <<"high">>}, {collapse_key, <<"true">>}]}],
   [{ok, Ref}] = gcm_erl:send(Nf).
```

Note that the same alert may be sent to multiple receivers, in which
case there will be multiple return values (one per notification sent):

```
   Alert = <<"Hello, Android!">>,
   RegId1 = <<"dQMPBffffff:APA91bbeeff...8yC19k7ULYDa9X">>,
   RegId2 = <<"dQMPBeeeeee:APA91dddddd...8yCefefefefefe">>,
   AppId = <<"com.example.MyApp">>,
   Receivers = [{svc_appid_tok,[{gcm, AppId, RegId1},{gcm, AppId, RegId2}]}],
   Nf = [{alert,<<"Please register">>},
         {receivers, Receivers},
         {gcm, [{priority, <<"high">>}, {collapse_key, <<"true">>}]}],
   [{ok, Ref,}, {ok, Ref2}] = gcm_erl:send(Nf).
   In the same way, an alert may be sent to receivers on different services
   and using different receiver types, such as `tag'.
   === Sending an alert via the gcm_erl API ===
   ```
   Alert = <<"Hello, Android!">>,
   {ok, SeqNo} = gcm_erl:send('gcm-com.example.MyApp', Nf).
```


#### <a name="Sending_an_alert_via_a_session">Sending an alert via a session</a> ####

```
   Nf = [{id, sc_util:to_bin(RegId)},
         {data, [{alert, sc_util:to_bin(Msg)}]}],
   {ok, SeqNo} = gcm_erl_session:send('gcm-com.example.MyApp', Nf).
```

Note that the above notification is semantically identical to

```
   Nf = [{registration_ids, [sc_util:to_bin(RegId)]},
         {data, [{alert, sc_util:to_bin(Msg)]}].
```

It follows that you can send to multiple registration ids:

```
   BRegIds = [sc_util:to_bin(RegId) || RegId <- RegIds],
   Nf = [{registration_ids, BRegIds},
         {data, [{alert, sc_util:to_bin(Msg)}]}],
   Rsps = gcm_erl_session:send('gcm-com.example.MyApp', Nf).
```


<a name="types"></a>

## Data Types ##




### <a name="type-async_send_result">async_send_result()</a> ###


<pre><code>
async_send_result() = {ok, {submitted, <a href="#type-uuid">uuid()</a>}} | {error, term()}
</code></pre>




### <a name="type-callback">callback()</a> ###


<pre><code>
callback() = fun((<a href="#type-notification">notification()</a>, <a href="#type-cb_req">cb_req()</a>, <a href="#type-cb_resp">cb_resp()</a>) -&gt; any())
</code></pre>




### <a name="type-cb_req">cb_req()</a> ###


<pre><code>
cb_req() = <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>




### <a name="type-cb_resp">cb_resp()</a> ###


<pre><code>
cb_resp() = {ok, term()} | {error, term()}
</code></pre>




### <a name="type-notification">notification()</a> ###


<pre><code>
notification() = <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>




### <a name="type-uuid">uuid()</a> ###


<pre><code>
uuid() = <a href="/home/efine/work/sc/scpf/_build/default/lib/uuid/doc/uuid.md#type-uuid_str">uuid:uuid_str()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_send-2">async_send/2</a></td><td>Asynchronously send a notification specified by proplist
<code>Nf</code>; Same as <a href="#send-2"><code>send/2</code></a> beside returning only 'ok' on success.</td></tr><tr><td valign="top"><a href="#async_send-3">async_send/3</a></td><td>Asynchronously send a notification specified by proplist
<code>Nf</code> with options <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#async_send_cb-5">async_send_cb/5</a></td><td>Asynchronously send a notification specified by proplist
<code>Nf</code></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_alert-1">normalize_alert/1</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_data-1">normalize_data/1</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_id-1">normalize_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_reg_ids-1">normalize_reg_ids/1</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_token-1">normalize_token/1</a></td><td></td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send a notification specified by proplist <code>Nf</code></td></tr><tr><td valign="top"><a href="#send-3">send/3</a></td><td>Send a notification specified by proplist <code>Nf</code> and
with options <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td><code>Opts</code> is a list of proplists.</td></tr><tr><td valign="top"><a href="#start_session-2">start_session/2</a></td><td>Start named session for specific host and certificate as
described in the options proplist <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#stop_session-1">stop_session/1</a></td><td>Stop named session.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_send-2"></a>

### async_send/2 ###

`async_send(Name, Nf) -> any()`

Asynchronously send a notification specified by proplist
`Nf`; Same as [`send/2`](#send-2) beside returning only 'ok' on success.

<a name="async_send-3"></a>

### async_send/3 ###

`async_send(Name, Nf, Opts) -> any()`

Asynchronously send a notification specified by proplist
`Nf` with options `Opts`. `Opts` is currently unused.

<a name="async_send_cb-5"></a>

### async_send_cb/5 ###

<pre><code>
async_send_cb(Name, Nf, Opts, ReplyPid, Cb) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = term()</code></li><li><code>Nf = <a href="#type-notification">notification()</a></code></li><li><code>Opts = list()</code></li><li><code>ReplyPid = pid()</code></li><li><code>Cb = <a href="#type-callback">callback()</a></code></li><li><code>Result = <a href="#type-async_send_result">async_send_result()</a></code></li></ul>

Asynchronously send a notification specified by proplist
`Nf`

<a name="init-1"></a>

### init/1 ###

`init(Opts) -> any()`

<a name="normalize_alert-1"></a>

### normalize_alert/1 ###

`normalize_alert(Nf) -> any()`

<a name="normalize_data-1"></a>

### normalize_data/1 ###

`normalize_data(Nf) -> any()`

<a name="normalize_id-1"></a>

### normalize_id/1 ###

`normalize_id(Nf) -> any()`

<a name="normalize_reg_ids-1"></a>

### normalize_reg_ids/1 ###

`normalize_reg_ids(Nf) -> any()`

<a name="normalize_token-1"></a>

### normalize_token/1 ###

`normalize_token(Nf) -> any()`

<a name="send-2"></a>

### send/2 ###

`send(Name, Nf) -> any()`

Send a notification specified by proplist `Nf`

<a name="send-3"></a>

### send/3 ###

`send(Name, Nf, Opts) -> any()`

Send a notification specified by proplist `Nf` and
with options `Opts`.

__See also:__ [gcm_erl_session:send/3](gcm_erl_session.md#send-3).

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Opts::list()) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

`Opts` is a list of proplists.
Each proplist is a session definition containing
`name` and `config` keys.

<a name="start_session-2"></a>

### start_session/2 ###

<pre><code>
start_session(Name::atom(), Opts::list()) -&gt; {ok, pid()} | {error, already_started} | {error, Reason::term()}
</code></pre>
<br />

Start named session for specific host and certificate as
described in the options proplist `Opts`.

__See also:__ [__Options__ in `gcm_erl_session`](gcm_erl_session.md#start_session-2).

<a name="stop_session-1"></a>

### stop_session/1 ###

<pre><code>
stop_session(Name::atom()) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

Stop named session.


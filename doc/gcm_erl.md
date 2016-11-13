

# Module gcm_erl #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Google Cloud Messaging (GCM) API.

Copyright (c) 2015 Silent Circle

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##

This is the API to the GCM Service Provider.


### <a name="Synopsis">Synopsis</a> ###

In the example below, all optional values are shown with their
defaults if omitted.


#### <a name="Starting_a_session">Starting a session</a> ####

```
   Opts = [
               %% Required GCM API key
               {api_key, <<"ahsgdfjkjkjfdk">>},
               %% Required, even if empty list. Defaults shown.
               {ssl_opts, [
                   {verify, verify_peer},
                   {reuse_sessions, true}
               ]},
               %% Optional, defaults as shown.
               {uri, "https://gcm-http.googleapis.com/gcm/send"},
               %% Optional, omitted if missing.
               {restricted_package_name, <<"my-android-pkg">>},
               %% Maximum times to try to send and then give up.
               {max_attempts, 10},
               %% Starting point in seconds for exponential backoff.
               %% Optional.
               {retry_interval, 1},
               %% Maximum seconds for a request to live in a retrying state.
               {max_req_ttl, 3600},
               %% Reserved for future use
               {failure_action, fun(_)}
           ],
   {ok, Pid} = gcm_erl:start_session('gcm-com.example.MyApp', Opts).
```


#### <a name="Sending_an_alert_via_the_API">Sending an alert via the API</a> ####

```
   RegId = <<"e7b300...a67b">>, % From earlier Android registration
   Opts = [
       {id, RegId},
       {collapse_key, <<"New Mail">>},
       {data, [{msg, <<"You have new mail">>}]}
   ],
   {ok, Result} = gcm_erl:send('gcm-com.example.MyApp', Opts),
   {UUID, Props} = Result.
```


#### <a name="Sending_an_alert_via_a_session_(for_testing_only)">Sending an alert via a session (for testing only)</a> ####

```
   {ok, Result} = gcm_erl_session:send('gcm-com.example.MyApp',
                                                  Opts),
   {UUID, Props} = Result.
```


#### <a name="Stopping_a_session">Stopping a session</a> ####

```
   ok = gcm_erl:stop_session('gcm-com.example.MyApp').
```

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_send-2">async_send/2</a></td><td>Asynchronously send a notification specified by proplist
<code>Notification</code> to <code>SvrRef</code>.</td></tr><tr><td valign="top"><a href="#async_send-3">async_send/3</a></td><td>Asynchronously send a notification specified by proplist
<code>Notification</code> to <code>SvrRef</code> with options <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send a notification specified by proplist <code>Notification</code>
to <code>SvrRef</code>.</td></tr><tr><td valign="top"><a href="#send-3">send/3</a></td><td>Send a notification specified by proplist <code>Notification</code>
to <code>SvrRef</code> with options <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#start_session-2">start_session/2</a></td><td>
Start a named session.</td></tr><tr><td valign="top"><a href="#stop_session-1">stop_session/1</a></td><td>Stop named session.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_send-2"></a>

### async_send/2 ###

<pre><code>
async_send(SvrRef, Notification) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>SvrRef = term()</code></li><li><code>Notification = <a href="gcm_json.md#type-notification">gcm_json:notification()</a></code></li><li><code>Result = {ok, {submitted, Reply}} | {error, Reason}</code></li><li><code>Reply = term()</code></li><li><code>Reason = term()</code></li></ul>

Asynchronously send a notification specified by proplist
`Notification` to `SvrRef`.

__See also:__ [async_send/3](#async_send-3), [gcm_erl_session:async_send/2](gcm_erl_session.md#async_send-2).

<a name="async_send-3"></a>

### async_send/3 ###

<pre><code>
async_send(SvrRef, Notification, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>SvrRef = term()</code></li><li><code>Notification = <a href="gcm_json.md#type-notification">gcm_json:notification()</a></code></li><li><code>Opts = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Result = {ok, {submitted, Reply}} | {error, Reason}</code></li><li><code>Reply = term()</code></li><li><code>Reason = term()</code></li></ul>

Asynchronously send a notification specified by proplist
`Notification` to `SvrRef` with options `Opts`.

__See also:__ [send/3](#send-3), [gcm_erl_session:async_send/3](gcm_erl_session.md#async_send-3).

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(SvrRef, Notification) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>SvrRef = term()</code></li><li><code>Notification = <a href="gcm_json.md#type-notification">gcm_json:notification()</a></code></li><li><code>Result = {ok, Reply} | {error, Reason}</code></li><li><code>Reply = term()</code></li><li><code>Reason = term()</code></li></ul>

Send a notification specified by proplist `Notification`
to `SvrRef`.

__See also:__ [send/3](#send-3), [gcm_erl_session:send/2](gcm_erl_session.md#send-2).

<a name="send-3"></a>

### send/3 ###

<pre><code>
send(SvrRef, Notification, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>SvrRef = term()</code></li><li><code>Notification = <a href="gcm_json.md#type-notification">gcm_json:notification()</a></code></li><li><code>Opts = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Result = {ok, Reply} | {error, Reason}</code></li><li><code>Reply = term()</code></li><li><code>Reason = term()</code></li></ul>

Send a notification specified by proplist `Notification`
to `SvrRef` with options `Opts`. `Opts` currently only supports
`{http_headers, [{string(), string()}]}` to provide extra headers.

Note that `SvrRef` may be the registered name or `{Name, Node}`,
where `Node` is an Erlang node on which the registered process
called `Name` is running.


#### <a name="Example">Example</a> ####

```
  Name = 'gcm-com.example.MyApp', % Note: atom() !
  Notification = [
     %% Required, all others optional
     {id, <<"abc">>},
     {collapse_key, <<"Something">>},
     {priority, <<"high">>},
     {content_available, true},
     {data, []},
     {delay_while_idle, false},
     {time_to_live, 3600},
     {restricted_package_name, <<"foo_pkg>>},
     {dry_run, false}
  ],
  gcm_erl:send(Name, Notification, []),
  gcm_erl:send({Name, node()}, Notification, []).
```

__See also:__ [gcm_erl_session:send/3](gcm_erl_session.md#send-3).

<a name="start_session-2"></a>

### start_session/2 ###

<pre><code>
start_session(Name::atom(), Opts::<a href="gcm_erl_session.md#type-start_opts">gcm_erl_session:start_opts()</a>) -&gt; {ok, pid()} | {error, already_started} | {error, Reason::term()}
</code></pre>
<br />

Start a named session.

__See also:__ [gcm_erl_session:start_link/2](gcm_erl_session.md#start_link-2).

<a name="stop_session-1"></a>

### stop_session/1 ###

<pre><code>
stop_session(Name::atom()) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

Stop named session.




# Module sc_push #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module is the Erlang API for Silent Circle push notifications
and registrations.

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##


### <a name="Push_Notification_Format">Push Notification Format</a> ###

The notification format is very flexible. There is a simple format
and a more elaborate, canonical format.

The simple format requires push tokens
to have been registered using the registration API, so that the
push tag can be used as a destination for thte notification.

The canonical format requires a list of receivers to be provided for
each notification. This allows a notification to be sent to multiple
recipients on different services, specifying the destination in a
number of possible formats.


#### <a name="Simple_notification_format">Simple notification format</a> ####

```
   [{alert, <<"Hello there.">>},
    {tag, <<"foo@example.com">>}]
```


#### <a name="Canonical_notification_format">Canonical notification format</a> ####

The canonical format is as follows. Any or all of the receiver types may be specified,
but at least one, for example `svc_tok`, **must** be provided.

```
   [{alert, <<"Hello there.">>},
    {receivers, [{tag, [Tag]},
                 {svc_tok, [{Service, Token}]},
                 {svc_appid_tok, [{Service, AppId, Token}]},
                 {device_id, [DeviceId]}]}]
```

More formally, the notification specification is defined in
[`notification()`](#type-notification).


#### <a name="Example">Example</a> ####

```
   AppId = <<"com.example.MyApp">>,
   Tok = <<"4df5676af4307b3e0366da63e8854752d9219d8b9848f7c31bbab8741730fda6">>,
   [{alert,<<"Hello">>},
    {aps,[{badge,22}]},
    {receivers,[{svc_appid_tok,[{apns, AppId, Tok}]}]}]
```

<a name="types"></a>

## Data Types ##




### <a name="type-alert">alert()</a> ###


<pre><code>
alert() = binary()
</code></pre>

Alert message as binary string.



### <a name="type-alert_spec">alert_spec()</a> ###


<pre><code>
alert_spec() = {alert, <a href="#type-alert">alert()</a>}
</code></pre>




### <a name="type-app_id">app_id()</a> ###


<pre><code>
app_id() = binary()
</code></pre>

AppId as binary string, e.g. `<<"com.foo.app">>`.



### <a name="type-async_send_result">async_send_result()</a> ###


<pre><code>
async_send_result() = {ok, {<a href="#type-async_status">async_status()</a>, <a href="#type-uuid">uuid()</a>}} | {error, term()}
</code></pre>




### <a name="type-async_send_results">async_send_results()</a> ###


<pre><code>
async_send_results() = [<a href="#type-async_send_result">async_send_result()</a>]
</code></pre>




### <a name="type-async_status">async_status()</a> ###


<pre><code>
async_status() = submitted | queued
</code></pre>




### <a name="type-child_id">child_id()</a> ###


<pre><code>
child_id() = term()
</code></pre>

Not a pid().



### <a name="type-device_id">device_id()</a> ###


<pre><code>
device_id() = binary()
</code></pre>




### <a name="type-device_id_spec">device_id_spec()</a> ###


<pre><code>
device_id_spec() = {device_id, <a href="#type-device_ids">device_ids()</a>}
</code></pre>




### <a name="type-device_ids">device_ids()</a> ###


<pre><code>
device_ids() = [<a href="#type-device_id">device_id()</a>]
</code></pre>




### <a name="type-dist">dist()</a> ###


<pre><code>
dist() = binary()
</code></pre>

Distribution, e.g. `<<"prod">>`, `<<"dev">>`.



### <a name="type-filtered_result">filtered_result()</a> ###


<pre><code>
filtered_result() = {<a href="#type-reg_info_list">reg_info_list()</a>, <a href="#type-reg_err_list">reg_err_list()</a>}
</code></pre>




### <a name="type-mode">mode()</a> ###


<pre><code>
mode() = sync | async
</code></pre>




### <a name="type-notification">notification()</a> ###


<pre><code>
notification() = [<a href="#type-alert_spec">alert_spec()</a> | <a href="#type-tag_spec">tag_spec()</a> | <a href="#type-service_specific_spec">service_specific_spec()</a> | <a href="#type-receivers">receivers()</a>]
</code></pre>




### <a name="type-props">props()</a> ###


<pre><code>
props() = <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>




### <a name="type-receiver_regs">receiver_regs()</a> ###


<pre><code>
receiver_regs() = <a href="#type-filtered_result">filtered_result()</a>
</code></pre>




### <a name="type-receiver_spec">receiver_spec()</a> ###


<pre><code>
receiver_spec() = <a href="#type-tag_spec_mult">tag_spec_mult()</a> | <a href="#type-svc_tok_spec">svc_tok_spec()</a> | <a href="#type-svc_appid_tok_spec">svc_appid_tok_spec()</a> | <a href="#type-device_id_spec">device_id_spec()</a>
</code></pre>




### <a name="type-receiver_specs">receiver_specs()</a> ###


<pre><code>
receiver_specs() = [<a href="#type-receiver_spec">receiver_spec()</a>]
</code></pre>




### <a name="type-receivers">receivers()</a> ###


<pre><code>
receivers() = {receivers, <a href="#type-receiver_specs">receiver_specs()</a>}
</code></pre>




### <a name="type-reg_api_func_name">reg_api_func_name()</a> ###


<pre><code>
reg_api_func_name() = get_registration_info_by_device_id | get_registration_info_by_svc_tok | get_registration_info_by_tag
</code></pre>




### <a name="type-reg_err">reg_err()</a> ###


<pre><code>
reg_err() = {error, <a href="#type-reg_err_term">reg_err_term()</a>}
</code></pre>




### <a name="type-reg_err_list">reg_err_list()</a> ###


<pre><code>
reg_err_list() = [<a href="#type-reg_err_term">reg_err_term()</a>]
</code></pre>




### <a name="type-reg_err_term">reg_err_term()</a> ###


<pre><code>
reg_err_term() = term()
</code></pre>




### <a name="type-reg_id_keys">reg_id_keys()</a> ###


<pre><code>
reg_id_keys() = [<a href="sc_push_reg_db.md#type-reg_id_key">sc_push_reg_db:reg_id_key()</a>]
</code></pre>




### <a name="type-reg_info">reg_info()</a> ###


<pre><code>
reg_info() = [<a href="#type-reg_info_prop">reg_info_prop()</a>]
</code></pre>




### <a name="type-reg_info_list">reg_info_list()</a> ###


<pre><code>
reg_info_list() = [<a href="#type-reg_info">reg_info()</a>]
</code></pre>




### <a name="type-reg_info_prop">reg_info_prop()</a> ###


<pre><code>
reg_info_prop() = {service, <a href="#type-service">service()</a>} | {token, <a href="#type-token">token()</a>} | {app_id, <a href="#type-app_id">app_id()</a>} | {dist, <a href="#type-dist">dist()</a>}
</code></pre>




### <a name="type-reg_ok">reg_ok()</a> ###


<pre><code>
reg_ok() = {ok, <a href="#type-reg_info_list">reg_info_list()</a>}
</code></pre>




### <a name="type-reg_result">reg_result()</a> ###


<pre><code>
reg_result() = <a href="#type-reg_ok">reg_ok()</a> | <a href="#type-reg_err">reg_err()</a>
</code></pre>




### <a name="type-reg_results">reg_results()</a> ###


<pre><code>
reg_results() = [<a href="#type-reg_result">reg_result()</a>]
</code></pre>




### <a name="type-send_result">send_result()</a> ###


<pre><code>
send_result() = <a href="#type-sync_send_result">sync_send_result()</a> | <a href="#type-async_send_result">async_send_result()</a>
</code></pre>




### <a name="type-service">service()</a> ###


<pre><code>
service() = atom()
</code></pre>

Service type, e.g. apns, gcm



### <a name="type-service_specific_spec">service_specific_spec()</a> ###


<pre><code>
service_specific_spec() = {<a href="#type-service">service()</a>, <a href="#type-std_proplist">std_proplist()</a>}
</code></pre>




### <a name="type-std_proplist">std_proplist()</a> ###


<pre><code>
std_proplist() = <a href="sc_types.md#type-proplist">sc_types:proplist</a>(atom(), term())
</code></pre>




### <a name="type-svc_appid_tok">svc_appid_tok()</a> ###


<pre><code>
svc_appid_tok() = {<a href="#type-service">service()</a>, <a href="#type-app_id">app_id()</a>, <a href="#type-token">token()</a>}
</code></pre>




### <a name="type-svc_appid_tok_spec">svc_appid_tok_spec()</a> ###


<pre><code>
svc_appid_tok_spec() = {svc_appid_tok, <a href="#type-svc_appid_toks">svc_appid_toks()</a>}
</code></pre>




### <a name="type-svc_appid_toks">svc_appid_toks()</a> ###


<pre><code>
svc_appid_toks() = [<a href="#type-svc_appid_tok">svc_appid_tok()</a>]
</code></pre>




### <a name="type-svc_tok">svc_tok()</a> ###


<pre><code>
svc_tok() = {<a href="#type-service">service()</a>, <a href="#type-token">token()</a>}
</code></pre>




### <a name="type-svc_tok_spec">svc_tok_spec()</a> ###


<pre><code>
svc_tok_spec() = {svc_tok, <a href="#type-svc_toks">svc_toks()</a>}
</code></pre>




### <a name="type-svc_toks">svc_toks()</a> ###


<pre><code>
svc_toks() = [<a href="#type-svc_tok">svc_tok()</a>]
</code></pre>




### <a name="type-sync_send_result">sync_send_result()</a> ###


<pre><code>
sync_send_result() = {ok, {<a href="#type-uuid">uuid()</a>, <a href="#type-props">props()</a>}} | {error, term()}
</code></pre>




### <a name="type-sync_send_results">sync_send_results()</a> ###


<pre><code>
sync_send_results() = [<a href="#type-sync_send_result">sync_send_result()</a>]
</code></pre>




### <a name="type-tag">tag()</a> ###


<pre><code>
tag() = binary()
</code></pre>

Opaque group identifier.



### <a name="type-tag_spec">tag_spec()</a> ###


<pre><code>
tag_spec() = {tag, <a href="#type-tag">tag()</a>}
</code></pre>




### <a name="type-tag_spec_mult">tag_spec_mult()</a> ###


<pre><code>
tag_spec_mult() = {tag, <a href="#type-tags">tags()</a>}
</code></pre>




### <a name="type-tags">tags()</a> ###


<pre><code>
tags() = [<a href="#type-tag">tag()</a>]
</code></pre>




### <a name="type-token">token()</a> ###


<pre><code>
token() = binary()
</code></pre>

APNS token as hex string, GCM registration id.



### <a name="type-uuid">uuid()</a> ###


<pre><code>
uuid() = binary()
</code></pre>

A raw uuid in the form `<<_:128>>`.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_send-1">async_send/1</a></td><td>Asynchronously sends a notification specified by proplist
<code>Notification</code>.</td></tr><tr><td valign="top"><a href="#async_send-2">async_send/2</a></td><td></td></tr><tr><td valign="top"><a href="#deregister_device_ids-1">deregister_device_ids/1</a></td><td>Deregister multiple registered device IDs.</td></tr><tr><td valign="top"><a href="#deregister_id-1">deregister_id/1</a></td><td>Deregister a registered ID.</td></tr><tr><td valign="top"><a href="#deregister_ids-1">deregister_ids/1</a></td><td>Deregister multiple registered IDs.</td></tr><tr><td valign="top"><a href="#get_all_service_configs-0">get_all_service_configs/0</a></td><td>Get all service configurations.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_id-1">get_registration_info_by_id/1</a></td><td>Get registration info corresponding to a device ID.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_svc_tok-2">get_registration_info_by_svc_tok/2</a></td><td>Get registration info corresponding to service and reg id.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_tag-1">get_registration_info_by_tag/1</a></td><td>Get registration info corresponding to a tag.</td></tr><tr><td valign="top"><a href="#get_service_config-1">get_service_config/1</a></td><td>Get service configuration.</td></tr><tr><td valign="top"><a href="#get_session_pid-1">get_session_pid/1</a></td><td>Get pid of named session.</td></tr><tr><td valign="top"><a href="#make_service_child_spec-1">make_service_child_spec/1</a></td><td>Make a supervisor child spec for a service.</td></tr><tr><td valign="top"><a href="#quiesce_all_services-0">quiesce_all_services/0</a></td><td>Quiesce all services.</td></tr><tr><td valign="top"><a href="#quiesce_service-1">quiesce_service/1</a></td><td>Quiesce a service.</td></tr><tr><td valign="top"><a href="#quiesce_session-2">quiesce_session/2</a></td><td>Quiesce session.</td></tr><tr><td valign="top"><a href="#register_id-1">register_id/1</a></td><td>
Register to receive push notifications.</td></tr><tr><td valign="top"><a href="#register_ids-1">register_ids/1</a></td><td>Perform multiple registrations.</td></tr><tr><td valign="top"><a href="#register_service-1">register_service/1</a></td><td>Register a service in the service configuration registry.</td></tr><tr><td valign="top"><a href="#send-1">send/1</a></td><td>Send a notification specified by proplist <code>Notification</code>.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_service-1">start_service/1</a></td><td>Start a push service.</td></tr><tr><td valign="top"><a href="#start_session-2">start_session/2</a></td><td>Start named session as described in the options proplist <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#stop_service-1">stop_service/1</a></td><td>Stops a service and all sessions for that service.</td></tr><tr><td valign="top"><a href="#stop_session-2">stop_session/2</a></td><td>Stop named session.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_send-1"></a>

### async_send/1 ###

<pre><code>
async_send(Notification) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Notification = <a href="#type-notification">notification()</a></code></li><li><code>Result = <a href="#type-async_send_results">async_send_results()</a></code></li></ul>

Asynchronously sends a notification specified by proplist
`Notification`.  The contents of the proplist differ depending on the push
service used (see [`notification()`](#type-notification)).

The asynchronous response is sent to the calling pid's mailbox as a tuple.
The tuple is defined as`async_message()` as shown below.

```
  -type svc_response_id() :: apns_response | gcm_response | atom().
  -type version() :: atom(). % For example, `` 'v1' ''.
  -type async_message() :: {svc_response_id(), version(), gen_send_result()}.
```

<a name="async_send-2"></a>

### async_send/2 ###

<pre><code>
async_send(Notification, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Notification = <a href="#type-notification">notification()</a></code></li><li><code>Opts = <a href="#type-std_proplist">std_proplist()</a></code></li><li><code>Result = <a href="#type-async_send_results">async_send_results()</a></code></li></ul>

<a name="deregister_device_ids-1"></a>

### deregister_device_ids/1 ###

<pre><code>
deregister_device_ids(IDs::[binary()]) -&gt; ok | {error, term()}
</code></pre>
<br />

Deregister multiple registered device IDs.

<a name="deregister_id-1"></a>

### deregister_id/1 ###

<pre><code>
deregister_id(ID::<a href="sc_push_reg_db.md#type-reg_id_key">sc_push_reg_db:reg_id_key()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Deregister a registered ID.

<a name="deregister_ids-1"></a>

### deregister_ids/1 ###

<pre><code>
deregister_ids(IDs::<a href="#type-reg_id_keys">reg_id_keys()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Deregister multiple registered IDs.

<a name="get_all_service_configs-0"></a>

### get_all_service_configs/0 ###

<pre><code>
get_all_service_configs() -&gt; SvcConfigs
</code></pre>

<ul class="definitions"><li><code>SvcConfigs = [<a href="#type-std_proplist">std_proplist()</a>]</code></li></ul>

Get all service configurations

<a name="get_registration_info_by_id-1"></a>

### get_registration_info_by_id/1 ###

<pre><code>
get_registration_info_by_id(ID::<a href="sc_push_reg_db.md#type-reg_id_key">sc_push_reg_db:reg_id_key()</a>) -&gt; notfound | [<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>]
</code></pre>
<br />

Get registration info corresponding to a device ID. Note that
multiple results should not be returned, although the returned
value is a list for consistency with the other APIs.

<a name="get_registration_info_by_svc_tok-2"></a>

### get_registration_info_by_svc_tok/2 ###

<pre><code>
get_registration_info_by_svc_tok(Service::string() | atom(), RegId::binary() | string()) -&gt; notfound | <a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>
</code></pre>
<br />

Get registration info corresponding to service and reg id.

<a name="get_registration_info_by_tag-1"></a>

### get_registration_info_by_tag/1 ###

<pre><code>
get_registration_info_by_tag(Tag::binary()) -&gt; notfound | [<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>]
</code></pre>
<br />

Get registration info corresponding to a tag. Note that multiple
results may be returned if multiple registrations are found for the
same tag.

<a name="get_service_config-1"></a>

### get_service_config/1 ###

<pre><code>
get_service_config(Service) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Service = term()</code></li><li><code>Result = {ok, <a href="#type-std_proplist">std_proplist()</a>} | {error, term()}</code></li></ul>

Get service configuration

__See also:__ [start_service/1](#start_service-1).

<a name="get_session_pid-1"></a>

### get_session_pid/1 ###

<pre><code>
get_session_pid(Name) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = atom()</code></li><li><code>Result = pid() | undefined</code></li></ul>

Get pid of named session.

<a name="make_service_child_spec-1"></a>

### make_service_child_spec/1 ###

<pre><code>
make_service_child_spec(Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Opts = <a href="#type-std_proplist">std_proplist()</a></code></li><li><code>Result = <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a></code></li></ul>

Make a supervisor child spec for a service. The name of the
supervisor module is obtained from the API module, and is what will
be started. Obviously, it must be a supervisor.

__See also:__ [start_service/1](#start_service-1).

<a name="quiesce_all_services-0"></a>

### quiesce_all_services/0 ###

<pre><code>
quiesce_all_services() -&gt; ok
</code></pre>
<br />

Quiesce all services.

__See also:__ [quiesce_service/1](#quiesce_service-1).

<a name="quiesce_service-1"></a>

### quiesce_service/1 ###

<pre><code>
quiesce_service(Service) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Service = atom() | pid()</code></li><li><code>Result = ok | {error, term()}</code></li></ul>

Quiesce a service. This instructs the service not to accept any more
service requests to allow pending requests to drain.  This call does not
return until all pending service requests have completed.

<a name="quiesce_session-2"></a>

### quiesce_session/2 ###

<pre><code>
quiesce_session(Service, Name) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Service = atom()</code></li><li><code>Name = atom()</code></li><li><code>Result = ok | {error, Reason}</code></li><li><code>Reason = term()</code></li></ul>

Quiesce session. This puts the session in a state that rejects new
push requests, but continues to service in-flight requests.  Once there are
no longer any in-flight requests, the session is stopped.

<a name="register_id-1"></a>

### register_id/1 ###

<pre><code>
register_id(Props::<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>) -&gt; <a href="sc_types.md#type-reg_result">sc_types:reg_result()</a>
</code></pre>
<br />

Register to receive push notifications.

`Props` is a proplist with the following elements, all of which are
required:



<dt><code>{service, string()}</code></dt>




<dd>A name that denotes the push service for which the
caller is registering. Currently this includes <code>apns</code> for
Apple Push, and <code>gcm</code> for Android (Google Cloud Messaging) push.
</dd>




<dt><code>{token, string()}</code></dt>




<dd>The "token" identifying the destination for a push notification
- for example, the APNS push token or the GCM registration ID.
</dd>




<dt><code>{tag, string()}</code></dt>




<dd>The identification for this registration. Need not be
unique, but note that multiple entries with the same tag
will all receive push notifications. This is one way to
support pushing to a user with multiple devices.
</dd>




<dt><code>{device_id, string()}</code></dt>




<dd>The device identification for this registration. MUST be
unique, so a UUID is recommended.
</dd>




<dt><code>{app_id, string()}</code></dt>




<dd>The application ID for this registration, e.g. <code>com.example.MyApp</code>.
The exact format of application ID varies between services.
</dd>




<dt><code>{dist, string()}</code></dt>




<dd>The distribution for this registration. This optional value
must be either <code>"prod"</code> or <code>"dev"</code>. If omitted, <code>"prod"</code> is assumed.
This affects how pushes are sent, and behaves differently for each
push service. For example, in APNS, this will select between using
production or development push certificates. It currently has no effect
on Android push, but it is likely that this may change to select
between using "production" or "development" push servers.
</dd>



<a name="register_ids-1"></a>

### register_ids/1 ###

<pre><code>
register_ids(ListOfProplists::[<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>]) -&gt; ok | {error, term()}
</code></pre>
<br />

Perform multiple registrations. This takes a list of proplists,
where the proplist is defined in register_id/1.

__See also:__ [register_id/1](#register_id-1).

<a name="register_service-1"></a>

### register_service/1 ###

<pre><code>
register_service(Svc) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Svc = <a href="#type-std_proplist">std_proplist()</a></code></li></ul>

Register a service in the service configuration registry.

__See also:__ [start_service/1](#start_service-1).

<a name="send-1"></a>

### send/1 ###

<pre><code>
send(Notification) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Notification = <a href="#type-notification">notification()</a></code></li><li><code>Result = <a href="#type-sync_send_results">sync_send_results()</a></code></li></ul>

Send a notification specified by proplist `Notification`. The
contents of the proplist differ depending on the push service used.

Notifications have generic and specific sections.
The specific sections are for supported push services,
and contain options that only make sense to the service
in question.

<h5><a name="Example">Example</a></h5>

Providing a UUID as shown is not required, but is recommended for tracking
purposes.

```
  Notification = [
      {uuid, <<"4091b3a2-df6a-443e-a119-8f1d430ed53c">>},
      {alert, <<"Notification to be sent">>},
      {tag, <<"user@domain.com">>},
      % ... other generic options ...
      {aps, [APSOpts]},
      {gcm, [GCMOpts]},
      {etc, [FutureOpts]} % Obviously etc is not a real service.
  ].
```

__See also:__ [sync_send_results()](#type-sync_send_results).

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(Notification, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Notification = <a href="#type-notification">notification()</a></code></li><li><code>Opts = <a href="#type-std_proplist">std_proplist()</a></code></li><li><code>Result = <a href="#type-sync_send_results">sync_send_results()</a></code></li></ul>

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

<a name="start_service-1"></a>

### start_service/1 ###

<pre><code>
start_service(ServiceOpts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>ServiceOpts = <a href="#type-std_proplist">std_proplist()</a></code></li><li><code>Result = {ok, pid()} | {error, term()}</code></li></ul>

Start a push service.


#### <a name="Synopsis">Synopsis</a> ####


```
  Cfg = [
      {name, 'null'},
      {mod, 'sc_push_svc_null'},
      {description, "Null Push Service"},
      {sessions, [
              [
                  {name, 'null-com.silentcircle.SCPushSUITETest'},
                  {mod, sc_push_svc_null},
                  {config, []}
              ]
          ]}
  ],
  {ok, Pid} = sc_push:start_service(Cfg).
```



<dt><code>{name, atom()}</code></dt>




<dd>
A name that denotes the push service for which the
caller is registering. Currently this includes <code>apns</code> for
Apple Push, and <code>gcm</code> for Android (Google Cloud Messaging) push.
There is also a built-in <code>null</code> service for testing.
</dd>




<dt><code>{mod, atom()}</code></dt>




<dd>
All calls to a service are done via the service-specific
API module, which must be on the code path. This is the name
of that API module. All API modules have the same public interface
and are top-level supervisors. They must therefore implement the
Erlang supervisor behavior.
</dd>




<dt><code>{description, string()}</code></dt>




<dd>
A human-readable description of the push service.
</dd>




<dt><code>{sessions, list()}</code></dt>




<dd>
A list of proplists. Each proplist describes a session to be
started. See <code>start_session/1</code> for details.
</dd>




<a name="start_session-2"></a>

### start_session/2 ###

<pre><code>
start_session(Service, Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Service = atom()</code></li><li><code>Opts = <a href="#type-std_proplist">std_proplist()</a></code></li><li><code>Result = {ok, Pid} | {error, Reason}</code></li><li><code>Pid = pid()</code></li><li><code>Reason = term()</code></li></ul>

Start named session as described in the options proplist `Opts`.
`config` is service-dependent.


### <a name="IMPORTANT_NOTE">IMPORTANT NOTE</a> ###

`name` 
<strong>must</strong>
 be in the format `service-api_key`.  For
example, if the service name is `apns`, and the `api_key` is, in this case,
`com.example.SomeApp`, then the session name must be
`apns-com.example.SomeApp`.


#### <a name="APNS_Service">APNS Service</a> ####

Note that the v2 service, `sc_push_svc_apns`, is **deprecated**.
Use `sc_push_svc_apnsv3` instead.

```
  [
   {mod, 'sc_push_svc_apnsv3'},
   {name, 'apnsv3-com.example.MyApp'},
   {config,
    [{host, "api.push.apple.com"},
     {port, 443},
     {apns_env, prod},
     {bundle_seed_id, <<"com.example.MyApp">>},
     {apns_topic, <<"com.example.MyApp">>},
     {retry_delay, 1000},
     {disable_apns_cert_validation, true},
     {ssl_opts,
      [{certfile, "/some/path/com.example.MyApp.cert.pem"},
       {keyfile, "/some/path/com.example.MyApp.key.unencrypted.pem"},
       {cacertfile, "/some/path/cacerts.crt"},
       {honor_cipher_order, false},
       {versions, ['tlsv1.2']},
       {alpn_preferred_protocols, [<<"h2">>]}
      ]
     }
    ]
   }
  ]
```


#### <a name="GCM_Service">GCM Service</a> ####

```
  [
   {mod, 'sc_push_svc_gcm'},
   {name, 'gcm-com.example.SomeApp'},
   {config,
    [
     %% Required GCM API key
     {api_key, <<"ahsgdblahblahfjkjkjfdk">>},
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
    ]}
  ]
```


<a name="stop_service-1"></a>

### stop_service/1 ###

<pre><code>
stop_service(Id) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Id = pid() | <a href="#type-child_id">child_id()</a></code></li><li><code>Result = ok | {error, term()}</code></li></ul>

Stops a service and all sessions for that service.

<a name="stop_session-2"></a>

### stop_session/2 ###

<pre><code>
stop_session(Service, Name) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Service = atom()</code></li><li><code>Name = atom()</code></li><li><code>Result = ok | {error, Reason}</code></li><li><code>Reason = term()</code></li></ul>

Stop named session.


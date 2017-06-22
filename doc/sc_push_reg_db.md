

# Module sc_push_reg_db #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module defines the callback interface for all database-specific
backends such as `sc_push_reg_db_postgres`.

__This module defines the `sc_push_reg_db` behaviour.__<br /> Required callback functions: `db_init/1`, `db_info/1`, `db_terminate/1`, `delete_push_regs_by_device_ids/2`, `delete_push_regs_by_ids/2`, `delete_push_regs_by_svc_toks/2`, `update_invalid_timestamps_by_svc_toks/2`, `delete_push_regs_by_tags/2`, `get_registration_info_by_device_id/2`, `get_registration_info_by_id/2`, `get_registration_info_by_svc_tok/2`, `get_registration_info_by_tag/2`, `is_valid_push_reg/2`, `reregister_ids/2`, `reregister_svc_toks/2`, `save_push_regs/2`, `all_reg/1`, `all_registration_info/1`.

__See also:__ [sc_push_reg_db_mnesia](sc_push_reg_db_mnesia.md), [sc_push_reg_db_postgres](sc_push_reg_db_postgres.md).

<a name="description"></a>

## Description ##
This is an active module that supports connection pooling.
<a name="types"></a>

## Data Types ##




### <a name="type-app_id">app_id()</a> ###


<pre><code>
app_id() = binary()
</code></pre>

An application id, e.g. `<<"com.example.foo">>`.



### <a name="type-atom_or_str">atom_or_str()</a> ###


<pre><code>
atom_or_str() = atom() | string()
</code></pre>




### <a name="type-atomable">atomable()</a> ###


<pre><code>
atomable() = atom() | string() | binary()
</code></pre>




### <a name="type-binable">binable()</a> ###


<pre><code>
binable() = atom() | binary() | integer() | iolist()
</code></pre>




### <a name="type-created_on">created_on()</a> ###


<pre><code>
created_on() = <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>




### <a name="type-db_props_lookup_result">db_props_lookup_result()</a> ###


<pre><code>
db_props_lookup_result() = <a href="#type-reg_db_result">reg_db_result</a>(<a href="#type-mult_reg_db_props">mult_reg_db_props()</a> | notfound)
</code></pre>




### <a name="type-db_props_result">db_props_result()</a> ###


<pre><code>
db_props_result() = <a href="#type-reg_db_result">reg_db_result</a>(<a href="#type-reg_db_props">reg_db_props()</a>)
</code></pre>




### <a name="type-device_id">device_id()</a> ###


<pre><code>
device_id() = binary()
</code></pre>

A binary string denoting the installation and
application-specific device identifier. The
format and content of the string is opaque.



### <a name="type-dist">dist()</a> ###


<pre><code>
dist() = binary()
</code></pre>

A distribution, `<<"prod">>` or `<<"dev">>`.



### <a name="type-err_disconnected">err_disconnected()</a> ###


<pre><code>
err_disconnected() = {error, disconnected}
</code></pre>




### <a name="type-global_name">global_name()</a> ###


<pre><code>
global_name() = term()
</code></pre>




### <a name="type-inv_time">inv_time()</a> ###


<pre><code>
inv_time() = undefined | <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>




### <a name="type-last_invalid_ts">last_invalid_ts()</a> ###


<pre><code>
last_invalid_ts() = <a href="#type-posix_timestamp_milliseconds">posix_timestamp_milliseconds()</a>
</code></pre>




### <a name="type-mod_time">mod_time()</a> ###


<pre><code>
mod_time() = <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>




### <a name="type-mult_db_props_result">mult_db_props_result()</a> ###


<pre><code>
mult_db_props_result() = <a href="#type-reg_db_result">reg_db_result</a>(<a href="#type-mult_reg_db_props">mult_reg_db_props()</a>)
</code></pre>




### <a name="type-mult_reg_db_props">mult_reg_db_props()</a> ###


<pre><code>
mult_reg_db_props() = [<a href="#type-reg_db_props">reg_db_props()</a>]
</code></pre>




### <a name="type-mult_svc_tok_ts">mult_svc_tok_ts()</a> ###


<pre><code>
mult_svc_tok_ts() = [<a href="#type-svc_tok_ts">svc_tok_ts()</a>]
</code></pre>




### <a name="type-name">name()</a> ###


<pre><code>
name() = atom()
</code></pre>




### <a name="type-posix_timestamp_milliseconds">posix_timestamp_milliseconds()</a> ###


<pre><code>
posix_timestamp_milliseconds() = non_neg_integer()
</code></pre>




### <a name="type-reg_db_props">reg_db_props()</a> ###


<pre><code>
reg_db_props() = [{app_id, <a href="#type-app_id">app_id()</a>} | {dist, <a href="#type-dist">dist()</a>} | {service, <a href="#type-service">service()</a>} | {device_id, <a href="#type-device_id">device_id()</a>} | {tag, <a href="#type-tag">tag()</a>} | {modified, <a href="#type-mod_time">mod_time()</a>} | {created_on, <a href="#type-created_on">created_on()</a>} | {last_invalid_on, <a href="#type-inv_time">inv_time()</a>} | {token, <a href="#type-token">token()</a>}]
</code></pre>




### <a name="type-reg_db_result">reg_db_result()</a> ###


<pre><code>
reg_db_result(Result) = Result | <a href="#type-err_disconnected">err_disconnected()</a>
</code></pre>




### <a name="type-reg_id_key">reg_id_key()</a> ###


<pre><code>
reg_id_key() = {<a href="#type-device_id">device_id()</a>, <a href="#type-tag">tag()</a>}
</code></pre>

Registration ID key.



### <a name="type-reg_id_keys">reg_id_keys()</a> ###


<pre><code>
reg_id_keys() = [<a href="#type-reg_id_key">reg_id_key()</a>]
</code></pre>

A list of registration id keys.



### <a name="type-server_ref">server_ref()</a> ###


<pre><code>
server_ref() = <a href="#type-name">name()</a> | {<a href="#type-name">name()</a>, node()} | {global, <a href="#type-global_name">global_name()</a>} | {via, module(), <a href="#type-via_name">via_name()</a>} | pid()
</code></pre>




### <a name="type-service">service()</a> ###


<pre><code>
service() = atom()
</code></pre>

The name of the push service. Currently, only
`apns` (Apple Push) and `gcm` (Google Cloud
Messaging) are known identifiers.



### <a name="type-svc_tok_key">svc_tok_key()</a> ###


<pre><code>
svc_tok_key() = {<a href="#type-service">service()</a>, <a href="#type-token">token()</a>}
</code></pre>

Service/Token key.



### <a name="type-svc_tok_ts">svc_tok_ts()</a> ###


<pre><code>
svc_tok_ts() = {<a href="#type-svc_tok_key">svc_tok_key()</a>, <a href="#type-last_invalid_ts">last_invalid_ts()</a>}
</code></pre>




### <a name="type-tag">tag()</a> ###


<pre><code>
tag() = binary()
</code></pre>

A binary string denoting an identifier that
links together a number of device
registrations. This could be an email address,
a UUID, or something else. This value is
totally opaque to the registration service,
so if it is a string, it may be case-sensitive.



### <a name="type-token">token()</a> ###


<pre><code>
token() = binary()
</code></pre>

The push token (APNS) or registration id (GCM)
of the device + app being registered.
The format of this is a binary string of the
values provided by APNS or GCM, namely a lower
case hex string (AONS) and a very long text
string returned by GCM.



### <a name="type-via_name">via_name()</a> ###


<pre><code>
via_name() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete_push_regs_by_device_ids-2">delete_push_regs_by_device_ids/2</a></td><td>Delete push registrations by device ids.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_ids-2">delete_push_regs_by_ids/2</a></td><td>Delete push registrations by internal registration id.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_svc_toks-2">delete_push_regs_by_svc_toks/2</a></td><td>Delete push registrations by service-token.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_tags-2">delete_push_regs_by_tags/2</a></td><td>Delete push registrations by tags.</td></tr><tr><td valign="top"><a href="#from_posix_time_ms-1">from_posix_time_ms/1</a></td><td>Convert timestamp in milliseconds from epoch to Erlang <code>now</code>
format.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_device_id-2">get_registration_info_by_device_id/2</a></td><td>Get registration information by device id.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_id-2">get_registration_info_by_id/2</a></td><td>Get registration information by unique id.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_svc_tok-2">get_registration_info_by_svc_tok/2</a></td><td>Get registration information by service-token.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_tag-2">get_registration_info_by_tag/2</a></td><td>Get registration information by tag.</td></tr><tr><td valign="top"><a href="#is_valid_push_reg-2">is_valid_push_reg/2</a></td><td>Is push registration proplist valid?.</td></tr><tr><td valign="top"><a href="#make_sc_push_props-7">make_sc_push_props/7</a></td><td>Equivalent to <a href="#make_sc_push_props-8"><tt>make_sc_push_props(Service, Token, DeviceId, Tag, AppId,
Dist, LastModifiedOn, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#make_sc_push_props-8">make_sc_push_props/8</a></td><td>Equivalent to <a href="#make_sc_push_props-9"><tt>make_sc_push_props(Service, Token, DeviceId, Tag, AppId,
Dist, LastModifiedOn, LastInvalidOn, erlang:timestamp())</tt></a>.</td></tr><tr><td valign="top"><a href="#make_sc_push_props-9">make_sc_push_props/9</a></td><td>Create a property list from push registration data.</td></tr><tr><td valign="top"><a href="#reregister_ids-2">reregister_ids/2</a></td><td>Re-register invalidated tokens.</td></tr><tr><td valign="top"><a href="#reregister_svc_toks-2">reregister_svc_toks/2</a></td><td>Re-register invalidated tokens by svc_tok.</td></tr><tr><td valign="top"><a href="#save_push_regs-2">save_push_regs/2</a></td><td>Save a list of push registrations.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>
Start the server without linking (for debugging and testing).</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Start the server.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>
Stop an unlinked server (for testing and debugging).</td></tr><tr><td valign="top"><a href="#update_invalid_timestamps_by_svc_toks-2">update_invalid_timestamps_by_svc_toks/2</a></td><td>Update push registration invalid timestamp by service-token.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete_push_regs_by_device_ids-2"></a>

### delete_push_regs_by_device_ids/2 ###

<pre><code>
delete_push_regs_by_device_ids(Worker::pid(), DeviceIDs::[binary()]) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by device ids

<a name="delete_push_regs_by_ids-2"></a>

### delete_push_regs_by_ids/2 ###

<pre><code>
delete_push_regs_by_ids(Worker::pid(), IDs::<a href="#type-reg_id_keys">reg_id_keys()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by internal registration id.

<a name="delete_push_regs_by_svc_toks-2"></a>

### delete_push_regs_by_svc_toks/2 ###

<pre><code>
delete_push_regs_by_svc_toks(Worker::pid(), SvcToks::[<a href="#type-svc_tok_key">svc_tok_key()</a>]) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by service-token.

<a name="delete_push_regs_by_tags-2"></a>

### delete_push_regs_by_tags/2 ###

<pre><code>
delete_push_regs_by_tags(Worker::pid(), Tags::[binary()]) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by tags.

<a name="from_posix_time_ms-1"></a>

### from_posix_time_ms/1 ###

`from_posix_time_ms(TimestampMs) -> any()`

Convert timestamp in milliseconds from epoch to Erlang `now`
format.

<a name="get_registration_info_by_device_id-2"></a>

### get_registration_info_by_device_id/2 ###

<pre><code>
get_registration_info_by_device_id(Worker::pid(), DeviceID::binary()) -&gt; <a href="#type-mult_reg_db_props">mult_reg_db_props()</a> | notfound
</code></pre>
<br />

Get registration information by device id.

<a name="get_registration_info_by_id-2"></a>

### get_registration_info_by_id/2 ###

<pre><code>
get_registration_info_by_id(Worker::pid(), ID::<a href="#type-reg_id_key">reg_id_key()</a>) -&gt; <a href="#type-mult_reg_db_props">mult_reg_db_props()</a> | notfound
</code></pre>
<br />

Get registration information by unique id.

__See also:__ [sc_push_reg_api:make_id/2](sc_push_reg_api.md#make_id-2).

<a name="get_registration_info_by_svc_tok-2"></a>

### get_registration_info_by_svc_tok/2 ###

<pre><code>
get_registration_info_by_svc_tok(Worker::pid(), SvcTok::<a href="#type-svc_tok_key">svc_tok_key()</a>) -&gt; <a href="#type-mult_reg_db_props">mult_reg_db_props()</a> | notfound
</code></pre>
<br />

Get registration information by service-token.

__See also:__ [sc_push_reg_api:make_svc_tok/2](sc_push_reg_api.md#make_svc_tok-2).

<a name="get_registration_info_by_tag-2"></a>

### get_registration_info_by_tag/2 ###

<pre><code>
get_registration_info_by_tag(Worker::pid(), Tag::binary()) -&gt; <a href="#type-mult_reg_db_props">mult_reg_db_props()</a> | notfound
</code></pre>
<br />

Get registration information by tag.

<a name="is_valid_push_reg-2"></a>

### is_valid_push_reg/2 ###

<pre><code>
is_valid_push_reg(Worker::pid(), PL::<a href="#type-reg_db_props">reg_db_props()</a>) -&gt; boolean()
</code></pre>
<br />

Is push registration proplist valid?

<a name="make_sc_push_props-7"></a>

### make_sc_push_props/7 ###

<pre><code>
make_sc_push_props(Service, Token, DeviceId, Tag, AppId, Dist, LastModifiedOn) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Service = <a href="#type-atomable">atomable()</a></code></li><li><code>Token = <a href="#type-binable">binable()</a></code></li><li><code>DeviceId = <a href="#type-binable">binable()</a></code></li><li><code>Tag = <a href="#type-binable">binable()</a></code></li><li><code>AppId = <a href="#type-binable">binable()</a></code></li><li><code>Dist = <a href="#type-binable">binable()</a></code></li><li><code>LastModifiedOn = <a href="erlang.md#type-timestamp">erlang:timestamp()</a></code></li><li><code>Result = <a href="#type-reg_db_props">reg_db_props()</a></code></li></ul>

Equivalent to [`make_sc_push_props(Service, Token, DeviceId, Tag, AppId,Dist, LastModifiedOn, undefined)`](#make_sc_push_props-8).

<a name="make_sc_push_props-8"></a>

### make_sc_push_props/8 ###

<pre><code>
make_sc_push_props(Service, Token, DeviceId, Tag, AppId, Dist, LastModifiedOn, LastInvalidOn) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Service = <a href="#type-atomable">atomable()</a></code></li><li><code>Token = <a href="#type-binable">binable()</a></code></li><li><code>DeviceId = <a href="#type-binable">binable()</a></code></li><li><code>Tag = <a href="#type-binable">binable()</a></code></li><li><code>AppId = <a href="#type-binable">binable()</a></code></li><li><code>Dist = <a href="#type-binable">binable()</a></code></li><li><code>LastModifiedOn = <a href="erlang.md#type-timestamp">erlang:timestamp()</a></code></li><li><code>LastInvalidOn = undefined | <a href="erlang.md#type-timestamp">erlang:timestamp()</a></code></li><li><code>Result = <a href="#type-reg_db_props">reg_db_props()</a></code></li></ul>

Equivalent to [`make_sc_push_props(Service, Token, DeviceId, Tag, AppId,Dist, LastModifiedOn, LastInvalidOn, erlang:timestamp())`](#make_sc_push_props-9).

<a name="make_sc_push_props-9"></a>

### make_sc_push_props/9 ###

<pre><code>
make_sc_push_props(Service, Token, DeviceId, Tag, AppId, Dist, LastModifiedOn, LastInvalidOn, CreatedOn) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Service = <a href="#type-atomable">atomable()</a></code></li><li><code>Token = <a href="#type-binable">binable()</a></code></li><li><code>DeviceId = <a href="#type-binable">binable()</a></code></li><li><code>Tag = <a href="#type-binable">binable()</a></code></li><li><code>AppId = <a href="#type-binable">binable()</a></code></li><li><code>Dist = <a href="#type-binable">binable()</a></code></li><li><code>LastModifiedOn = <a href="erlang.md#type-timestamp">erlang:timestamp()</a></code></li><li><code>LastInvalidOn = undefined | <a href="erlang.md#type-timestamp">erlang:timestamp()</a></code></li><li><code>CreatedOn = <a href="erlang.md#type-timestamp">erlang:timestamp()</a></code></li><li><code>Result = <a href="#type-reg_db_props">reg_db_props()</a></code></li></ul>

Create a property list from push registration data.

<a name="reregister_ids-2"></a>

### reregister_ids/2 ###

<pre><code>
reregister_ids(Worker::pid(), IDToks::[{<a href="#type-reg_id_key">reg_id_key()</a>, binary()}]) -&gt; ok
</code></pre>
<br />

Re-register invalidated tokens

<a name="reregister_svc_toks-2"></a>

### reregister_svc_toks/2 ###

<pre><code>
reregister_svc_toks(Worker::pid(), SvcToks::[{<a href="#type-svc_tok_key">svc_tok_key()</a>, binary()}]) -&gt; ok
</code></pre>
<br />

Re-register invalidated tokens by svc_tok

<a name="save_push_regs-2"></a>

### save_push_regs/2 ###

<pre><code>
save_push_regs(Worker, ListOfPropLists) -&gt; ok | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Worker = pid()</code></li><li><code>ListOfPropLists = [<a href="#type-reg_db_props">reg_db_props()</a>, ...]</code></li></ul>

Save a list of push registrations.

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Args) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Args = term()</code></li><li><code>Result = {ok, pid()} | ignore | {error, term()}</code></li></ul>

Start the server without linking (for debugging and testing).

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Args) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Args = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Reason}</code></li><li><code>Pid = pid()</code></li><li><code>Reason = term()</code></li></ul>

Start the server

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(ServerRef) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>ServerRef = <a href="#type-server_ref">server_ref()</a></code></li></ul>

Stop an unlinked server (for testing and debugging).

<a name="update_invalid_timestamps_by_svc_toks-2"></a>

### update_invalid_timestamps_by_svc_toks/2 ###

<pre><code>
update_invalid_timestamps_by_svc_toks(Worker, SvcToksTs) -&gt; ok | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Worker = pid()</code></li><li><code>SvcToksTs = [{<a href="#type-svc_tok_key">svc_tok_key()</a>, non_neg_integer()}]</code></li></ul>

Update push registration invalid timestamp by service-token.


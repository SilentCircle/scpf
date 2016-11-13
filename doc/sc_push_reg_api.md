

# Module sc_push_reg_api #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-atom_or_str">atom_or_str()</a> ###


<pre><code>
atom_or_str() = atom() | string()
</code></pre>




### <a name="type-bin_or_str">bin_or_str()</a> ###


<pre><code>
bin_or_str() = binary() | string()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_registration_info-0">all_registration_info/0</a></td><td>Get registration info of all registered IDs.</td></tr><tr><td valign="top"><a href="#deregister_device_id-1">deregister_device_id/1</a></td><td>Deregister all registrations using a common device ID.</td></tr><tr><td valign="top"><a href="#deregister_device_ids-1">deregister_device_ids/1</a></td><td>Deregister all registrations corresponding to a list of device IDs.</td></tr><tr><td valign="top"><a href="#deregister_id-1">deregister_id/1</a></td><td>Deregister by id.</td></tr><tr><td valign="top"><a href="#deregister_ids-1">deregister_ids/1</a></td><td>Deregister using list of ids.</td></tr><tr><td valign="top"><a href="#deregister_svc_tok-1">deregister_svc_tok/1</a></td><td>Deregister all registrations with common service+push token.</td></tr><tr><td valign="top"><a href="#deregister_svc_toks-1">deregister_svc_toks/1</a></td><td>Deregister all registrations corresponding to list of service-tokens.</td></tr><tr><td valign="top"><a href="#deregister_tag-1">deregister_tag/1</a></td><td>Deregister all registrations using a common tag.</td></tr><tr><td valign="top"><a href="#deregister_tags-1">deregister_tags/1</a></td><td>Deregister all registrations corresponding to a list of tags.</td></tr><tr><td valign="top"><a href="#get_registration_info-1">get_registration_info/1</a></td><td>Get registration information.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_device_id-1">get_registration_info_by_device_id/1</a></td><td>Get registration information by device_id.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_id-1">get_registration_info_by_id/1</a></td><td>Get registration information by unique id.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_id-2">get_registration_info_by_id/2</a></td><td>Equivalent to <tt>get_registration_info_by_id / 1</tt>.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_svc_tok-1">get_registration_info_by_svc_tok/1</a></td><td>Get registration information by service-token.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_svc_tok-2">get_registration_info_by_svc_tok/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_registration_info_by_tag-1">get_registration_info_by_tag/1</a></td><td>Get registration information by tag.</td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td>Initialize API.</td></tr><tr><td valign="top"><a href="#is_valid_push_reg-1">is_valid_push_reg/1</a></td><td>Validate push registration proplist.</td></tr><tr><td valign="top"><a href="#make_id-2">make_id/2</a></td><td>Create a unique id from device_id and tag.</td></tr><tr><td valign="top"><a href="#make_svc_tok-2">make_svc_tok/2</a></td><td>Create service-token key.</td></tr><tr><td valign="top"><a href="#register_id-1">register_id/1</a></td><td>Register an identity for receiving push notifications
from a supported push service.</td></tr><tr><td valign="top"><a href="#register_ids-1">register_ids/1</a></td><td>Register a list of identities that should receive push notifications.</td></tr><tr><td valign="top"><a href="#reregister_id-2">reregister_id/2</a></td><td>Reregister a previously-registered identity, substituting a new token
for the specified push service.</td></tr><tr><td valign="top"><a href="#reregister_svc_tok-2">reregister_svc_tok/2</a></td><td>Reregister a previously-registered identity, substituting a new token
for the specified push service and removing .</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_registration_info-0"></a>

### all_registration_info/0 ###

<pre><code>
all_registration_info() -&gt; [<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>]
</code></pre>
<br />

Get registration info of all registered IDs. Note
that in future, this may be limited to the first 100
IDs found. It may also be supplemented by an API that
supports getting the information in batches.

<a name="deregister_device_id-1"></a>

### deregister_device_id/1 ###

<pre><code>
deregister_device_id(DeviceID::binary()) -&gt; ok | {error, term()}
</code></pre>
<br />

Deregister all registrations using a common device ID

<a name="deregister_device_ids-1"></a>

### deregister_device_ids/1 ###

<pre><code>
deregister_device_ids(DeviceIDs::[binary()]) -&gt; ok | {error, term()}
</code></pre>
<br />

Deregister all registrations corresponding to a list of device IDs.

<a name="deregister_id-1"></a>

### deregister_id/1 ###

<pre><code>
deregister_id(ID::<a href="sc_push_reg_db.md#type-reg_id_key">sc_push_reg_db:reg_id_key()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Deregister by id.

<a name="deregister_ids-1"></a>

### deregister_ids/1 ###

<pre><code>
deregister_ids(IDs::[<a href="sc_push_reg_db.md#type-reg_id_key">sc_push_reg_db:reg_id_key()</a>]) -&gt; ok | {error, term()}
</code></pre>
<br />

Deregister using list of ids.

<a name="deregister_svc_tok-1"></a>

### deregister_svc_tok/1 ###

<pre><code>
deregister_svc_tok(SvcTok::<a href="sc_push_reg_db.md#type-svc_tok_key">sc_push_reg_db:svc_tok_key()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Deregister all registrations with common service+push token

<a name="deregister_svc_toks-1"></a>

### deregister_svc_toks/1 ###

<pre><code>
deregister_svc_toks(SvcToks::[<a href="sc_push_reg_db.md#type-svc_tok_key">sc_push_reg_db:svc_tok_key()</a>]) -&gt; ok | {error, term()}
</code></pre>
<br />

Deregister all registrations corresponding to list of service-tokens.

<a name="deregister_tag-1"></a>

### deregister_tag/1 ###

<pre><code>
deregister_tag(Tag::binary()) -&gt; ok | {error, term()}
</code></pre>
<br />

Deregister all registrations using a common tag

<a name="deregister_tags-1"></a>

### deregister_tags/1 ###

<pre><code>
deregister_tags(Tags::[binary()]) -&gt; ok | {error, term()}
</code></pre>
<br />

Deregister all registrations corresponding to a list of tags.

<a name="get_registration_info-1"></a>

### get_registration_info/1 ###

<pre><code>
get_registration_info(Tag::<a href="#type-bin_or_str">bin_or_str()</a>) -&gt; <a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a> | notfound
</code></pre>
<br />

Equivalent to `get_registration_info_by_tag / 1`.

Get registration information.

<a name="get_registration_info_by_device_id-1"></a>

### get_registration_info_by_device_id/1 ###

<pre><code>
get_registration_info_by_device_id(DeviceID::binary()) -&gt; [<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>] | notfound
</code></pre>
<br />

Get registration information by device_id.

<a name="get_registration_info_by_id-1"></a>

### get_registration_info_by_id/1 ###

<pre><code>
get_registration_info_by_id(ID::<a href="sc_push_reg_db.md#type-reg_id_key">sc_push_reg_db:reg_id_key()</a>) -&gt; <a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a> | notfound
</code></pre>
<br />

Get registration information by unique id.

__See also:__ [make_id/2](#make_id-2).

<a name="get_registration_info_by_id-2"></a>

### get_registration_info_by_id/2 ###

<pre><code>
get_registration_info_by_id(DeviceID::<a href="#type-bin_or_str">bin_or_str()</a>, Tag::<a href="#type-bin_or_str">bin_or_str()</a>) -&gt; <a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a> | notfound
</code></pre>
<br />

Equivalent to `get_registration_info_by_id / 1`.

<a name="get_registration_info_by_svc_tok-1"></a>

### get_registration_info_by_svc_tok/1 ###

<pre><code>
get_registration_info_by_svc_tok(SvcTok::<a href="sc_push_reg_db.md#type-svc_tok_key">sc_push_reg_db:svc_tok_key()</a>) -&gt; <a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a> | notfound
</code></pre>
<br />

Get registration information by service-token

__See also:__ [make_svc_tok/2](#make_svc_tok-2).

<a name="get_registration_info_by_svc_tok-2"></a>

### get_registration_info_by_svc_tok/2 ###

<pre><code>
get_registration_info_by_svc_tok(Svc::atom(), Tok::binary()) -&gt; <a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a> | notfound
</code></pre>
<br />

<a name="get_registration_info_by_tag-1"></a>

### get_registration_info_by_tag/1 ###

<pre><code>
get_registration_info_by_tag(Tag::binary()) -&gt; [<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>] | notfound
</code></pre>
<br />

Get registration information by tag.

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; ok
</code></pre>
<br />

Initialize API.

<a name="is_valid_push_reg-1"></a>

### is_valid_push_reg/1 ###

<pre><code>
is_valid_push_reg(PL::list()) -&gt; boolean()
</code></pre>
<br />

Validate push registration proplist.

<a name="make_id-2"></a>

### make_id/2 ###

<pre><code>
make_id(DeviceID::<a href="#type-bin_or_str">bin_or_str()</a>, Tag::<a href="#type-bin_or_str">bin_or_str()</a>) -&gt; <a href="sc_push_reg_db.md#type-reg_id_key">sc_push_reg_db:reg_id_key()</a>
</code></pre>
<br />

Create a unique id from device_id and tag.

<a name="make_svc_tok-2"></a>

### make_svc_tok/2 ###

<pre><code>
make_svc_tok(Svc::<a href="#type-atom_or_str">atom_or_str()</a>, Tok::<a href="#type-bin_or_str">bin_or_str()</a>) -&gt; <a href="sc_push_reg_db.md#type-svc_tok_key">sc_push_reg_db:svc_tok_key()</a>
</code></pre>
<br />

Create service-token key

<a name="register_id-1"></a>

### register_id/1 ###

<pre><code>
register_id(Props::<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>) -&gt; <a href="sc_types.md#type-reg_result">sc_types:reg_result()</a>
</code></pre>
<br />

Register an identity for receiving push notifications
from a supported push service.

<a name="register_ids-1"></a>

### register_ids/1 ###

<pre><code>
register_ids(ListOfProplists::[<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>, ...]) -&gt; ok | {error, term()}
</code></pre>
<br />

Register a list of identities that should receive push notifications.

<a name="reregister_id-2"></a>

### reregister_id/2 ###

<pre><code>
reregister_id(OldId::<a href="sc_push_reg_db.md#type-reg_id_key">sc_push_reg_db:reg_id_key()</a>, X2::binary()) -&gt; ok
</code></pre>
<br />

Reregister a previously-registered identity, substituting a new token
for the specified push service.

<a name="reregister_svc_tok-2"></a>

### reregister_svc_tok/2 ###

<pre><code>
reregister_svc_tok(OldSvcTok::<a href="sc_push_reg_db.md#type-svc_tok_key">sc_push_reg_db:svc_tok_key()</a>, X2::binary()) -&gt; ok
</code></pre>
<br />

Reregister a previously-registered identity, substituting a new token
for the specified push service and removing .


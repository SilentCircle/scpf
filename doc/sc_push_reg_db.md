

# Module sc_push_reg_db #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




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




### <a name="type-push_reg_list">push_reg_list()</a> ###


<pre><code>
push_reg_list() = [#sc_pshrg{}]
</code></pre>




### <a name="type-reg_id_key">reg_id_key()</a> ###


<pre><code>
reg_id_key() = {binary(), binary()}
</code></pre>




### <a name="type-reg_id_keys">reg_id_keys()</a> ###


<pre><code>
reg_id_keys() = [<a href="#type-reg_id_key">reg_id_key()</a>]
</code></pre>




### <a name="type-svc_tok_key">svc_tok_key()</a> ###


<pre><code>
svc_tok_key() = {atom(), binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_reg-0">all_reg/0</a></td><td>(<em>Deprecated</em>.) Return a list of all push registration records.</td></tr><tr><td valign="top"><a href="#all_registration_info-0">all_registration_info/0</a></td><td>(<em>Deprecated</em>.) Return a list of property lists of all registrations.</td></tr><tr><td valign="top"><a href="#check_id-1">check_id/1</a></td><td>Check registration id key.</td></tr><tr><td valign="top"><a href="#check_ids-1">check_ids/1</a></td><td>Check multiple registration id keys.</td></tr><tr><td valign="top"><a href="#create_tables-1">create_tables/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete_push_regs_by_device_ids-1">delete_push_regs_by_device_ids/1</a></td><td>Delete push registrations by device ids.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_ids-1">delete_push_regs_by_ids/1</a></td><td>Delete push registrations by internal registration id.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_svc_toks-1">delete_push_regs_by_svc_toks/1</a></td><td>Delete push registrations by service-token.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_tags-1">delete_push_regs_by_tags/1</a></td><td>Delete push registrations by tags.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_device_id-1">get_registration_info_by_device_id/1</a></td><td>Get registration information by device id.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_id-1">get_registration_info_by_id/1</a></td><td>Get registration information by unique id.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_svc_tok-1">get_registration_info_by_svc_tok/1</a></td><td>Get registration information by service-token.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_tag-1">get_registration_info_by_tag/1</a></td><td>Get registration information by tag.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_valid_push_reg-1">is_valid_push_reg/1</a></td><td>Is push registration proplist valid?.</td></tr><tr><td valign="top"><a href="#make_id-2">make_id/2</a></td><td>Convert to an opaque registration ID key.</td></tr><tr><td valign="top"><a href="#make_sc_push_props-8">make_sc_push_props/8</a></td><td>Create a registration proplist required by other functions
in this API.</td></tr><tr><td valign="top"><a href="#make_svc_tok-2">make_svc_tok/2</a></td><td>Convert to an opaque service-token key.</td></tr><tr><td valign="top"><a href="#reregister_ids-1">reregister_ids/1</a></td><td>Re-register invalidated tokens.</td></tr><tr><td valign="top"><a href="#reregister_svc_toks-1">reregister_svc_toks/1</a></td><td>Re-register invalidated tokens by svc_tok.</td></tr><tr><td valign="top"><a href="#save_push_regs-1">save_push_regs/1</a></td><td>Save a list of push registrations.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_reg-0"></a>

### all_reg/0 ###

<pre><code>
all_reg() -&gt; <a href="#type-push_reg_list">push_reg_list()</a>
</code></pre>
<br />

__This function is deprecated:__ For debug only.

Return a list of all push registration records.

<a name="all_registration_info-0"></a>

### all_registration_info/0 ###

<pre><code>
all_registration_info() -&gt; [<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>]
</code></pre>
<br />

__This function is deprecated:__ For debug only

Return a list of property lists of all registrations.

<a name="check_id-1"></a>

### check_id/1 ###

<pre><code>
check_id(ID::<a href="#type-reg_id_key">reg_id_key()</a>) -&gt; <a href="#type-reg_id_key">reg_id_key()</a>
</code></pre>
<br />

Check registration id key.

<a name="check_ids-1"></a>

### check_ids/1 ###

<pre><code>
check_ids(IDs::<a href="#type-reg_id_keys">reg_id_keys()</a>) -&gt; <a href="#type-reg_id_keys">reg_id_keys()</a>
</code></pre>
<br />

Check multiple registration id keys.

<a name="create_tables-1"></a>

### create_tables/1 ###

`create_tables(Nodes) -> any()`

<a name="delete_push_regs_by_device_ids-1"></a>

### delete_push_regs_by_device_ids/1 ###

<pre><code>
delete_push_regs_by_device_ids(DeviceIDs::[binary()]) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by device ids

<a name="delete_push_regs_by_ids-1"></a>

### delete_push_regs_by_ids/1 ###

<pre><code>
delete_push_regs_by_ids(IDs::<a href="#type-reg_id_keys">reg_id_keys()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by internal registration id.

<a name="delete_push_regs_by_svc_toks-1"></a>

### delete_push_regs_by_svc_toks/1 ###

<pre><code>
delete_push_regs_by_svc_toks(SvcToks::[<a href="#type-svc_tok_key">svc_tok_key()</a>]) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by service-token.

<a name="delete_push_regs_by_tags-1"></a>

### delete_push_regs_by_tags/1 ###

<pre><code>
delete_push_regs_by_tags(Tags::[binary()]) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by tags.

<a name="get_registration_info_by_device_id-1"></a>

### get_registration_info_by_device_id/1 ###

<pre><code>
get_registration_info_by_device_id(DeviceID::binary()) -&gt; [<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>] | notfound
</code></pre>
<br />

Get registration information by device id.

<a name="get_registration_info_by_id-1"></a>

### get_registration_info_by_id/1 ###

<pre><code>
get_registration_info_by_id(ID::<a href="#type-reg_id_key">reg_id_key()</a>) -&gt; [<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>] | notfound
</code></pre>
<br />

Get registration information by unique id.

__See also:__ [make_id/2](#make_id-2).

<a name="get_registration_info_by_svc_tok-1"></a>

### get_registration_info_by_svc_tok/1 ###

<pre><code>
get_registration_info_by_svc_tok(SvcTok::<a href="#type-svc_tok_key">svc_tok_key()</a>) -&gt; [<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>] | notfound
</code></pre>
<br />

Get registration information by service-token.

__See also:__ [make_svc_tok/2](#make_svc_tok-2).

<a name="get_registration_info_by_tag-1"></a>

### get_registration_info_by_tag/1 ###

<pre><code>
get_registration_info_by_tag(Tag::binary()) -&gt; [<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>] | notfound
</code></pre>
<br />

Get registration information by tag.

<a name="init-1"></a>

### init/1 ###

`init(Nodes) -> any()`

<a name="is_valid_push_reg-1"></a>

### is_valid_push_reg/1 ###

<pre><code>
is_valid_push_reg(PL::<a href="sc_types.md#type-proplist">sc_types:proplist</a>(atom(), term())) -&gt; boolean()
</code></pre>
<br />

Is push registration proplist valid?

<a name="make_id-2"></a>

### make_id/2 ###

<pre><code>
make_id(Id::<a href="#type-binable">binable()</a>, Tag::<a href="#type-binable">binable()</a>) -&gt; <a href="#type-reg_id_key">reg_id_key()</a>
</code></pre>
<br />

Convert to an opaque registration ID key.

<a name="make_sc_push_props-8"></a>

### make_sc_push_props/8 ###

<pre><code>
make_sc_push_props(Service::<a href="#type-atomable">atomable()</a>, Token::<a href="#type-binable">binable()</a>, DeviceId::<a href="#type-binable">binable()</a>, Tag::<a href="#type-binable">binable()</a>, AppId::<a href="#type-binable">binable()</a>, Dist::<a href="#type-binable">binable()</a>, Vsn::non_neg_integer(), Mod::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>) -&gt; [{app_id, binary()} | {dist, binary()} | {service, atom()} | {device_id, binary()} | {tag, binary()} | {modified, tuple()} | {version, non_neg_integer()} | {token, binary()}]
</code></pre>
<br />

Create a registration proplist required by other functions
in this API.

<a name="make_svc_tok-2"></a>

### make_svc_tok/2 ###

<pre><code>
make_svc_tok(Service::<a href="#type-atom_or_str">atom_or_str()</a>, Token::<a href="#type-binable">binable()</a>) -&gt; <a href="#type-svc_tok_key">svc_tok_key()</a>
</code></pre>
<br />

Convert to an opaque service-token key.

<a name="reregister_ids-1"></a>

### reregister_ids/1 ###

<pre><code>
reregister_ids(IDToks::[{<a href="#type-reg_id_key">reg_id_key()</a>, binary()}]) -&gt; ok
</code></pre>
<br />

Re-register invalidated tokens

<a name="reregister_svc_toks-1"></a>

### reregister_svc_toks/1 ###

<pre><code>
reregister_svc_toks(SvcToks::[{<a href="#type-svc_tok_key">svc_tok_key()</a>, binary()}]) -&gt; ok
</code></pre>
<br />

Re-register invalidated tokens by svc_tok

<a name="save_push_regs-1"></a>

### save_push_regs/1 ###

<pre><code>
save_push_regs(ListOfProplists::[<a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>, ...]) -&gt; ok | {error, term()}
</code></pre>
<br />

Save a list of push registrations.


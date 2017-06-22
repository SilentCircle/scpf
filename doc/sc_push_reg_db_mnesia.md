

# Module sc_push_reg_db_mnesia #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-ctx">ctx()</a> ###


<pre><code>
ctx() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#db_info-1">db_info/1</a></td><td>Get information about the database context passed in <code>Ctx</code>.</td></tr><tr><td valign="top"><a href="#db_init-1">db_init/1</a></td><td>Initialize the database connection.</td></tr><tr><td valign="top"><a href="#db_terminate-1">db_terminate/1</a></td><td>Terminate the database connection.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_device_ids-2">delete_push_regs_by_device_ids/2</a></td><td>Delete push registrations by device ids.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_ids-2">delete_push_regs_by_ids/2</a></td><td>Delete push registrations by internal registration id.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_svc_toks-2">delete_push_regs_by_svc_toks/2</a></td><td>Delete push registrations by service-token.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_tags-2">delete_push_regs_by_tags/2</a></td><td>Delete push registrations by tags.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_device_id-2">get_registration_info_by_device_id/2</a></td><td>Get registration information by device id.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_id-2">get_registration_info_by_id/2</a></td><td>Get registration information by unique id.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_svc_tok-2">get_registration_info_by_svc_tok/2</a></td><td>Get registration information by service-token.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_tag-2">get_registration_info_by_tag/2</a></td><td>Get registration information by tag.</td></tr><tr><td valign="top"><a href="#is_valid_push_reg-2">is_valid_push_reg/2</a></td><td>Is push registration proplist valid?.</td></tr><tr><td valign="top"><a href="#reregister_ids-2">reregister_ids/2</a></td><td>Re-register invalidated tokens.</td></tr><tr><td valign="top"><a href="#reregister_svc_toks-2">reregister_svc_toks/2</a></td><td>Re-register invalidated tokens by svc_tok.</td></tr><tr><td valign="top"><a href="#save_push_regs-2">save_push_regs/2</a></td><td>Save a list of push registrations.</td></tr><tr><td valign="top"><a href="#update_invalid_timestamps_by_svc_toks-2">update_invalid_timestamps_by_svc_toks/2</a></td><td>Update push registration last_invalid timestamp by service-token.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="db_info-1"></a>

### db_info/1 ###

<pre><code>
db_info(Ctx) -&gt; Props
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>Props = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li></ul>

Get information about the database context passed in `Ctx`.

This is currently a no-op and returns an empty list.

<a name="db_init-1"></a>

### db_init/1 ###

<pre><code>
db_init(Config) -&gt; {ok, Context} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Config = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Context = <a href="#type-ctx">ctx()</a></code></li><li><code>Reason = term()</code></li></ul>

Initialize the database connection.

Return an opaque context for use with db_info/1 and db_terminate/1.



<dt><code>Context</code></dt>



<dd>An opaque term returned to the caller.</dd>




<dt><code>Config :: [node()]</code></dt>




<dd>A list of atoms (nodes) on which to create the Mnesia tables. If the
list is empty, defaults to a list containing only the current <code>node()</code>.
This has only been tested with a current node. It is not recommended to use
Mnesia in a distributed context for push registrations due to issues arising
from network partitions (split-brain).</dd>



<a name="db_terminate-1"></a>

### db_terminate/1 ###

<pre><code>
db_terminate(Ctx::<a href="#type-ctx">ctx()</a>) -&gt; ok
</code></pre>
<br />

Terminate the database connection.
This is a no-op an the return value has no significance.

<a name="delete_push_regs_by_device_ids-2"></a>

### delete_push_regs_by_device_ids/2 ###

<pre><code>
delete_push_regs_by_device_ids(Ctx::<a href="#type-ctx">ctx()</a>, DeviceIDs::[binary()]) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by device ids

<a name="delete_push_regs_by_ids-2"></a>

### delete_push_regs_by_ids/2 ###

<pre><code>
delete_push_regs_by_ids(Ctx::<a href="#type-ctx">ctx()</a>, IDs::<a href="sc_push_reg_db.md#type-reg_id_keys">sc_push_reg_db:reg_id_keys()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by internal registration id.

<a name="delete_push_regs_by_svc_toks-2"></a>

### delete_push_regs_by_svc_toks/2 ###

<pre><code>
delete_push_regs_by_svc_toks(Ctx::<a href="#type-ctx">ctx()</a>, SvcToks::[<a href="sc_push_reg_db.md#type-svc_tok_key">sc_push_reg_db:svc_tok_key()</a>]) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by service-token.

<a name="delete_push_regs_by_tags-2"></a>

### delete_push_regs_by_tags/2 ###

<pre><code>
delete_push_regs_by_tags(Ctx::<a href="#type-ctx">ctx()</a>, Tags::[binary()]) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete push registrations by tags.

<a name="get_registration_info_by_device_id-2"></a>

### get_registration_info_by_device_id/2 ###

<pre><code>
get_registration_info_by_device_id(Ctx::<a href="#type-ctx">ctx()</a>, DeviceID::binary()) -&gt; <a href="sc_push_reg_db.md#type-mult_reg_db_props">sc_push_reg_db:mult_reg_db_props()</a> | notfound
</code></pre>
<br />

Get registration information by device id.

<a name="get_registration_info_by_id-2"></a>

### get_registration_info_by_id/2 ###

<pre><code>
get_registration_info_by_id(Ctx::<a href="#type-ctx">ctx()</a>, ID::<a href="sc_push_reg_db.md#type-reg_id_key">sc_push_reg_db:reg_id_key()</a>) -&gt; <a href="sc_push_reg_db.md#type-mult_reg_db_props">sc_push_reg_db:mult_reg_db_props()</a> | notfound
</code></pre>
<br />

Get registration information by unique id.

__See also:__ [sc_push_reg_api:make_id/2](sc_push_reg_api.md#make_id-2).

<a name="get_registration_info_by_svc_tok-2"></a>

### get_registration_info_by_svc_tok/2 ###

<pre><code>
get_registration_info_by_svc_tok(Ctx::<a href="#type-ctx">ctx()</a>, SvcTok::<a href="sc_push_reg_db.md#type-svc_tok_key">sc_push_reg_db:svc_tok_key()</a>) -&gt; <a href="sc_push_reg_db.md#type-mult_reg_db_props">sc_push_reg_db:mult_reg_db_props()</a> | notfound
</code></pre>
<br />

Get registration information by service-token.

__See also:__ [sc_push_reg_api:make_svc_tok/2](sc_push_reg_api.md#make_svc_tok-2).

<a name="get_registration_info_by_tag-2"></a>

### get_registration_info_by_tag/2 ###

<pre><code>
get_registration_info_by_tag(Ctx::<a href="#type-ctx">ctx()</a>, Tag::binary()) -&gt; <a href="sc_push_reg_db.md#type-mult_reg_db_props">sc_push_reg_db:mult_reg_db_props()</a> | notfound
</code></pre>
<br />

Get registration information by tag.

<a name="is_valid_push_reg-2"></a>

### is_valid_push_reg/2 ###

<pre><code>
is_valid_push_reg(Ctx::<a href="#type-ctx">ctx()</a>, PL::<a href="sc_types.md#type-proplist">sc_types:proplist</a>(atom(), term())) -&gt; boolean()
</code></pre>
<br />

Is push registration proplist valid?

<a name="reregister_ids-2"></a>

### reregister_ids/2 ###

<pre><code>
reregister_ids(Ctx::<a href="#type-ctx">ctx()</a>, IDToks::[{<a href="sc_push_reg_db.md#type-reg_id_key">sc_push_reg_db:reg_id_key()</a>, binary()}]) -&gt; ok
</code></pre>
<br />

Re-register invalidated tokens

<a name="reregister_svc_toks-2"></a>

### reregister_svc_toks/2 ###

<pre><code>
reregister_svc_toks(Ctx::<a href="#type-ctx">ctx()</a>, SvcToks::[{<a href="sc_push_reg_db.md#type-svc_tok_key">sc_push_reg_db:svc_tok_key()</a>, binary()}]) -&gt; ok
</code></pre>
<br />

Re-register invalidated tokens by svc_tok

<a name="save_push_regs-2"></a>

### save_push_regs/2 ###

<pre><code>
save_push_regs(Ctx::<a href="#type-ctx">ctx()</a>, ListOfProplists::[<a href="sc_push_reg_db.md#type-reg_db_props">sc_push_reg_db:reg_db_props()</a>, ...]) -&gt; ok | {error, term()}
</code></pre>
<br />

Save a list of push registrations.

<a name="update_invalid_timestamps_by_svc_toks-2"></a>

### update_invalid_timestamps_by_svc_toks/2 ###

<pre><code>
update_invalid_timestamps_by_svc_toks(Ctx::<a href="#type-ctx">ctx()</a>, SvcToksTs::[{<a href="sc_push_reg_db.md#type-svc_tok_key">sc_push_reg_db:svc_tok_key()</a>, non_neg_integer()}]) -&gt; ok | {error, term()}
</code></pre>
<br />

Update push registration last_invalid timestamp by service-token.


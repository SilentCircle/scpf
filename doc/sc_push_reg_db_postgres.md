

# Module sc_push_reg_db_postgres #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-config">config()</a> ###


<pre><code>
config() = <a href="#type-simple_config">simple_config()</a> | <a href="#type-extended_config">extended_config()</a>
</code></pre>




### <a name="type-ctx">ctx()</a> ###


<pre><code>
ctx() = #sc_push_reg_db_postgres{}
</code></pre>




### <a name="type-ext_cfg_connection_only">ext_cfg_connection_only()</a> ###


<pre><code>
ext_cfg_connection_only() = #{connection =&gt; <a href="#type-simple_config">simple_config()</a>}
</code></pre>




### <a name="type-ext_cfg_full">ext_cfg_full()</a> ###


<pre><code>
ext_cfg_full() = #{connection =&gt; <a href="#type-simple_config">simple_config()</a>, table_config =&gt; <a href="#type-table_config">table_config()</a>}
</code></pre>




### <a name="type-extended_config">extended_config()</a> ###


<pre><code>
extended_config() = <a href="#type-ext_cfg_connection_only">ext_cfg_connection_only()</a> | <a href="#type-ext_cfg_full">ext_cfg_full()</a>
</code></pre>




### <a name="type-mult_svc_tok_ts">mult_svc_tok_ts()</a> ###


<pre><code>
mult_svc_tok_ts() = <a href="sc_push_reg_db.md#type-mult_svc_tok_ts">sc_push_reg_db:mult_svc_tok_ts()</a>
</code></pre>




### <a name="type-nonempty_reg_proplists">nonempty_reg_proplists()</a> ###


<pre><code>
nonempty_reg_proplists() = [<a href="#type-reg_proplist">reg_proplist()</a>, ...]
</code></pre>




### <a name="type-pl">pl()</a> ###


<pre><code>
pl(KT, VT) = [{KT, VT}]
</code></pre>




### <a name="type-reg_id_key">reg_id_key()</a> ###


<pre><code>
reg_id_key() = <a href="sc_push_reg_db.md#type-reg_id_key">sc_push_reg_db:reg_id_key()</a>
</code></pre>




### <a name="type-reg_id_keys">reg_id_keys()</a> ###


<pre><code>
reg_id_keys() = <a href="sc_push_reg_db.md#type-reg_id_keys">sc_push_reg_db:reg_id_keys()</a>
</code></pre>




### <a name="type-reg_proplist">reg_proplist()</a> ###


<pre><code>
reg_proplist() = <a href="sc_types.md#type-reg_proplist">sc_types:reg_proplist()</a>
</code></pre>




### <a name="type-simple_config">simple_config()</a> ###


<pre><code>
simple_config() = <a href="#type-pl">pl</a>(atom(), string())
</code></pre>




### <a name="type-svc_tok_key">svc_tok_key()</a> ###


<pre><code>
svc_tok_key() = <a href="sc_push_reg_db.md#type-svc_tok_key">sc_push_reg_db:svc_tok_key()</a>
</code></pre>




### <a name="type-table_config">table_config()</a> ###


<pre><code>
table_config() = <a href="#type-pl">pl</a>(atom(), string())
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#db_info-1">db_info/1</a></td><td>Get information about the database context passed in <code>Ctx</code>.</td></tr><tr><td valign="top"><a href="#db_init-1">db_init/1</a></td><td>Initialize the database connection.</td></tr><tr><td valign="top"><a href="#db_terminate-1">db_terminate/1</a></td><td>Terminate the database connection.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_device_ids-2">delete_push_regs_by_device_ids/2</a></td><td>Delete push registrations by device ids.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_ids-2">delete_push_regs_by_ids/2</a></td><td>Delete push registrations by internal registration id.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_svc_toks-2">delete_push_regs_by_svc_toks/2</a></td><td>Delete push registrations by service-token.</td></tr><tr><td valign="top"><a href="#delete_push_regs_by_tags-2">delete_push_regs_by_tags/2</a></td><td>Delete push registrations by tags.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_device_id-2">get_registration_info_by_device_id/2</a></td><td>Get registration information by device id.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_id-2">get_registration_info_by_id/2</a></td><td>Get registration information by unique id.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_svc_tok-2">get_registration_info_by_svc_tok/2</a></td><td>Get registration information by service-token.</td></tr><tr><td valign="top"><a href="#get_registration_info_by_tag-2">get_registration_info_by_tag/2</a></td><td>Get registration information by tag.</td></tr><tr><td valign="top"><a href="#is_valid_push_reg-2">is_valid_push_reg/2</a></td><td>Return <code>true</code> if push registration proplist is valid.</td></tr><tr><td valign="top"><a href="#reregister_ids-2">reregister_ids/2</a></td><td>Re-register invalidated tokens.</td></tr><tr><td valign="top"><a href="#reregister_svc_toks-2">reregister_svc_toks/2</a></td><td>Re-register invalidated tokens by service and token.</td></tr><tr><td valign="top"><a href="#save_push_regs-2">save_push_regs/2</a></td><td>Save a list of push registrations.</td></tr><tr><td valign="top"><a href="#update_invalid_timestamps_by_svc_toks-2">update_invalid_timestamps_by_svc_toks/2</a></td><td>Update one or more push registration's last invalid timestamp, given a
list of <code>{{Service, Token}, Timestamp}</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="db_info-1"></a>

### db_info/1 ###

<pre><code>
db_info(Ctx) -&gt; Props
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>Props = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li></ul>

Get information about the database context passed in `Ctx`.

Return a property list as follows:



<dt><code>conn :: pid()</code></dt>



<dd>Postgres connection pid</dd>




<dt><code>config :: proplist()</code></dt>



<dd>Value passed to db_init/1</dd>




<dt><code>extra :: term()</code></dt>




<dd>Extra information. This is currently a map of prepared statements,
but may change without notice.</dd>



<a name="db_init-1"></a>

### db_init/1 ###

<pre><code>
db_init(Config) -&gt; {ok, Context} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Config = <a href="#type-config">config()</a></code></li><li><code>Context = <a href="#type-ctx">ctx()</a></code></li><li><code>Reason = term()</code></li></ul>

Initialize the database connection.

Return an opaque context for use with the other API calls.



<dt><code>Config</code></dt>




<dd>This may be provided one of the following formats:
<ul>
<li><em>Simple format</em>A property list containing at least
the following properties (plus any others supported by
<code>epgsql</code>, to which the property list is passed directly):
<dl>
<dt><code>hostname :: string()</code></dt><dd>Postgres host name</dd>
<dt><code>database :: string()</code></dt><dd>Database name</dd>
<dt><code>username :: string()</code></dt><dd>User (role) name</dd>
<dt><code>password :: string()</code></dt><dd>User/role password</dd>
</dl>
</li>
<li><em>Extended format</em>A map with the following keys and
values:
<dl>
<dt><code>connection :: proplist()</code> (required)</dt>
<dd>A property list as defined in the <em>Simple
format</em></dd>
<dt><code>table_config :: proplist()</code> (optional)</dt>
<dd>A property list containing zero or more of the
following properties:
<dl>
<dt><code>table_name :: string()</code></dt>
<dd> The name of the push tokens table (default:
<code>"push_tokens"</code>)</dd>
<dt><code>table_schema :: string()</code></dt>
<dd>The name of the push tokens table schema (default:
<code>"public"</code>)</dd>
</dl>
</dd>
</dl>
</li>
</ul>
</dd>




<dt><code>Context</code></dt>




<dd>An opaque term returned to the caller.</dd>



### Example 1: Simple format ###

```
  Config = [
   {hostname, "db.example.com"},
   {database, "mydb"},
   {username, "mydbuser"},
   {password, "mydbpasswd"}
  ].
```

### Example 2: Extended format without table info ###

```
  Config = #{
    connection => [
                   {hostname, "db.example.com"},
                   {database, "mydb"},
                   {username, "mydbuser"},
                   {password, "mydbpasswd"}
                  ]
  }.
```

### Example 3: Extended format with table info ###

```
  Config = #{
    connection => [
                   {hostname, "db.example.com"},
                   {database, "mydb"},
                   {username, "mydbuser"},
                   {password, "mydbpasswd"}
                  ],
    table_config => [
                     {table_name, "push_tokens"},
                     {table_schema, "public"}
                    ]
  }.
```


<a name="db_terminate-1"></a>

### db_terminate/1 ###

<pre><code>
db_terminate(Ctx) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>Result = ok</code></li></ul>

Terminate the database connection.
The return value has no significance.

<a name="delete_push_regs_by_device_ids-2"></a>

### delete_push_regs_by_device_ids/2 ###

<pre><code>
delete_push_regs_by_device_ids(Ctx, DeviceIds) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>DeviceIds = [binary()]</code></li><li><code>Result = ok | {error, term()}</code></li></ul>

Delete push registrations by device ids.

<a name="delete_push_regs_by_ids-2"></a>

### delete_push_regs_by_ids/2 ###

<pre><code>
delete_push_regs_by_ids(Ctx, IDs) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>IDs = <a href="#type-reg_id_keys">reg_id_keys()</a></code></li><li><code>Result = ok | {error, term()}</code></li></ul>

Delete push registrations by internal registration id.

<a name="delete_push_regs_by_svc_toks-2"></a>

### delete_push_regs_by_svc_toks/2 ###

<pre><code>
delete_push_regs_by_svc_toks(Ctx, SvcToks) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>SvcToks = [<a href="#type-svc_tok_key">svc_tok_key()</a>]</code></li><li><code>Result = ok | {error, term()}</code></li></ul>

Delete push registrations by service-token.

<a name="delete_push_regs_by_tags-2"></a>

### delete_push_regs_by_tags/2 ###

<pre><code>
delete_push_regs_by_tags(Ctx, Tags) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>Tags = [binary()]</code></li><li><code>Result = ok | {error, term()}</code></li></ul>

Delete push registrations by tags.

<a name="get_registration_info_by_device_id-2"></a>

### get_registration_info_by_device_id/2 ###

<pre><code>
get_registration_info_by_device_id(Ctx, DeviceID) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>DeviceID = binary()</code></li><li><code>Result = <a href="sc_push_reg_db.md#type-db_props_lookup_result">sc_push_reg_db:db_props_lookup_result()</a></code></li></ul>

Get registration information by device id.

Return a list of registration property lists. or `notfound`.

<a name="get_registration_info_by_id-2"></a>

### get_registration_info_by_id/2 ###

<pre><code>
get_registration_info_by_id(Ctx, ID) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>ID = <a href="#type-reg_id_key">reg_id_key()</a></code></li><li><code>Result = <a href="sc_push_reg_db.md#type-db_props_lookup_result">sc_push_reg_db:db_props_lookup_result()</a></code></li></ul>

Get registration information by unique id.

__See also:__ [sc_push_reg_db:make_id/2](sc_push_reg_db.md#make_id-2).

<a name="get_registration_info_by_svc_tok-2"></a>

### get_registration_info_by_svc_tok/2 ###

<pre><code>
get_registration_info_by_svc_tok(Ctx, SvcTok) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>SvcTok = <a href="#type-svc_tok_key">svc_tok_key()</a></code></li><li><code>Result = <a href="sc_push_reg_db.md#type-db_props_lookup_result">sc_push_reg_db:db_props_lookup_result()</a></code></li></ul>

Get registration information by service-token.

__See also:__ [sc_push_reg_api:make_svc_tok/2](sc_push_reg_api.md#make_svc_tok-2).

<a name="get_registration_info_by_tag-2"></a>

### get_registration_info_by_tag/2 ###

<pre><code>
get_registration_info_by_tag(Ctx, Tag) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>Tag = binary()</code></li><li><code>Result = <a href="sc_push_reg_db.md#type-db_props_lookup_result">sc_push_reg_db:db_props_lookup_result()</a></code></li></ul>

Get registration information by tag.

<a name="is_valid_push_reg-2"></a>

### is_valid_push_reg/2 ###

<pre><code>
is_valid_push_reg(Ctx, PL) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>PL = <a href="#type-reg_proplist">reg_proplist()</a></code></li></ul>

Return `true` if push registration proplist is valid.

<a name="reregister_ids-2"></a>

### reregister_ids/2 ###

<pre><code>
reregister_ids(Ctx, IDToks) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>IDToks = [{RegID, NewToken}]</code></li><li><code>RegID = <a href="#type-reg_id_key">reg_id_key()</a></code></li><li><code>NewToken = binary()</code></li></ul>

Re-register invalidated tokens.

<a name="reregister_svc_toks-2"></a>

### reregister_svc_toks/2 ###

<pre><code>
reregister_svc_toks(Ctx, SvcToks) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>SvcToks = [{SvcTok, NewToken}]</code></li><li><code>SvcTok = <a href="#type-svc_tok_key">svc_tok_key()</a></code></li><li><code>NewToken = binary()</code></li></ul>

Re-register invalidated tokens by service and token.

<a name="save_push_regs-2"></a>

### save_push_regs/2 ###

<pre><code>
save_push_regs(Ctx, NonemptyRegProplists) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>NonemptyRegProplists = <a href="#type-nonempty_reg_proplists">nonempty_reg_proplists()</a></code></li><li><code>Result = ok | {error, term()}</code></li></ul>

Save a list of push registrations.

<a name="update_invalid_timestamps_by_svc_toks-2"></a>

### update_invalid_timestamps_by_svc_toks/2 ###

<pre><code>
update_invalid_timestamps_by_svc_toks(Ctx, SvcToksTs) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Ctx = <a href="#type-ctx">ctx()</a></code></li><li><code>SvcToksTs = <a href="#type-mult_svc_tok_ts">mult_svc_tok_ts()</a></code></li><li><code>Result = ok | {error, term()}</code></li></ul>

Update one or more push registration's last invalid timestamp, given a
list of `{{Service, Token}, Timestamp}`.


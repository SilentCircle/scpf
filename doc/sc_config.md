

# Module sc_config #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Configuration server for sc_push.

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Edwin Fine.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete key.</td></tr><tr><td valign="top"><a href="#delete_all-0">delete_all/0</a></td><td></td></tr><tr><td valign="top"><a href="#delete_keys-1">delete_keys/1</a></td><td>Delete multiple values matching a key.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get value for key, or undefined is not found.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Get value for key, or default value if key not found.</td></tr><tr><td valign="top"><a href="#get_all_keys-0">get_all_keys/0</a></td><td>Get all keys.</td></tr><tr><td valign="top"><a href="#get_all_values-0">get_all_values/0</a></td><td>Get all values.</td></tr><tr><td valign="top"><a href="#select-1">select/1</a></td><td>Select multiple values matching a key.</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td>Set key/value pair.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(K::term()) -&gt; ok
</code></pre>
<br />

Delete key.

<a name="delete_all-0"></a>

### delete_all/0 ###

<pre><code>
delete_all() -&gt; ok
</code></pre>
<br />

<a name="delete_keys-1"></a>

### delete_keys/1 ###

<pre><code>
delete_keys(K) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>K = any()</code></li></ul>

Delete multiple values matching a key. The key may contain wildcards,
defined as the atom `'_'`. There is no prefix matching such as `'foo_'`. To match part of a key that is a tuple, put wildcards in the "don't
care" positions of the tuple. The arity of the tuple must be correct.

The deletion will be performed atomically.
--------------------------------------------------------------------

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(K::term()) -&gt; term() | undefined
</code></pre>
<br />

Get value for key, or undefined is not found.

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(K::term(), Def::term()) -&gt; term()
</code></pre>
<br />

Get value for key, or default value if key not found.

<a name="get_all_keys-0"></a>

### get_all_keys/0 ###

<pre><code>
get_all_keys() -&gt; list()
</code></pre>
<br />

Get all keys

<a name="get_all_values-0"></a>

### get_all_values/0 ###

<pre><code>
get_all_values() -&gt; list()
</code></pre>
<br />

Get all values

<a name="select-1"></a>

### select/1 ###

<pre><code>
select(K) -&gt; Values
</code></pre>

<ul class="definitions"><li><code>K = any()</code></li><li><code>Values = [any()]</code></li></ul>

Select multiple values matching a key. The key may contain wildcards,
defined as the atom `'_'`. There is no prefix matching such as `'foo_'`. To match part of a key that is a tuple, put wildcards in the "don't
care" positions of the tuple. The arity of the tuple must be correct.


#### <a name="Examples">Examples</a> ####

* `select('_')` is the same as `get_all_values()`.

* `select({foo,'_'})` will select all keys that are 2-element
tuples whose first element is `'foo'`


<a name="set-2"></a>

### set/2 ###

<pre><code>
set(K::term(), V::term()) -&gt; ok
</code></pre>
<br />

Set key/value pair.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>
<br />

Starts the server




# Module sc_push_req_mgr #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) (C) 2012,2013 Silent Circle LLC

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Edwin Fine.

<a name="types"></a>

## Data Types ##




### <a name="type-req_prop">req_prop()</a> ###


<pre><code>
req_prop() = <a href="sc_types.md#type-prop">sc_types:prop</a>(atom(), term())
</code></pre>




### <a name="type-req_props">req_props()</a> ###


<pre><code>
req_props() = [<a href="#type-req_prop">req_prop()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-2">add/2</a></td><td>Add a request.</td></tr><tr><td valign="top"><a href="#all_req-0">all_req/0</a></td><td>Return all requests as a list of proplists.</td></tr><tr><td valign="top"><a href="#default_callback-1">default_callback/1</a></td><td>Default callback.</td></tr><tr><td valign="top"><a href="#lookup-1">lookup/1</a></td><td>
Lookup a request.</td></tr><tr><td valign="top"><a href="#remove-1">remove/1</a></td><td>Remove a request and return it if it was there, or undefined.</td></tr><tr><td valign="top"><a href="#remove_all-0">remove_all/0</a></td><td>Remove all requests.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Start the server.</td></tr><tr><td valign="top"><a href="#sweep-0">sweep/0</a></td><td>Manually kick off an asynchronous sweep for aged-out requests.</td></tr><tr><td valign="top"><a href="#sweep-1">sweep/1</a></td><td>Manually kick off a sync sweep for requests, specifying max age in
seconds.</td></tr><tr><td valign="top"><a href="#sync_sweep-0">sync_sweep/0</a></td><td>Manually kick off a synchronous sweep for aged-out requests.</td></tr><tr><td valign="top"><a href="#sync_sweep-1">sync_sweep/1</a></td><td>Manually kick off a sync sweep for requests, specifying max age in
seconds.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-2"></a>

### add/2 ###

<pre><code>
add(Id::term(), Req::term()) -&gt; ok
</code></pre>
<br />

Add a request.

<a name="all_req-0"></a>

### all_req/0 ###

<pre><code>
all_req() -&gt; [<a href="#type-req_props">req_props()</a>]
</code></pre>
<br />

Return all requests as a list of proplists.

<a name="default_callback-1"></a>

### default_callback/1 ###

`default_callback(Props) -> any()`

Default callback. Does nothing, returns ok.

<a name="lookup-1"></a>

### lookup/1 ###

<pre><code>
lookup(Id::term()) -&gt; Req::<a href="#type-req_props">req_props()</a> | undefined
</code></pre>
<br />

Lookup a request.

<a name="remove-1"></a>

### remove/1 ###

<pre><code>
remove(Id::term()) -&gt; Req::<a href="#type-req_props">req_props()</a> | undefined
</code></pre>
<br />

Remove a request and return it if it was there, or undefined.

<a name="remove_all-0"></a>

### remove_all/0 ###

<pre><code>
remove_all() -&gt; ok
</code></pre>
<br />

Remove all requests.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>
<br />

Start the server.

<a name="sweep-0"></a>

### sweep/0 ###

<pre><code>
sweep() -&gt; ok
</code></pre>
<br />

Manually kick off an asynchronous sweep for aged-out requests.

<a name="sweep-1"></a>

### sweep/1 ###

<pre><code>
sweep(MaxAge::non_neg_integer()) -&gt; ok
</code></pre>
<br />

Manually kick off a sync sweep for requests, specifying max age in
seconds. If max age is negative, all requests will be removed.

<a name="sync_sweep-0"></a>

### sync_sweep/0 ###

<pre><code>
sync_sweep() -&gt; {ok, NumDeleted::non_neg_integer()}
</code></pre>
<br />

Manually kick off a synchronous sweep for aged-out requests.

<a name="sync_sweep-1"></a>

### sync_sweep/1 ###

<pre><code>
sync_sweep(MaxAge::non_neg_integer()) -&gt; {ok, NumDeleted::non_neg_integer()}
</code></pre>
<br />

Manually kick off a sync sweep for requests, specifying max age in
seconds. If max age is negative, all requests will be removed.


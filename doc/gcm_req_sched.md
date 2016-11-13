

# Module gcm_req_sched #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module is the request scheduler.

Copyright (c) 2015 Silent Circle

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##

Its purpose is to store and
resubmit delayed GCM requests at some future time. For performance
reasons, it does not store notifications on disk, so a shutdown
or crash will lose notifications that were to have been resubmitted.

GCM notifications may be delayed because the GCM server was busy
or temporarily unavailable.

<a name="types"></a>

## Data Types ##




### <a name="type-time_posix_milli_seconds">time_posix_milli_seconds()</a> ###


<pre><code>
time_posix_milli_seconds() = pos_integer()
</code></pre>




### <a name="type-time_posix_secs">time_posix_secs()</a> ###


<pre><code>
time_posix_secs() = pos_integer()
</code></pre>




### <a name="type-trigger_time">trigger_time()</a> ###


<pre><code>
trigger_time() = <a href="#type-time_posix_secs">time_posix_secs()</a> | {<a href="#type-time_posix_milli_seconds">time_posix_milli_seconds()</a>, milli_seconds}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-4">add/4</a></td><td>Add data with unique id to be triggered at a POSIX time
to send the data to pid.</td></tr><tr><td valign="top"><a href="#clear_all-0">clear_all/0</a></td><td>Clear all entries.</td></tr><tr><td valign="top"><a href="#del-1">del/1</a></td><td>Delete data with given id.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Retrieve data with given id, and return <code>{TriggerTime, Data}</code>, or
<code>notfound</code>.</td></tr><tr><td valign="top"><a href="#run_sweep-0">run_sweep/0</a></td><td>Force a sweep of all data to trigger matching messages.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stops the server - for testing only.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-4"></a>

### add/4 ###

<pre><code>
add(Id::any(), TriggerTime::<a href="#type-trigger_time">trigger_time()</a>, Data::any(), Pid::pid()) -&gt; ok
</code></pre>
<br />

Add data with unique id to be triggered at a POSIX time
to send the data to pid.
`Data` is sent to `Pid` as `{triggered, {ReqId, Data}}`.
--------------------------------------------------------------------

<a name="clear_all-0"></a>

### clear_all/0 ###

<pre><code>
clear_all() -&gt; ok
</code></pre>
<br />

Clear all entries.
--------------------------------------------------------------------

<a name="del-1"></a>

### del/1 ###

<pre><code>
del(Id::any()) -&gt; ok
</code></pre>
<br />

Delete data with given id.
--------------------------------------------------------------------

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(Id::any()) -&gt; {ok, {TriggerTime::pos_integer(), Data::any}} | notfound
</code></pre>
<br />

Retrieve data with given id, and return `{TriggerTime, Data}`, or
`notfound`. `TriggerTime` is Erlang monotonic time in milliseconds.
--------------------------------------------------------------------

<a name="run_sweep-0"></a>

### run_sweep/0 ###

<pre><code>
run_sweep() -&gt; ok
</code></pre>
<br />

Force a sweep of all data to trigger matching messages.
--------------------------------------------------------------------

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>
<br />

Starts the server

<a name="stop-0"></a>

### stop/0 ###

<pre><code>
stop() -&gt; ok
</code></pre>
<br />

Stops the server - for testing only.
--------------------------------------------------------------------


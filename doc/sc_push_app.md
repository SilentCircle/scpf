

# Module sc_push_app #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`application`](application.md).

<a name="types"></a>

## Data Types ##




### <a name="type-pv_list">pv_list()</a> ###


<pre><code>
pv_list() = [<a href="#type-pv_tuple">pv_tuple()</a>]
</code></pre>




### <a name="type-pv_tuple">pv_tuple()</a> ###


<pre><code>
pv_tuple() = {term(), term()}
</code></pre>




### <a name="type-start_type">start_type()</a> ###


<pre><code>
start_type() = normal | {takeover, Node::node()} | {failover, Node::node()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#config_change-3">config_change/3</a></td><td>
This function is called by an application after a code replacement, if there
are any changes to the configuration parameters.</td></tr><tr><td valign="top"><a href="#prep_stop-1">prep_stop/1</a></td><td>
This function is called when an application is about to be stopped, before
shutting down the processes of the application.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Start the <code>sc_push</code> application.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop the <code>sc_push</code> application.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="config_change-3"></a>

### config_change/3 ###

<pre><code>
config_change(Changed::<a href="#type-pv_list">pv_list()</a>, New::<a href="#type-pv_list">pv_list()</a>, Removed::list()) -&gt; ok
</code></pre>
<br />

This function is called by an application after a code replacement, if there
are any changes to the configuration parameters. Changed is a list of
parameter-value tuples with all configuration parameters with changed
values, New is a list of parameter-value tuples with all configuration
parameters that have been added, and Removed is a list of all parameters
that have been removed.

<a name="prep_stop-1"></a>

### prep_stop/1 ###

<pre><code>
prep_stop(State::term()) -&gt; NewState::term()
</code></pre>
<br />

This function is called when an application is about to be stopped, before
shutting down the processes of the application. State is the state returned
from Module:start/2, or [] if no state was returned. NewState is any term
and will be passed to Module:stop/1. The function is optional. If it is not
defined, the processes will be terminated and then Module:stop(State) is
called.

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(StartType::<a href="#type-start_type">start_type()</a>, StartArgs::term()) -&gt; {ok, pid(), list()} | {error, term()}
</code></pre>
<br />

Start the `sc_push` application.

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(State::term()) -&gt; any()
</code></pre>
<br />

Stop the `sc_push` application.


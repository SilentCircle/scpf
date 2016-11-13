

# Module sc_push_wm_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Supervisor for the webmachine part of the sc_push application.

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td>supervisor callback.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>API for starting the supervisor.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>API for starting the supervisor.</td></tr><tr><td valign="top"><a href="#upgrade-0">upgrade/0</a></td><td>Add processes if necessary.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Env::[]) -&gt; SupervisorTree
</code></pre>
<br />

supervisor callback.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; ServerRet
</code></pre>
<br />

API for starting the supervisor.

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Env) -&gt; ServerRet
</code></pre>
<br />

API for starting the supervisor.

<a name="upgrade-0"></a>

### upgrade/0 ###

<pre><code>
upgrade() -&gt; ok
</code></pre>
<br />

Add processes if necessary.


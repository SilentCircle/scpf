

# Module sc_push_lib_app #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Application callbacks.

Copyright (c) 2015, 2016 Silent Circle

__Behaviours:__ [`application`](application.md).

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-2">start/2</a></td><td>
This function is called whenever an application is started using
application:start/[1,2], and should start the processes of the
application.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>
This function is called whenever an application has stopped.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(StartType, StartArgs) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>StartType = normal | {takeover, Node} | {failover, Node}</code></li><li><code>StartArgs = term()</code></li><li><code>Result = {ok, Pid} | {ok, Pid, State} | {error, Reason}</code></li><li><code>Node = node()</code></li><li><code>Pid = pid()</code></li><li><code>State = term()</code></li><li><code>Reason = term()</code></li></ul>

This function is called whenever an application is started using
application:start/[1,2], and should start the processes of the
application. If the application is structured according to the OTP
design principles as a supervision tree, this means starting the
top supervisor of the tree.

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(State) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>State = term()</code></li></ul>

This function is called whenever an application has stopped. It
is intended to be the opposite of Module:start/2 and should do
any necessary cleaning up. The return value is ignored.


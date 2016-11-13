

# Module sc_util_app #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Utility functions to manage applications and their configurations.

Copyright (c) 2015 Silent Circle

__Authors:__ Sebastien Merle, Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-app_info">app_info()</a> ###


<pre><code>
app_info() = {Application::atom(), Description::string(), Version::string()}
</code></pre>




### <a name="type-app_info_list">app_info_list()</a> ###


<pre><code>
app_info_list() = [<a href="#type-app_info">app_info()</a>]
</code></pre>




### <a name="type-app_state">app_state()</a> ###


<pre><code>
app_state() = loading | loaded | starting | started | start_p_false | running
</code></pre>




### <a name="type-app_state_info">app_state_info()</a> ###


<pre><code>
app_state_info() = {<a href="#type-app_state">app_state()</a>, <a href="#type-app_info_list">app_info_list()</a>}
</code></pre>




### <a name="type-app_states">app_states()</a> ###


<pre><code>
app_states() = [<a href="#type-app_state_info">app_state_info()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_app_info-0">get_app_info/0</a></td><td>Return states of all applications.</td></tr><tr><td valign="top"><a href="#merge_config-2">merge_config/2</a></td><td>Merge a list of application configurations and a list of overrides.</td></tr><tr><td valign="top"><a href="#start_applications-1">start_applications/1</a></td><td>Starts a list of application.</td></tr><tr><td valign="top"><a href="#start_applications-2">start_applications/2</a></td><td>Start a list of applications, using specified options.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_app_info-0"></a>

### get_app_info/0 ###

<pre><code>
get_app_info() -&gt; <a href="#type-app_states">app_states()</a>
</code></pre>
<br />

Return states of all applications.

<a name="merge_config-2"></a>

### merge_config/2 ###

<pre><code>
merge_config(Config, Overrides) -&gt; Config
</code></pre>

<ul class="definitions"><li><code>Config = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Overrides = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li></ul>

Merge a list of application configurations and a list of overrides.
Guarantee the order of the keys; the keys from the configuration will be
first and in the same order they were specified, then the keys from
the override in the same order they were specified.

Note that this function is not very efficient and should not be used
intensively.

Configuration and override are proplists, e.g:

```
  Config = [{app1, [{foo, 1}, {bar, [{spam, 2}]}, {buz, 3}]}],
  Override = [{app1, [{foo, a}, {biz, b}, {bar, [{spam, c}, {eggs, d}]}]}],
  sc_util_app:merge_config(Config, Override).
  -> [{app1, [{foo, a}, {bar, [{spam, c}, {eggs, d}]}, {buz, 3}, {biz, b}]}]
```

<a name="start_applications-1"></a>

### start_applications/1 ###

<pre><code>
start_applications(Apps) -&gt; {ok, StartedApps} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Apps = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>StartedApps = [atom()]</code></li><li><code>Reason = term()</code></li></ul>

Starts a list of application. Same as start_applications(Apps, []).

<a name="start_applications-2"></a>

### start_applications/2 ###

<pre><code>
start_applications(Apps, Options) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Apps = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Options = [Option]</code></li><li><code>Option = {logger, Logger}</code></li><li><code>Logger = fun((Level, Format, Args) -&gt; ok)</code></li><li><code>Level = info | warning</code></li><li><code>Format = nonempty_string()</code></li><li><code>Args = [term()]</code></li><li><code>Result = {ok, StartedApps} | {error, Reason}</code></li><li><code>StartedApps = [atom()]</code></li><li><code>Reason = term()</code></li></ul>

Start a list of applications, using specified options.
The applications are specified as a proplist like

```
  [{app1_name, app1_config}, {app2_name, app2_config}]
```

like in sys.config.

* All the dependencies will be started in the proper order.

* If a dependency is not specified it will be started without setting
any environment for it.

* If a specified application is already started, it will not be started
again and its environment will not be changed; it will only log a warning.



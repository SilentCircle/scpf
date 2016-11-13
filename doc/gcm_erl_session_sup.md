

# Module gcm_erl_session_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

GCM session supervisor behavior callback module.

Copyright (c) 2015 Silent Circle

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_child_pid-1">get_child_pid/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_child_alive-1">is_child_alive/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_child-2">start_child/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop_child-1">stop_child/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_child_pid-1"></a>

### get_child_pid/1 ###

`get_child_pid(Name) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="is_child_alive-1"></a>

### is_child_alive/1 ###

`is_child_alive(Name) -> any()`

<a name="start_child-2"></a>

### start_child/2 ###

`start_child(Name, Opts) -> any()`

<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Sessions) -> any()`

<a name="stop_child-1"></a>

### stop_child/1 ###

`stop_child(Name) -> any()`


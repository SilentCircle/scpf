

# Module sc_push_wm_version #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

webmachine resource that handles the version endpoint.

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##
See [`https://github.com/basho/webmachine/wiki/Resource-Functions`](https://github.com/basho/webmachine/wiki/Resource-Functions) for a
description of the resource functions used in this module.
<a name="types"></a>

## Data Types ##




### <a name="type-ctx">ctx()</a> ###


<pre><code>
ctx() = #ctx{}
</code></pre>

Context record.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#allowed_methods-2">allowed_methods/2</a></td><td></td></tr><tr><td valign="top"><a href="#content_types_provided-2">content_types_provided/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#resource_exists-2">resource_exists/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_json-2">to_json/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="allowed_methods-2"></a>

### allowed_methods/2 ###

<pre><code>
allowed_methods(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; {list(), <a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

<a name="content_types_provided-2"></a>

### content_types_provided/2 ###

<pre><code>
content_types_provided(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; {list(), <a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Config::<a href="sc_push_wm_helper.md#type-config">sc_push_wm_helper:config()</a>) -&gt; {<a href="sc_push_wm_helper.md#type-debug_info">sc_push_wm_helper:debug_info()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

<a name="resource_exists-2"></a>

### resource_exists/2 ###

<pre><code>
resource_exists(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />

<a name="to_json-2"></a>

### to_json/2 ###

<pre><code>
to_json(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; {<a href="sc_push_wm_helper.md#type-json">sc_push_wm_helper:json()</a>, <a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />


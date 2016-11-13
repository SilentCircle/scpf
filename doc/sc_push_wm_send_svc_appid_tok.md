

# Module sc_push_wm_send_svc_appid_tok #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

webmachine resource that handles the push notification component
of the REST API.

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#allow_missing_post-2">allow_missing_post/2</a></td><td></td></tr><tr><td valign="top"><a href="#allowed_methods-2">allowed_methods/2</a></td><td></td></tr><tr><td valign="top"><a href="#finish_request-2">finish_request/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#malformed_request-2">malformed_request/2</a></td><td></td></tr><tr><td valign="top"><a href="#post_is_create-2">post_is_create/2</a></td><td></td></tr><tr><td valign="top"><a href="#process_post-2">process_post/2</a></td><td></td></tr><tr><td valign="top"><a href="#resource_exists-2">resource_exists/2</a></td><td></td></tr><tr><td valign="top"><a href="#valid_content_headers-2">valid_content_headers/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="allow_missing_post-2"></a>

### allow_missing_post/2 ###

<pre><code>
allow_missing_post(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />

<a name="allowed_methods-2"></a>

### allowed_methods/2 ###

<pre><code>
allowed_methods(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; {list(), <a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

<a name="finish_request-2"></a>

### finish_request/2 ###

<pre><code>
finish_request(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Config::<a href="sc_push_wm_helper.md#type-config">sc_push_wm_helper:config()</a>) -&gt; {<a href="sc_push_wm_helper.md#type-debug_info">sc_push_wm_helper:debug_info()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

<a name="malformed_request-2"></a>

### malformed_request/2 ###

<pre><code>
malformed_request(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />

<a name="post_is_create-2"></a>

### post_is_create/2 ###

<pre><code>
post_is_create(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />

<a name="process_post-2"></a>

### process_post/2 ###

<pre><code>
process_post(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />

<a name="resource_exists-2"></a>

### resource_exists/2 ###

<pre><code>
resource_exists(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />

<a name="valid_content_headers-2"></a>

### valid_content_headers/2 ###

<pre><code>
valid_content_headers(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />


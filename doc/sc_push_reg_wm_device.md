

# Module sc_push_reg_wm_device #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

webmachine resource that handles the device registration resource
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#allowed_methods-2">allowed_methods/2</a></td><td>Define the allowed HTTP methods in a list.</td></tr><tr><td valign="top"><a href="#content_types_accepted-2">content_types_accepted/2</a></td><td>Define the acceptable <code>Content-Type</code>s in a list.</td></tr><tr><td valign="top"><a href="#content_types_provided-2">content_types_provided/2</a></td><td>This should return a list of pairs where each pair is of the form
<code>{Mediatype, Handler}</code> where <code>Mediatype</code> is a string of content-type format and
the <code>Handler</code> is an atom naming the function which can provide a resource
representation in that media type.</td></tr><tr><td valign="top"><a href="#delete_completed-2">delete_completed/2</a></td><td>This is only called after a successful delete_resource call, and should
return <code>false</code> if the deletion was accepted but cannot yet be guaranteed to
have finished.</td></tr><tr><td valign="top"><a href="#delete_resource-2">delete_resource/2</a></td><td>This is called when a <code>DELETE</code> request should be enacted, and should
return <code>true</code> if the deletion succeeded.</td></tr><tr><td valign="top"><a href="#finish_request-2">finish_request/2</a></td><td>This function, if exported, is called just before the final response is
constructed and sent.</td></tr><tr><td valign="top"><a href="#from_json-2">from_json/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>webmachine initialization resource function.</td></tr><tr><td valign="top"><a href="#malformed_request-2">malformed_request/2</a></td><td>This function is called very early on, and checks the incoming request
to determine if it is malformed.</td></tr><tr><td valign="top"><a href="#resource_exists-2">resource_exists/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_json-2">to_json/2</a></td><td>Get registration data as JSON.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="allowed_methods-2"></a>

### allowed_methods/2 ###

<pre><code>
allowed_methods(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; {AllowedMethods::list(), <a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

Define the allowed HTTP methods in a list.  If a Method not in this
list is requested, then a `405 Method Not Allowed` will be sent. Note that
Methods (`'PUT'`, `'GET'`, etc.) are all-caps and are atoms
(single-quoted).


#### <a name="Example_return_value">Example return value</a> ####


```
  {['GET', 'PUT', 'DELETE'], ReqData, Ctx}
```

<a name="content_types_accepted-2"></a>

### content_types_accepted/2 ###

<pre><code>
content_types_accepted(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; {list(), <a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

Define the acceptable `Content-Type`s in a list. This is used similarly
to [`content_types_provided/2`](#content_types_provided-2), except that it is for incoming resource
representations - for example, `PUT` requests. Handler functions usually
want to use `wrq:req_body(ReqData)` to access the incoming request body.


#### <a name="Example_return_value">Example return value</a> ####


```
  {[{"application/json", from_json}], ReqData, Ctx}
```

<a name="content_types_provided-2"></a>

### content_types_provided/2 ###

<pre><code>
content_types_provided(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; {list(), <a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

This should return a list of pairs where each pair is of the form
`{Mediatype, Handler}` where `Mediatype` is a string of content-type format and
the `Handler` is an atom naming the function which can provide a resource
representation in that media type. Content negotiation is driven by this
return value. For example, if a client request includes an `Accept` header
with a value that does not appear as a first element in any of the return
tuples, then a `406 Not Acceptable` will be sent.


#### <a name="Example_return_value">Example return value</a> ####


```
  {[{"application/json", to_json}], ReqData, Ctx}
```

<a name="delete_completed-2"></a>

### delete_completed/2 ###

<pre><code>
delete_completed(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />

This is only called after a successful delete_resource call, and should
return `false` if the deletion was accepted but cannot yet be guaranteed to
have finished.  A `false` return results in a `202 Accepted` HTTP status code,
while a `true` return may cause a `200 OK` or a `204 No Content` or possibly
others, depending on subsequent processing.


#### <a name="Example_returns">Example returns</a> ####


```
  {true, ReqData, Ctx} % Deletes are immediate
```

<a name="delete_resource-2"></a>

### delete_resource/2 ###

<pre><code>
delete_resource(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />

This is called when a `DELETE` request should be enacted, and should
return `true` if the deletion succeeded.


#### <a name="Example_return_value">Example return value</a> ####


```
  {true, ReqData, Ctx}
```

<a name="finish_request-2"></a>

### finish_request/2 ###

<pre><code>
finish_request(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />

This function, if exported, is called just before the final response is
constructed and sent. The `Result` is ignored, so any effect of this function
must be by returning a modified `ReqData`.


#### <a name="Example_return_value">Example return value</a> ####


```
  {true, NewReqData, Ctx}
```

<a name="from_json-2"></a>

### from_json/2 ###

`from_json(ReqData, Ctx) -> any()`

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Config::<a href="sc_push_wm_helper.md#type-config">sc_push_wm_helper:config()</a>) -&gt; {<a href="sc_push_wm_helper.md#type-debug_info">sc_push_wm_helper:debug_info()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

webmachine initialization resource function.

<a name="malformed_request-2"></a>

### malformed_request/2 ###

<pre><code>
malformed_request(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="sc_push_wm_helper.md#type-wbool_ret">sc_push_wm_helper:wbool_ret()</a>
</code></pre>
<br />

This function is called very early on, and checks the incoming request
to determine if it is malformed. For example, a `GET` request that has a
missing or badly-formed required path component is malformed.

This returns `true` if the request is malformed, resulting in a `400 Bad
Request` HTTP status code.


#### <a name="Example_return_value">Example return value</a> ####


```
  {false, ReqData, Ctx} % Request is not malformed.
```

<a name="resource_exists-2"></a>

### resource_exists/2 ###

<pre><code>
resource_exists(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; {Exists::boolean(), <a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

<a name="to_json-2"></a>

### to_json/2 ###

<pre><code>
to_json(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; {<a href="sc_push_wm_helper.md#type-json">sc_push_wm_helper:json()</a>, <a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

Get registration data as JSON. The Body should be either an iolist() or {stream,streambody()}




# Module sc_push_wm_helper #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

webmachine helper functions.

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##
See [`https://github.com/basho/webmachine/wiki/Resource-Functions`](https://github.com/basho/webmachine/wiki/Resource-Functions) for a
description of the resource functions used in this module.
<a name="types"></a>

## Data Types ##




### <a name="type-config">config()</a> ###


<pre><code>
config() = <a href="#type-pl">pl</a>(atom(), term())
</code></pre>

A config property list.



### <a name="type-debug_info">debug_info()</a> ###


<pre><code>
debug_info() = ok | {trace, <a href="#type-filepath">filepath()</a>}
</code></pre>

Webmachine: possible returns from init/1.



### <a name="type-ejson">ejson()</a> ###


<pre><code>
ejson() = <a href="#type-ejson_term">ejson_term()</a>
</code></pre>




### <a name="type-ejson_collection">ejson_collection()</a> ###


<pre><code>
ejson_collection() = <a href="#type-ejson_list">ejson_list()</a> | <a href="#type-ejson_dict">ejson_dict()</a>
</code></pre>




### <a name="type-ejson_dict">ejson_dict()</a> ###


<pre><code>
ejson_dict() = <a href="#type-ejson_empty_dict">ejson_empty_dict()</a> | [<a href="#type-ejson_field">ejson_field()</a>]
</code></pre>




### <a name="type-ejson_empty_dict">ejson_empty_dict()</a> ###


<pre><code>
ejson_empty_dict() = [{}]
</code></pre>




### <a name="type-ejson_field">ejson_field()</a> ###


<pre><code>
ejson_field() = {<a href="#type-ejson_field_name">ejson_field_name()</a>, <a href="#type-ejson_term">ejson_term()</a>}
</code></pre>




### <a name="type-ejson_field_name">ejson_field_name()</a> ###


<pre><code>
ejson_field_name() = atom() | <a href="#type-unicode">unicode()</a>
</code></pre>




### <a name="type-ejson_list">ejson_list()</a> ###


<pre><code>
ejson_list() = [<a href="#type-ejson_term">ejson_term()</a>]
</code></pre>




### <a name="type-ejson_scalar">ejson_scalar()</a> ###


<pre><code>
ejson_scalar() = boolean() | null | number() | <a href="#type-unicode">unicode()</a>
</code></pre>




### <a name="type-ejson_term">ejson_term()</a> ###


<pre><code>
ejson_term() = <a href="#type-ejson_scalar">ejson_scalar()</a> | <a href="#type-ejson_collection">ejson_collection()</a>
</code></pre>




### <a name="type-filepath">filepath()</a> ###


<pre><code>
filepath() = string()
</code></pre>




### <a name="type-json">json()</a> ###


<pre><code>
json() = binary()
</code></pre>




### <a name="type-pl">pl()</a> ###


<pre><code>
pl(KT, VT) = [{KT, VT}]
</code></pre>

Property list.



### <a name="type-unicode">unicode()</a> ###


<pre><code>
unicode() = binary()
</code></pre>




### <a name="type-wbool_ret">wbool_ret()</a> ###


<pre><code>
wbool_ret() = <a href="#type-wret">wret</a>(boolean())
</code></pre>

Webmachine: boolean dispatch function return type.



### <a name="type-werr">werr()</a> ###


<pre><code>
werr() = {error, iolist()}
</code></pre>

Webmachine: dispatch function returns this
to signal 500 error.



### <a name="type-wiolist_ret">wiolist_ret()</a> ###


<pre><code>
wiolist_ret() = <a href="#type-wret">wret</a>(iolist())
</code></pre>

Webmachine: iolist dispatch function return type.



### <a name="type-wret">wret()</a> ###


<pre><code>
wret(T) = {T | <a href="#type-werr">werr()</a>, <a href="#type-wrq">wrq()</a>, any()}
</code></pre>

Webmachine: dispatch functions
return this.



### <a name="type-wrq">wrq()</a> ###


<pre><code>
wrq() = #wm_reqdata{}
</code></pre>

Webmachine: request data record.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_seps-2">add_seps/2</a></td><td></td></tr><tr><td valign="top"><a href="#bad_json_error-0">bad_json_error/0</a></td><td></td></tr><tr><td valign="top"><a href="#check_reg_lookup-1">check_reg_lookup/1</a></td><td>Check the return of the sc_push_reg_api lookup.</td></tr><tr><td valign="top"><a href="#config_debug-1">config_debug/1</a></td><td>Check if config contains <code>{debug, TraceFilePath}</code> and
return {trace, TraceFilePath} if so, <code>ok</code> if not.</td></tr><tr><td valign="top"><a href="#ejson_to_props-1">ejson_to_props/1</a></td><td></td></tr><tr><td valign="top"><a href="#enc_headers-1">enc_headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_props-1">encode_props/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_device_id-1">get_device_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_req_hdr-2">get_req_hdr/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_req_hdr-3">get_req_hdr/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_svc_id-1">get_svc_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_debug-2">is_debug/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_valid_reg_req-1">is_valid_reg_req/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_valid_service_id-2">is_valid_service_id/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_service_id_path-1">make_service_id_path/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_service_id_url-2">make_service_id_url/2</a></td><td>Return service/token URL as binary string.</td></tr><tr><td valign="top"><a href="#make_service_id_urls-2">make_service_id_urls/2</a></td><td>Create a list of binary resource URLs from the property lists
to be returned.</td></tr><tr><td valign="top"><a href="#malformed_err-2">malformed_err/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_json-1">parse_json/1</a></td><td></td></tr><tr><td valign="top"><a href="#pv-3">pv/3</a></td><td></td></tr><tr><td valign="top"><a href="#sanctify-1">sanctify/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_seps-2"></a>

### add_seps/2 ###

`add_seps(List, Sep) -> any()`

<a name="bad_json_error-0"></a>

### bad_json_error/0 ###

`bad_json_error() -> any()`

<a name="check_reg_lookup-1"></a>

### check_reg_lookup/1 ###

`check_reg_lookup(Error) -> any()`

Check the return of the sc_push_reg_api lookup

<a name="config_debug-1"></a>

### config_debug/1 ###

<pre><code>
config_debug(Config::<a href="#type-config">config()</a>) -&gt; <a href="#type-debug_info">debug_info()</a>
</code></pre>
<br />

Check if config contains `{debug, TraceFilePath}` and
return {trace, TraceFilePath} if so, `ok` if not.

<a name="ejson_to_props-1"></a>

### ejson_to_props/1 ###

`ejson_to_props(EJSON) -> any()`

<a name="enc_headers-1"></a>

### enc_headers/1 ###

`enc_headers(T) -> any()`

<a name="encode_props-1"></a>

### encode_props/1 ###

`encode_props(Props) -> any()`

<a name="get_device_id-1"></a>

### get_device_id/1 ###

<pre><code>
get_device_id(ReqData::<a href="#type-wrq">wrq()</a>) -&gt; {ok, any()} | undefined
</code></pre>
<br />

<a name="get_req_hdr-2"></a>

### get_req_hdr/2 ###

<pre><code>
get_req_hdr(HeaderName::string(), ReqData::<a href="#type-wrq">wrq()</a>) -&gt; string() | undefined
</code></pre>
<br />

<a name="get_req_hdr-3"></a>

### get_req_hdr/3 ###

<pre><code>
get_req_hdr(HeaderName::string(), ReqData::<a href="#type-wrq">wrq()</a>, Default::string()) -&gt; string()
</code></pre>
<br />

<a name="get_svc_id-1"></a>

### get_svc_id/1 ###

<pre><code>
get_svc_id(ReqData::<a href="#type-wrq">wrq()</a>) -&gt; {ok, any()} | undefined
</code></pre>
<br />

<a name="is_debug-2"></a>

### is_debug/2 ###

<pre><code>
is_debug(ReqData::<a href="#type-wrq">wrq()</a>, Cfg::list()) -&gt; boolean()
</code></pre>
<br />

<a name="is_valid_reg_req-1"></a>

### is_valid_reg_req/1 ###

<pre><code>
is_valid_reg_req(EJSON::<a href="#type-ejson">ejson()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="is_valid_service_id-2"></a>

### is_valid_service_id/2 ###

`is_valid_service_id(Svc, Token) -> any()`

<a name="make_service_id_path-1"></a>

### make_service_id_path/1 ###

`make_service_id_path(Proplist) -> any()`

<a name="make_service_id_url-2"></a>

### make_service_id_url/2 ###

`make_service_id_url(BaseURI, Proplist) -> any()`

Return service/token URL as binary string.

<a name="make_service_id_urls-2"></a>

### make_service_id_urls/2 ###

`make_service_id_urls(ReqData, Proplists) -> any()`

Create a list of binary resource URLs from the property lists
to be returned. The URLs will be urlencoded.

<a name="malformed_err-2"></a>

### malformed_err/2 ###

<pre><code>
malformed_err(Error::iolist() | binary(), ReqData::<a href="#type-wrq">wrq()</a>) -&gt; <a href="#type-wrq">wrq()</a>
</code></pre>
<br />

<a name="parse_json-1"></a>

### parse_json/1 ###

<pre><code>
parse_json(X1::<a href="#type-json">json()</a>) -&gt; {ok, <a href="#type-ejson">ejson()</a>} | {bad_json, string()}
</code></pre>
<br />

<a name="pv-3"></a>

### pv/3 ###

`pv(K, PL, Def) -> any()`

<a name="sanctify-1"></a>

### sanctify/1 ###

`sanctify(PathComponent) -> any()`


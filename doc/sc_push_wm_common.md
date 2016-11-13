

# Module sc_push_wm_common #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_result-2">add_result/2</a></td><td></td></tr><tr><td valign="top"><a href="#bad_request-3">bad_request/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode_ref-1">encode_ref/1</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_path_item-2">ensure_path_item/2</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_path_items-2">ensure_path_items/2</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_path_items-3">ensure_path_items/3</a></td><td></td></tr><tr><td valign="top"><a href="#error_to_json-1">error_to_json/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_media_type-1">get_media_type/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_json-1">parse_json/1</a></td><td></td></tr><tr><td valign="top"><a href="#result_to_json-1">result_to_json/1</a></td><td></td></tr><tr><td valign="top"><a href="#results_to_json-1">results_to_json/1</a></td><td></td></tr><tr><td valign="top"><a href="#send_push-1">send_push/1</a></td><td></td></tr><tr><td valign="top"><a href="#store_prop-2">store_prop/2</a></td><td></td></tr><tr><td valign="top"><a href="#store_props-2">store_props/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_result-2"></a>

### add_result/2 ###

<pre><code>
add_result(ReqData::<a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>, Results::list()) -&gt; <a href="sc_push_wm_helper.md#type-wrq">sc_push_wm_helper:wrq()</a>
</code></pre>
<br />

<a name="bad_request-3"></a>

### bad_request/3 ###

`bad_request(ReqData, Ctx, Msg) -> any()`

<a name="encode_ref-1"></a>

### encode_ref/1 ###

<pre><code>
encode_ref(Ref::term()) -&gt; binary()
</code></pre>
<br />

<a name="ensure_path_item-2"></a>

### ensure_path_item/2 ###

`ensure_path_item(Key, ReqData) -> any()`

<a name="ensure_path_items-2"></a>

### ensure_path_items/2 ###

`ensure_path_items(Keys, ReqData) -> any()`

<a name="ensure_path_items-3"></a>

### ensure_path_items/3 ###

`ensure_path_items(Rest, ReqData, Acc) -> any()`

<a name="error_to_json-1"></a>

### error_to_json/1 ###

`error_to_json(L) -> any()`

<a name="get_media_type-1"></a>

### get_media_type/1 ###

<pre><code>
get_media_type(CT::string()) -&gt; string()
</code></pre>
<br />

<a name="parse_json-1"></a>

### parse_json/1 ###

<pre><code>
parse_json(ReqBody::binary()) -&gt; {ok, <a href="sc_types.md#type-proplist">sc_types:proplist</a>(atom(), string() | binary())} | {error, binary()}
</code></pre>
<br />

<a name="result_to_json-1"></a>

### result_to_json/1 ###

`result_to_json(X1) -> any()`

<a name="results_to_json-1"></a>

### results_to_json/1 ###

`results_to_json(Results) -> any()`

<a name="send_push-1"></a>

### send_push/1 ###

<pre><code>
send_push(Notification) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Notification = <a href="sc_push.md#type-notification">sc_push:notification()</a></code></li><li><code>Result = {ok, term()} | {error, binary()}</code></li></ul>

<a name="store_prop-2"></a>

### store_prop/2 ###

`store_prop(NewProp, PL) -> any()`

<a name="store_props-2"></a>

### store_props/2 ###

`store_props(Props, PL) -> any()`


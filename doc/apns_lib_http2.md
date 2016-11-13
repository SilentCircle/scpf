

# Module apns_lib_http2 #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

APNS HTTP/2 support library.

Copyright (c) 2015-2016 Silent Circle

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##
This is the next format after the v2 binary format, v3 if you like.
See [`https://developer.apple.com`](https://developer.apple.com) for more information.
<a name="types"></a>

## Data Types ##




### <a name="type-base64_urlencoded">base64_urlencoded()</a> ###


<pre><code>
base64_urlencoded() = <a href="#type-bstring">bstring()</a>
</code></pre>




### <a name="type-bstring">bstring()</a> ###


<pre><code>
bstring() = binary()
</code></pre>




### <a name="type-http2_hdr">http2_hdr()</a> ###


<pre><code>
http2_hdr() = {Key::<a href="#type-bstring">bstring()</a>, Val::<a href="#type-bstring">bstring()</a>}
</code></pre>




### <a name="type-http2_hdrs">http2_hdrs()</a> ###


<pre><code>
http2_hdrs() = [<a href="#type-http2_hdr">http2_hdr()</a>]
</code></pre>




### <a name="type-http2_req">http2_req()</a> ###


<pre><code>
http2_req() = {<a href="#type-http2_hdrs">http2_hdrs()</a>, <a href="#type-http2_req_body">http2_req_body()</a>}
</code></pre>




### <a name="type-http2_req_body">http2_req_body()</a> ###


<pre><code>
http2_req_body() = <a href="#type-bstring">bstring()</a>
</code></pre>




### <a name="type-http2_rsp">http2_rsp()</a> ###


<pre><code>
http2_rsp() = {<a href="#type-http2_hdrs">http2_hdrs()</a>, <a href="#type-http2_rsp_body">http2_rsp_body()</a>}
</code></pre>




### <a name="type-http2_rsp_body">http2_rsp_body()</a> ###


<pre><code>
http2_rsp_body() = undefined | [<a href="#type-bstring">bstring()</a>]
</code></pre>




### <a name="type-jwt">jwt()</a> ###


<pre><code>
jwt() = <a href="#type-base64_urlencoded">base64_urlencoded()</a>
</code></pre>




### <a name="type-parsed_rsp">parsed_rsp()</a> ###


<pre><code>
parsed_rsp() = [<a href="#type-parsed_rsp_val">parsed_rsp_val()</a>]
</code></pre>




### <a name="type-parsed_rsp_val">parsed_rsp_val()</a> ###


<pre><code>
parsed_rsp_val() = {uuid, <a href="#type-uuid_str">uuid_str()</a>} | {status, <a href="#type-bstring">bstring()</a>} | {status_desc, <a href="#type-bstring">bstring()</a>} | {reason, <a href="#type-bstring">bstring()</a>} | {reason_desc, <a href="#type-bstring">bstring()</a>} | {timestamp, non_neg_integer() | undefined} | {timestamp_desc, <a href="#type-bstring">bstring()</a> | undefined} | {body, term()}
</code></pre>




### <a name="type-req_opt">req_opt()</a> ###


<pre><code>
req_opt() = {authorization, <a href="#type-jwt">jwt()</a>} | {uuid, <a href="#type-uuid_str">uuid_str()</a>} | {expiration, non_neg_integer()} | {priority, non_neg_integer()} | {topic, <a href="#type-bstring">bstring()</a>} | {collapse_id, <a href="#type-bstring">bstring()</a>} | {thread_id, <a href="#type-bstring">bstring()</a>}
</code></pre>




### <a name="type-req_opts">req_opts()</a> ###


<pre><code>
req_opts() = [<a href="#type-req_opt">req_opt()</a>]
</code></pre>




### <a name="type-uuid_str">uuid_str()</a> ###


<pre><code>
uuid_str() = <a href="#type-bstring">bstring()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#host_port-1">host_port/1</a></td><td>Returns a default <code>{Host, Port}</code> for <code>prod</code> or <code>dev</code> APNS environment.</td></tr><tr><td valign="top"><a href="#make_req-3">make_req/3</a></td><td>Create an HTTP/2 request ready to send.</td></tr><tr><td valign="top"><a href="#make_req_hdrs-4">make_req_hdrs/4</a></td><td>Create HTTP/2 request headers for an APNS request.</td></tr><tr><td valign="top"><a href="#make_ssl_opts-2">make_ssl_opts/2</a></td><td>Return default SSL options for APNS HTTP/2.</td></tr><tr><td valign="top"><a href="#make_uuid-0">make_uuid/0</a></td><td>Make a UUID suitable for APNS id header.</td></tr><tr><td valign="top"><a href="#parse_resp-1">parse_resp/1</a></td><td>Parse HTTP/2 response body and headers.</td></tr><tr><td valign="top"><a href="#parse_resp_body-1">parse_resp_body/1</a></td><td>Parse APNS HTTP/2 response body.</td></tr><tr><td valign="top"><a href="#reason_desc-1">reason_desc/1</a></td><td>Map APNS HTTP/2 reason to text description.</td></tr><tr><td valign="top"><a href="#status_desc-1">status_desc/1</a></td><td>Map HTTP/2 status code to textual description.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="host_port-1"></a>

### host_port/1 ###

<pre><code>
host_port(Env) -&gt; HostPort
</code></pre>

<ul class="definitions"><li><code>Env = prod | dev</code></li><li><code>HostPort = {Host, Port}</code></li><li><code>Host = string()</code></li><li><code>Port = non_neg_integer()</code></li></ul>

Returns a default `{Host, Port}` for `prod` or `dev` APNS environment.

<a name="make_req-3"></a>

### make_req/3 ###

<pre><code>
make_req(Token, JSON, Opts) -&gt; Req
</code></pre>

<ul class="definitions"><li><code>Token = string() | <a href="#type-bstring">bstring()</a></code></li><li><code>JSON = string() | <a href="#type-bstring">bstring()</a></code></li><li><code>Opts = <a href="#type-req_opts">req_opts()</a></code></li><li><code>Req = <a href="#type-http2_req">http2_req()</a></code></li></ul>

Create an HTTP/2 request ready to send.


#### <a name="Parameters">Parameters</a> ####



<dt><code>Token</code></dt>




<dd>The APNS token as a hexadecimal binary string.</dd>



<dt><code>JSON</code></dt>




<dd>The formatted JSON payload as a binary string.</dd>



<dt><code>Opts</code></dt>




<dd>A proplist containing one or more of the following:
<dl>
<dt><code>{authorization, jwt()}</code></dt>
<dd>The provider token that authorizes APNs to send push
notifications for the specified topics. The token is in
Base64URL-encoded JWT format.  When the provider certificate is
used to establish a connection, this request header is
ignored.</dd><p></p><dt><code>{uuid, uuid_str()}</code></dt>
<dd>A canonical UUID that identifies the notification. If there is
an error sending the notification, APNs uses this value to
identify the notification to your server.  The canonical form is
<b>32 lowercase hexadecimal digits, displayed in five groups
separated by hyphens in the form 8-4-4-4-12</b>.  An example UUID
is <code>123e4567-e89b-12d3-a456-42665544000</code>.  If you omit this
header, a new UUID is created by APNs and returned in the
response.</dd><p></p><dt><code>{expiration, non_neg_integer()}</code></dt>
<dd>A UNIX epoch date expressed in seconds (UTC). This header
identifies the date when the notification is no longer valid and
can be discarded.  If this value is nonzero, APNs stores the
notification and tries to deliver it at least once, repeating the
attempt as needed if it is unable to deliver the notification the
first time. If the value is <code>0</code>, APNs treats the notification as
if it expires immediately and does not store the notification or
attempt to redeliver it.</dd><p></p><dt><code>{priority, non_neg_integer()}</code></dt>
<dd>The priority of the notification.  Specify one of the following
values:
<ul>
<li><code>10</code>–Send the push message immediately. Notifications with
this priority must trigger an alert, sound, or badge on the
target device. It is an error to use this priority for a push
notification that contains only the content-available
key.</li><p></p><li><code>5</code>—Send the push message at a time that takes into
account power considerations for the device. Notifications
with this priority might be grouped and delivered in bursts.
They are throttled, and in some cases are not delivered.  If
you omit this header, the APNs server sets the priority to
<code>10</code>.</li>
</ul>
</dd><p></p><dt><code>{topic, string() | bstring()}</code></dt>
<dd>The topic of the remote notification, which is typically the
bundle ID for your app. The certificate you create in Member
Center must include the capability for this topic.  If your
certificate includes multiple topics, you must specify a value for
this header.  If you omit this header and your APNs certificate
does not specify multiple topics, the APNs server uses the
certificate’s Subject as the default topic.</dd><p></p><dt><code>{collapse_id, string() | bstring()}</code></dt>
<dd>Multiple notifications with same collapse identifier are
displayed to the user as a single notification. The value should
not exceed 64 bytes.</dd><p></p><dt><code>{thread_id, string() | bstring()}</code></dt>
<dd>When displaying notifications, the system visually groups
notifications with the same thread identifier together.  For
remote notifications, the value of the <code>threadIdentifier</code>
property is set to the value of this request header.</dd>
</dl>
</dd>



<dt><code>APNSId</code></dt>




<dd>A unique id for this request</dd>



Returns `{http2_hdrs(), http2_req_body()}`.

__See also:__ [apns_json](apns_json.md).

<a name="make_req_hdrs-4"></a>

### make_req_hdrs/4 ###

<pre><code>
make_req_hdrs(Method, Path, Scheme, Opts) -&gt; Headers
</code></pre>

<ul class="definitions"><li><code>Method = string() | <a href="#type-bstring">bstring()</a></code></li><li><code>Path = string() | <a href="#type-bstring">bstring()</a></code></li><li><code>Scheme = string() | <a href="#type-bstring">bstring()</a></code></li><li><code>Opts = <a href="#type-req_opts">req_opts()</a></code></li><li><code>Headers = <a href="#type-http2_hdrs">http2_hdrs()</a></code></li></ul>

Create HTTP/2 request headers for an APNS request.
`Opts` is a property list of supported optional headers.

__See also:__ [make_req/3](#make_req-3).

<a name="make_ssl_opts-2"></a>

### make_ssl_opts/2 ###

<pre><code>
make_ssl_opts(CertFile, KeyFile) -&gt; Opts
</code></pre>

<ul class="definitions"><li><code>CertFile = string()</code></li><li><code>KeyFile = string()</code></li><li><code>Opts = [{atom(), term()}]</code></li></ul>

Return default SSL options for APNS HTTP/2.

<a name="make_uuid-0"></a>

### make_uuid/0 ###

<pre><code>
make_uuid() -&gt; <a href="#type-uuid_str">uuid_str()</a>
</code></pre>
<br />

Make a UUID suitable for APNS id header.

The return value is a binary string comprising 32 lowercase hexadecimal
digits, displayed in five groups separated by hyphens in the form
8-4-4-4-12.


### <a name="Example">Example</a> ###

```
  >make_uuid().
  <<"123e4567-e89b-12d3-a456-42665544000">>
```


<a name="parse_resp-1"></a>

### parse_resp/1 ###

<pre><code>
parse_resp(Resp) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Resp = <a href="#type-http2_rsp">http2_rsp()</a></code></li><li><code>Result = <a href="#type-parsed_rsp">parsed_rsp()</a></code></li></ul>

Parse HTTP/2 response body and headers.
Return proplist with parsed body, uuid, status, and other information.

<a name="parse_resp_body-1"></a>

### parse_resp_body/1 ###

<pre><code>
parse_resp_body(RespBody) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>RespBody = <a href="#type-http2_rsp_body">http2_rsp_body()</a></code></li><li><code>Reason = <a href="#type-bstring">bstring()</a></code></li><li><code>Timestamp = undefined | non_neg_integer()</code></li><li><code>EJSON = <a href="apns_json.md#type-json_term">apns_json:json_term()</a></code></li><li><code>Result = [] | [{Reason, EJSON}] | [{Reason, Timestamp, EJSON}]</code></li></ul>

Parse APNS HTTP/2 response body.

<a name="reason_desc-1"></a>

### reason_desc/1 ###

<pre><code>
reason_desc(Reason) -&gt; Desc
</code></pre>

<ul class="definitions"><li><code>Reason = <a href="#type-bstring">bstring()</a></code></li><li><code>Desc = <a href="#type-bstring">bstring()</a></code></li></ul>

Map APNS HTTP/2 reason to text description.

<a name="status_desc-1"></a>

### status_desc/1 ###

<pre><code>
status_desc(Status) -&gt; Desc
</code></pre>

<ul class="definitions"><li><code>Status = <a href="#type-bstring">bstring()</a></code></li><li><code>Desc = <a href="#type-bstring">bstring()</a></code></li></ul>

Map HTTP/2 status code to textual description.


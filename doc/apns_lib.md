

# Module apns_lib #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

APNS wire-format encoding and decoding library.

Copyright (c) 2015 Silent Circle LLC

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##
This supports the simple (0), enhanced (1), and "v2" (2) formats.
<a name="types"></a>

## Data Types ##




### <a name="type-apns_error">apns_error()</a> ###


<pre><code>
apns_error() = term()
</code></pre>




### <a name="type-apns_notification">apns_notification()</a> ###


<pre><code>
apns_notification() = term()
</code></pre>




### <a name="type-apns_packet">apns_packet()</a> ###


<pre><code>
apns_packet() = binary()
</code></pre>




### <a name="type-bytes">bytes()</a> ###


<pre><code>
bytes() = [byte()]
</code></pre>




### <a name="type-decode_err_pkt_error">decode_err_pkt_error()</a> ###


<pre><code>
decode_err_pkt_error() = {error, <a href="#type-decode_err_pkt_reason">decode_err_pkt_reason()</a>}
</code></pre>




### <a name="type-decode_err_pkt_reason">decode_err_pkt_reason()</a> ###


<pre><code>
decode_err_pkt_reason() = bad_packet
</code></pre>




### <a name="type-decode_error">decode_error()</a> ###


<pre><code>
decode_error() = {error, <a href="#type-decode_reason">decode_reason()</a>}
</code></pre>




### <a name="type-decode_reason">decode_reason()</a> ###


<pre><code>
decode_reason() = bad_packet | buffer_too_short | bad_json
</code></pre>




### <a name="type-decoded_packet">decoded_packet()</a> ###


<pre><code>
decoded_packet() = {Timestamp::integer(), Token::binary()}
</code></pre>




### <a name="type-encode_error">encode_error()</a> ###


<pre><code>
encode_error() = {error, <a href="#type-encode_reason">encode_reason()</a>}
</code></pre>




### <a name="type-encode_reason">encode_reason()</a> ###


<pre><code>
encode_reason() = bad_token | bad_json | payload_too_long
</code></pre>




### <a name="type-json">json()</a> ###


<pre><code>
json() = string() | binary()
</code></pre>




### <a name="type-token">token()</a> ###


<pre><code>
token() = string() | <a href="#type-bytes">bytes()</a> | binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Decode an encoded APNS packet.</td></tr><tr><td valign="top"><a href="#decode_error_packet-1">decode_error_packet/1</a></td><td>Decode an error received from APNS.</td></tr><tr><td valign="top"><a href="#decode_feedback_packet-1">decode_feedback_packet/1</a></td><td>Decode a feedback packet received from APNS feedback service.</td></tr><tr><td valign="top"><a href="#encode_enhanced-4">encode_enhanced/4</a></td><td>Encode the <code>Id</code>, <code>Expiry</code>, <code>Token</code> and <code>Payload</code> into an
"enhanced" (command 1) APNS packet.</td></tr><tr><td valign="top"><a href="#encode_simple-2">encode_simple/2</a></td><td>Encode <code>Token</code> and <code>Payload</code> into a "simple" (command 0) APNS
packet.</td></tr><tr><td valign="top"><a href="#encode_v2-5">encode_v2/5</a></td><td>Encode into the command 3 APNS packet.</td></tr><tr><td valign="top"><a href="#error_description-1">error_description/1</a></td><td>Convert APNS error code to textual description (as binary
string).</td></tr><tr><td valign="top"><a href="#error_to_atom-1">error_to_atom/1</a></td><td>Convert APNS error code to symbolic name (an atom).</td></tr><tr><td valign="top"><a href="#maybe_encode_token-1">maybe_encode_token/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(Packet) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Packet = binary()</code></li><li><code>Result = <a href="#type-apns_notification">apns_notification()</a> | <a href="#type-decode_error">decode_error()</a></code></li></ul>

Decode an encoded APNS packet.

<a name="decode_error_packet-1"></a>

### decode_error_packet/1 ###

<pre><code>
decode_error_packet(ErrPkt) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>ErrPkt = iolist() | binary()</code></li><li><code>Result = <a href="#type-apns_error">apns_error()</a> | <a href="#type-decode_err_pkt_error">decode_err_pkt_error()</a></code></li></ul>

Decode an error received from APNS.

<a name="decode_feedback_packet-1"></a>

### decode_feedback_packet/1 ###

<pre><code>
decode_feedback_packet(Packet) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Packet = list() | binary()</code></li><li><code>Result = [<a href="#type-decoded_packet">decoded_packet()</a>]</code></li></ul>

Decode a feedback packet received from APNS feedback service.

<a name="encode_enhanced-4"></a>

### encode_enhanced/4 ###

<pre><code>
encode_enhanced(Id, Expiry, Token, Payload) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Id = integer()</code></li><li><code>Expiry = integer()</code></li><li><code>Token = <a href="#type-token">token()</a></code></li><li><code>Payload = <a href="#type-json">json()</a></code></li><li><code>Result = <a href="#type-apns_packet">apns_packet()</a> | <a href="#type-encode_error">encode_error()</a></code></li></ul>

Encode the `Id`, `Expiry`, `Token` and `Payload` into an
"enhanced" (command 1) APNS packet.

<a name="encode_simple-2"></a>

### encode_simple/2 ###

<pre><code>
encode_simple(Token, Payload) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Token = <a href="#type-token">token()</a></code></li><li><code>Payload = <a href="#type-json">json()</a></code></li><li><code>Result = <a href="#type-apns_packet">apns_packet()</a> | <a href="#type-encode_error">encode_error()</a></code></li></ul>

Encode `Token` and `Payload` into a "simple" (command 0) APNS
packet.

<a name="encode_v2-5"></a>

### encode_v2/5 ###

<pre><code>
encode_v2(Id, Expiry, Token, Payload, Prio) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Id = integer()</code></li><li><code>Expiry = integer()</code></li><li><code>Token = <a href="#type-token">token()</a></code></li><li><code>Payload = <a href="#type-json">json()</a></code></li><li><code>Prio = integer()</code></li><li><code>Result = <a href="#type-apns_packet">apns_packet()</a> | <a href="#type-encode_error">encode_error()</a></code></li></ul>

Encode into the command 3 APNS packet.

<a name="error_description-1"></a>

### error_description/1 ###

<pre><code>
error_description(Err) -&gt; Desc
</code></pre>

<ul class="definitions"><li><code>Err = integer()</code></li><li><code>Desc = binary()</code></li></ul>

Convert APNS error code to textual description (as binary
string).

<a name="error_to_atom-1"></a>

### error_to_atom/1 ###

<pre><code>
error_to_atom(Err) -&gt; Atom
</code></pre>

<ul class="definitions"><li><code>Err = 0..255</code></li><li><code>Atom = atom()</code></li></ul>

Convert APNS error code to symbolic name (an atom).

<a name="maybe_encode_token-1"></a>

### maybe_encode_token/1 ###

`maybe_encode_token(L) -> any()`


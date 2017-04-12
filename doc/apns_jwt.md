

# Module apns_jwt #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module supports the creation and validation of APNS
authorization tokens (JWTs).

Copyright (c) 2016 Silent Circle

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-alg">alg()</a> ###


<pre><code>
alg() = <a href="#type-bstring">bstring()</a>
</code></pre>




### <a name="type-apns_jwt_ctx">apns_jwt_ctx()</a> ###


<pre><code>
apns_jwt_ctx() = #apns_jwt_ctx{}
</code></pre>




### <a name="type-base64_urlencoded">base64_urlencoded()</a> ###


<pre><code>
base64_urlencoded() = <a href="#type-bstring">bstring()</a>
</code></pre>




### <a name="type-bstring">bstring()</a> ###


<pre><code>
bstring() = binary()
</code></pre>




### <a name="type-context">context()</a> ###


__abstract datatype__: `context()`




### <a name="type-ec_private_key">ec_private_key()</a> ###


<pre><code>
ec_private_key() = #'ECPrivateKey'{}
</code></pre>




### <a name="type-iat">iat()</a> ###


<pre><code>
iat() = <a href="#type-posix_time">posix_time()</a>
</code></pre>




### <a name="type-input_context">input_context()</a> ###


<pre><code>
input_context() = <a href="#type-context">context()</a> | <a href="#type-apns_jwt_ctx">apns_jwt_ctx()</a>
</code></pre>




### <a name="type-iss">iss()</a> ###


<pre><code>
iss() = <a href="#type-bstring">bstring()</a>
</code></pre>




### <a name="type-jose_header">jose_header()</a> ###


<pre><code>
jose_header() = <a href="#type-json">json()</a>
</code></pre>




### <a name="type-json">json()</a> ###


<pre><code>
json() = <a href="jsx.md#type-json_text">jsx:json_text()</a>
</code></pre>




### <a name="type-jws_payload">jws_payload()</a> ###


<pre><code>
jws_payload() = <a href="#type-json">json()</a>
</code></pre>




### <a name="type-jws_signature">jws_signature()</a> ###


<pre><code>
jws_signature() = <a href="#type-base64_urlencoded">base64_urlencoded()</a>
</code></pre>




### <a name="type-jws_signing_input">jws_signing_input()</a> ###


<pre><code>
jws_signing_input() = <a href="#type-bstring">bstring()</a>
</code></pre>




### <a name="type-jwt">jwt()</a> ###


<pre><code>
jwt() = <a href="#type-bstring">bstring()</a>
</code></pre>




### <a name="type-kid">kid()</a> ###


<pre><code>
kid() = <a href="#type-bstring">bstring()</a>
</code></pre>




### <a name="type-pem_encoded_key">pem_encoded_key()</a> ###


<pre><code>
pem_encoded_key() = <a href="#type-bstring">bstring()</a>
</code></pre>




### <a name="type-posix_time">posix_time()</a> ###


<pre><code>
posix_time() = pos_integer()
</code></pre>




### <a name="type-signing_key">signing_key()</a> ###


<pre><code>
signing_key() = <a href="#type-pem_encoded_key">pem_encoded_key()</a> | <a href="#type-ec_private_key">ec_private_key()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#base64urldecode-1">base64urldecode/1</a></td><td>
Base64urldecode <code>Bin</code>, which may or may not have padding.</td></tr><tr><td valign="top"><a href="#base64urlencode-1">base64urlencode/1</a></td><td>
Base64urlencode <code>Bin</code>, without padding.</td></tr><tr><td valign="top"><a href="#decode_jwt-1">decode_jwt/1</a></td><td>Decode a JWT into <code>{Header, Payload, Signature, SigInput}</code>.</td></tr><tr><td valign="top"><a href="#generate_private_key-0">generate_private_key/0</a></td><td>Generate a private key.</td></tr><tr><td valign="top"><a href="#get_private_key-1">get_private_key/1</a></td><td>Transform a pem-encoded PKCS8 binary to a private key structure.</td></tr><tr><td valign="top"><a href="#iss-1">iss/1</a></td><td>Accessor for iss.</td></tr><tr><td valign="top"><a href="#jwt-1">jwt/1</a></td><td>Equivalent to <tt>jwt</tt>.</td></tr><tr><td valign="top"><a href="#jwt-3">jwt/3</a></td><td>Create a JWT for APNS usage, using the current erlang system time.</td></tr><tr><td valign="top"><a href="#key-1">key/1</a></td><td>Accessor for key.</td></tr><tr><td valign="top"><a href="#kid-1">kid/1</a></td><td>Accessor for kid.</td></tr><tr><td valign="top"><a href="#named_curve-0">named_curve/0</a></td><td>
Return the named elliptic curve tuple for <code>secp256r1</code>.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create a signing context from the parameters passed.</td></tr><tr><td valign="top"><a href="#public_key-1">public_key/1</a></td><td>Extract an EC public key from context or private key.</td></tr><tr><td valign="top"><a href="#sign-3">sign/3</a></td><td>Sign a JWT given the JSON header and payload, and the private key.</td></tr><tr><td valign="top"><a href="#verify-2">verify/2</a></td><td>Verify a JWT using a context.</td></tr><tr><td valign="top"><a href="#verify-4">verify/4</a></td><td>Verify a JWT using the kid, iss, and signing key.</td></tr><tr><td valign="top"><a href="#verify_jwt-2">verify_jwt/2</a></td><td>
Verify a JWT as decoded by <code>decode_jwt/1</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="base64urldecode-1"></a>

### base64urldecode/1 ###

<pre><code>
base64urldecode(Bin) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Bin = <a href="#type-base64_urlencoded">base64_urlencoded()</a></code></li><li><code>Result = binary()</code></li></ul>

Base64urldecode `Bin`, which may or may not have padding.

<a name="base64urlencode-1"></a>

### base64urlencode/1 ###

<pre><code>
base64urlencode(Bin) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Bin = binary()</code></li><li><code>Result = <a href="#type-base64_urlencoded">base64_urlencoded()</a></code></li></ul>

Base64urlencode `Bin`, without padding.

<a name="decode_jwt-1"></a>

### decode_jwt/1 ###

<pre><code>
decode_jwt(JWT) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>JWT = <a href="#type-jwt">jwt()</a></code></li><li><code>Result = {Header, Payload, Signature, SigInput} | {error, invalid_jwt}</code></li><li><code>Header = <a href="jsx.md#type-json_term">jsx:json_term()</a></code></li><li><code>Payload = <a href="jsx.md#type-json_term">jsx:json_term()</a></code></li><li><code>Signature = binary()</code></li><li><code>SigInput = binary()</code></li></ul>

Decode a JWT into `{Header, Payload, Signature, SigInput}`.
`Header` and `Payload` are both decoded JSON as returned by
`jsx:decode/1`, and `Signature` is the binary signature of the
JWT.

`SigInput` is the input to the cryptographic signature validation, and is
the base64urlencoded JWT header concatenated with `"."` and the
base64urlencoded JWT payload, e.g.

The JWT is not validated.

Returns `{Header, Payload, Signature}` or '{error, invalid_jwt}'.

<a name="generate_private_key-0"></a>

### generate_private_key/0 ###

<pre><code>
generate_private_key() -&gt; <a href="#type-ec_private_key">ec_private_key()</a>
</code></pre>
<br />

Generate a private key. This is mostly useful for testing.

<a name="get_private_key-1"></a>

### get_private_key/1 ###

<pre><code>
get_private_key(SigningKeyPem) -&gt; PrivateKey
</code></pre>

<ul class="definitions"><li><code>SigningKeyPem = <a href="#type-pem_encoded_key">pem_encoded_key()</a></code></li><li><code>PrivateKey = <a href="#type-ec_private_key">ec_private_key()</a></code></li></ul>

Transform a pem-encoded PKCS8 binary to a private key structure.

<a name="iss-1"></a>

### iss/1 ###

<pre><code>
iss(Context) -&gt; Iss
</code></pre>

<ul class="definitions"><li><code>Context = <a href="#type-input_context">input_context()</a></code></li><li><code>Iss = <a href="#type-iss">iss()</a></code></li></ul>

Accessor for iss.

<a name="jwt-1"></a>

### jwt/1 ###

<pre><code>
jwt(Context) -&gt; JWT
</code></pre>

<ul class="definitions"><li><code>Context = <a href="#type-input_context">input_context()</a></code></li><li><code>JWT = <a href="#type-jwt">jwt()</a></code></li></ul>

Equivalent to `jwt`.

<a name="jwt-3"></a>

### jwt/3 ###

<pre><code>
jwt(KID, Issuer, SigningKey) -&gt; JWT
</code></pre>

<ul class="definitions"><li><code>KID = <a href="#type-kid">kid()</a></code></li><li><code>Issuer = <a href="#type-iss">iss()</a></code></li><li><code>SigningKey = <a href="#type-signing_key">signing_key()</a></code></li><li><code>JWT = <a href="#type-jwt">jwt()</a></code></li></ul>

Create a JWT for APNS usage, using the current erlang system time.
This is signed with ECDSA using the P-256 curve and the ES256 algorithm.


#### <a name="Parameters">Parameters</a> ####



<dd><code>KID :: binary()</code></dd>




<dt>This is the key ID of the private APNS key downloaded from the Apple
developer portal.</dt>




<dd><code>Issuer :: binary()</code></dd>




<dt>This is the Apple Team ID from the Apple developer portal.</dt>




<dd><code>SigningKey :: signing_key()</code></dd>




<dt>This is the private key downloaded from the Apple
developer portal, either PEM-encoded as downloaded, or as
an <code>#'ECPrivateKey{}'</code> record.</dt>



<a name="key-1"></a>

### key/1 ###

<pre><code>
key(Context) -&gt; Key
</code></pre>

<ul class="definitions"><li><code>Context = <a href="#type-input_context">input_context()</a></code></li><li><code>Key = <a href="#type-signing_key">signing_key()</a></code></li></ul>

Accessor for key.

<a name="kid-1"></a>

### kid/1 ###

<pre><code>
kid(Context) -&gt; KID
</code></pre>

<ul class="definitions"><li><code>Context = <a href="#type-input_context">input_context()</a></code></li><li><code>KID = <a href="#type-kid">kid()</a></code></li></ul>

Accessor for kid.

<a name="named_curve-0"></a>

### named_curve/0 ###

<pre><code>
named_curve() -&gt; {namedCurve, OID::tuple()}
</code></pre>
<br />

Return the named elliptic curve tuple for `secp256r1`.

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(KID, Issuer, SigningKey) -&gt; Context
</code></pre>

<ul class="definitions"><li><code>KID = <a href="#type-kid">kid()</a></code></li><li><code>Issuer = <a href="#type-iss">iss()</a></code></li><li><code>SigningKey = <a href="#type-signing_key">signing_key()</a></code></li><li><code>Context = <a href="#type-context">context()</a></code></li></ul>

Create a signing context from the parameters passed. This can
be used later to create a JWT.


#### <a name="Parameters">Parameters</a> ####



<dd><code>KID :: binary()</code></dd>




<dt>This is the key ID of the private APNS key downloaded from the Apple
developer portal.</dt>




<dd><code>Issuer :: binary()</code></dd>




<dt>This is the Apple Team ID from the Apple developer portal.</dt>




<dd><code>SigningKey :: signing_key()</code></dd>




<dt>This is the PEM-encoded private key downloaded from the Apple
developer portal.</dt>



<a name="public_key-1"></a>

### public_key/1 ###

<pre><code>
public_key(Opaque) -&gt; PublicKey
</code></pre>

<ul class="definitions"><li><code>Opaque = <a href="#type-ec_private_key">ec_private_key()</a> | <a href="#type-input_context">input_context()</a></code></li><li><code>PublicKey = {#'ECPoint'{}, {namedCurve, tuple()}}</code></li></ul>

Extract an EC public key from context or private key.

<a name="sign-3"></a>

### sign/3 ###

<pre><code>
sign(JsonHdr, JsonPayload, Key) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>JsonHdr = <a href="#type-jose_header">jose_header()</a></code></li><li><code>JsonPayload = <a href="#type-jws_payload">jws_payload()</a></code></li><li><code>Key = <a href="#type-ec_private_key">ec_private_key()</a></code></li><li><code>Result = <a href="#type-jws_signature">jws_signature()</a></code></li></ul>

Sign a JWT given the JSON header and payload, and the private key.
The header and payload must not be base64urlencoded.

<a name="verify-2"></a>

### verify/2 ###

<pre><code>
verify(JWT, Ctx) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>JWT = <a href="#type-jwt">jwt()</a></code></li><li><code>Ctx = <a href="#type-input_context">input_context()</a></code></li><li><code>Result = ok | {error, Reason}</code></li><li><code>Reason = term()</code></li></ul>

Verify a JWT using a context.
Return `ok` on success, and one of
`{error, {jwt_validation_failed, [Key :: binary()]}}` or

```
  {error, {missing_keys, [Key :: binary()],
           bad_items, [{Key :: binary(), Val :: any()}]}}
```

if an error occurred.

<a name="verify-4"></a>

### verify/4 ###

<pre><code>
verify(JWT, KID, Iss, SigningKey) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>JWT = <a href="#type-jwt">jwt()</a></code></li><li><code>KID = <a href="#type-kid">kid()</a></code></li><li><code>Iss = <a href="#type-iss">iss()</a></code></li><li><code>SigningKey = <a href="#type-signing_key">signing_key()</a></code></li><li><code>Result = ok | {error, Reason}</code></li><li><code>Reason = term()</code></li></ul>

Verify a JWT using the kid, iss, and signing key.

__See also:__ [verify/2](#verify-2).

<a name="verify_jwt-2"></a>

### verify_jwt/2 ###

<pre><code>
verify_jwt(X1::{Hdr, Payload, Sig, SigInput}, Ctx) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Hdr = <a href="jsx.md#type-json_term">jsx:json_term()</a></code></li><li><code>Payload = <a href="jsx.md#type-json_term">jsx:json_term()</a></code></li><li><code>Sig = binary()</code></li><li><code>SigInput = binary()</code></li><li><code>Ctx = <a href="#type-input_context">input_context()</a></code></li><li><code>Result = ok | {error, {jwt_validation_failed, signature}} | {error, {missing_keys, Ks, bad_items, Bs}}</code></li><li><code>Ks = [KeyName::binary()]</code></li><li><code>Bs = [{KeyName::binary(), Val::any()}]</code></li></ul>

Verify a JWT as decoded by `decode_jwt/1`.


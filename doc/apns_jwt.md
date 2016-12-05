

# Module apns_jwt #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module creates a JWT suitable for use with APNS.

Copyright (c) 2016 Silent Circle

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-alg">alg()</a> ###


<pre><code>
alg() = <a href="#type-bstring">bstring()</a>
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




### <a name="type-iat">iat()</a> ###


<pre><code>
iat() = <a href="#type-posix_time">posix_time()</a>
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
json() = <a href="#type-bstring">bstring()</a>
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

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#iss-1">iss/1</a></td><td></td></tr><tr><td valign="top"><a href="#jwt-1">jwt/1</a></td><td>Equivalent to <tt>jwt</tt>.</td></tr><tr><td valign="top"><a href="#jwt-3">jwt/3</a></td><td>Create a JWT for APNS usage, using the current erlang system time.</td></tr><tr><td valign="top"><a href="#key-1">key/1</a></td><td></td></tr><tr><td valign="top"><a href="#kid-1">kid/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create a signing context from the parameters passed.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="iss-1"></a>

### iss/1 ###

`iss(Apns_jwt_ctx) -> any()`

<a name="jwt-1"></a>

### jwt/1 ###

<pre><code>
jwt(Context) -&gt; JWT
</code></pre>

<ul class="definitions"><li><code>Context = <a href="#type-context">context()</a></code></li><li><code>JWT = <a href="#type-jwt">jwt()</a></code></li></ul>

Equivalent to `jwt`.

<a name="jwt-3"></a>

### jwt/3 ###

<pre><code>
jwt(KID, Issuer, SigningKey) -&gt; JWT
</code></pre>

<ul class="definitions"><li><code>KID = <a href="#type-kid">kid()</a></code></li><li><code>Issuer = <a href="#type-iss">iss()</a></code></li><li><code>SigningKey = <a href="#type-pem_encoded_key">pem_encoded_key()</a></code></li><li><code>JWT = <a href="#type-jwt">jwt()</a></code></li></ul>

Create a JWT for APNS usage, using the current erlang system time.
This is signed with ECDSA using the P-256 curve and the ES256 algorithm.


#### <a name="Parameters">Parameters</a> ####



<dd><code>KID :: binary()</code></dd>




<dt>This is the key ID of the private APNS key downloaded from the Apple
developer portal.</dt>




<dd><code>Issuer :: binary()</code></dd>




<dt>This is the Apple Team ID from the Apple developer portal.</dt>




<dd><code>SigningKey :: binary()</code></dd>




<dt>This is the PEM-encoded private key downloaded from the Apple
developer portal.</dt>



<a name="key-1"></a>

### key/1 ###

`key(Apns_jwt_ctx) -> any()`

<a name="kid-1"></a>

### kid/1 ###

`kid(Apns_jwt_ctx) -> any()`

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(KID, Issuer, SigningKeyPem) -&gt; Context
</code></pre>

<ul class="definitions"><li><code>KID = <a href="#type-kid">kid()</a></code></li><li><code>Issuer = <a href="#type-iss">iss()</a></code></li><li><code>SigningKeyPem = <a href="#type-pem_encoded_key">pem_encoded_key()</a></code></li><li><code>Context = <a href="#type-context">context()</a></code></li></ul>

Create a signing context from the parameters passed. This can
be used later to create a JWT.


#### <a name="Parameters">Parameters</a> ####



<dd><code>KID :: binary()</code></dd>




<dt>This is the key ID of the private APNS key downloaded from the Apple
developer portal.</dt>




<dd><code>Issuer :: binary()</code></dd>




<dt>This is the Apple Team ID from the Apple developer portal.</dt>




<dd><code>SigningKey :: binary()</code></dd>




<dt>This is the PEM-encoded private key downloaded from the Apple
developer portal.</dt>




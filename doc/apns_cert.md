

# Module apns_cert #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

APNS certificate utilities.

Copyright (c) 2015-2016 Silent Circle

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##
This module provides functions to decode and
validate APNS PEM and DER format certificates, given a Team ID
and the AppID Suffix (e.g. com.example.FakeApp).
See [`https://developer.apple.com`](https://developer.apple.com) for more information.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode_cert-1">decode_cert/1</a></td><td>Decode binary certificate data into an <code>#'OTPCertificate'{}</code>
record.</td></tr><tr><td valign="top"><a href="#der_decode_cert-1">der_decode_cert/1</a></td><td>Decode DER binary into an #'OTPCertificate'{} record.</td></tr><tr><td valign="top"><a href="#get_cert_info_map-1">get_cert_info_map/1</a></td><td>Extract more interesting APNS-related info from cert and
return in a map.</td></tr><tr><td valign="top"><a href="#pem_decode_certs-1">pem_decode_certs/1</a></td><td>Decode PEM binary into a list of #'OTPCertificate'{} records.</td></tr><tr><td valign="top"><a href="#validate-3">validate/3</a></td><td>Validate that the <code>TeamId</code> and <code>AppIdSuffix</code> correspond to the
certificate data <code>CertData</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode_cert-1"></a>

### decode_cert/1 ###

<pre><code>
decode_cert(CertData) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>CertData = binary()</code></li><li><code>Result = #'OTPCertificate'{} | {error, Reason::term()}</code></li></ul>

Decode binary certificate data into an `#'OTPCertificate'{}`
record.

<a name="der_decode_cert-1"></a>

### der_decode_cert/1 ###

<pre><code>
der_decode_cert(DerData::binary()) -&gt; #'OTPCertificate'{} | {error, Reason::term()}
</code></pre>
<br />

Decode DER binary into an #'OTPCertificate'{} record.

<a name="get_cert_info_map-1"></a>

### get_cert_info_map/1 ###

<pre><code>
get_cert_info_map(OTPCert) -&gt; CertInfo
</code></pre>

<ul class="definitions"><li><code>OTPCert = #'OTPCertificate'{}</code></li><li><code>CertInfo = #{}</code></li></ul>

Extract more interesting APNS-related info from cert and
return in a map.

<a name="pem_decode_certs-1"></a>

### pem_decode_certs/1 ###

<pre><code>
pem_decode_certs(PemData::binary()) -&gt; [#'OTPCertificate'{}] | {error, Reason::term()}
</code></pre>
<br />

Decode PEM binary into a list of #'OTPCertificate'{} records.

<a name="validate-3"></a>

### validate/3 ###

<pre><code>
validate(CertData, AppIdSuffix, TeamID) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>CertData = binary()</code></li><li><code>AppIdSuffix = binary()</code></li><li><code>TeamID = binary()</code></li><li><code>Result = ok | {error, Reason}</code></li><li><code>Reason = term()</code></li></ul>

Validate that the `TeamId` and `AppIdSuffix` correspond to the
certificate data `CertData`. `CertData` may be either PEM-encoded or
DER-encoded. If PEM-encoded, only one certificate is permitted in the data.


#### <a name="Cert_Data">Cert Data</a> ####

Depending on whether or not the certificate is PEM or DER
encoded, you could load it as follows:

```
  {ok, PemData} = file:read_file("cert.pem").
  {ok, DerData} = file:read_file("aps_developer.cer").
```


#### <a name="Team_ID">Team ID</a> ####

The team ID will be a 10-character binary string, such as
`<<"ABCDE12345">>`. This is obtained from the certificate's Subject OU
field.


#### <a name="AppID_Suffix">AppID Suffix</a> ####

The AppID Suffix will be a binary string such as `<<"com.example.MyApp">>`.
This is obtained from the certificate's Subject CN field.
The caller is expected to supply the right AppID Suffix or the
validation will fail.


#### <a name="Issuer_CN">Issuer CN</a> ####

The Issuer CN is expected to be
`Apple Worldwide Developer Relations Certification Authority`
or the validation will fail.


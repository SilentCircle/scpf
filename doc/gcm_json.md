

# Module gcm_json #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module handles JSON conversion for Google Cloud Messaging (GCM).

Copyright (c) 2015 Silent Circle

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-collapse_key">collapse_key()</a> ###


<pre><code>
collapse_key() = binary()
</code></pre>




### <a name="type-content_available">content_available()</a> ###


<pre><code>
content_available() = boolean()
</code></pre>




### <a name="type-data">data()</a> ###


<pre><code>
data() = [{binary(), any()}]
</code></pre>




### <a name="type-delay_while_idle">delay_while_idle()</a> ###


<pre><code>
delay_while_idle() = boolean()
</code></pre>




### <a name="type-dry_run">dry_run()</a> ###


<pre><code>
dry_run() = boolean()
</code></pre>




### <a name="type-gcm_opt">gcm_opt()</a> ###


<pre><code>
gcm_opt() = <a href="#type-recipient_id">recipient_id()</a> | <a href="#type-registration_ids">registration_ids()</a> | <a href="#type-collapse_key">collapse_key()</a> | <a href="#type-priority">priority()</a> | <a href="#type-content_available">content_available()</a> | <a href="#type-delay_while_idle">delay_while_idle()</a> | <a href="#type-time_to_live">time_to_live()</a> | <a href="#type-restricted_package_name">restricted_package_name()</a> | <a href="#type-dry_run">dry_run()</a> | <a href="#type-data">data()</a>
</code></pre>




### <a name="type-json_term">json_term()</a> ###


<pre><code>
json_term() = [{binary() | atom(), <a href="#type-json_term">json_term()</a>}] | [{}] | [<a href="#type-json_term">json_term()</a>] | [] | true | false | null | integer() | float() | binary() | atom() | <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>




### <a name="type-notification">notification()</a> ###


<pre><code>
notification() = [{atom(), <a href="#type-gcm_opt">gcm_opt()</a>}]
</code></pre>




### <a name="type-priority">priority()</a> ###


<pre><code>
priority() = binary()
</code></pre>




### <a name="type-recipient_id">recipient_id()</a> ###


<pre><code>
recipient_id() = binary()
</code></pre>




### <a name="type-registration_ids">registration_ids()</a> ###


<pre><code>
registration_ids() = [binary()]
</code></pre>




### <a name="type-restricted_package_name">restricted_package_name()</a> ###


<pre><code>
restricted_package_name() = binary()
</code></pre>




### <a name="type-time_to_live">time_to_live()</a> ###


<pre><code>
time_to_live() = integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#make_notification-1">make_notification/1</a></td><td>Create a notification consisting of a JSON binary suitable for
transmitting to the Google Cloud Messaging Service.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="make_notification-1"></a>

### make_notification/1 ###

<pre><code>
make_notification(Notification::<a href="#type-notification">notification()</a>) -&gt; binary()
</code></pre>
<br />

Create a notification consisting of a JSON binary suitable for
transmitting to the Google Cloud Messaging Service.

To understand the various properties below, please see
[
GCM Architectural Overview](http://developer.android.com/guide/google/gcm/index.md).
The description given is basically to show how to format the
properties in Erlang.


### <a name="Notification_Properties">Notification Properties</a> ###



<dt><code>id::binary()</code></dt>




<dd>
Recipient ID (binary string). <strong>Required</strong> unless
<code>registration_ids</code> is provided.  This corresponds to the GCM <code>to</code>
parameter, which is one of a registration token, notification key, or
topic.</dd>




<dt><code>registration_ids::[binary()]</code></dt>




<dd>List of binary strings. Each binary is a registration id
for an Android device+application. <strong>Required</strong> unless
<code>id</code> is provided.
</dd>




<dt><code>data::[{binary(), any()}]</code></dt>




<dd><p><strong>Required</strong>.<br />
Message payload data, which must be an
object (Erlang proplist) as described in the table below.</p><p></p><table class="with-borders">
<tr>
<th><strong>json</strong></th><th><strong>erlang</strong></th>
</tr>
<tr>
<td> <code>number</code> </td>
<td> <code>integer()</code> and <code>float()</code></td>
</tr>
<tr>
<td> <code>string</code> </td>
<td> <code>binary()</code> </td>
</tr>
<tr>
<td> <code>true</code>, <code>false</code> and <code>null</code></td>
<td> <code>true</code>, <code>false</code> and <code>null</code></td>
</tr>
<tr>
<td> <code>array</code> </td>
<td> <code>[]</code> and <code>[JSON]</code></td>
</tr>
<tr>
<td> <code>object</code> </td>
<td> <code>[{}]</code> and <code>[{binary() OR atom(), JSON}]</code></td>
</tr>
</table>
</dd>




<dt><code>gcm::list()</code></dt>




<dd>An optional list of GCM-specific properties</dd>





<dt><code>collapse_key::binary()</code></dt>




<dd>Binary string. Optional.</dd>




<dt><code>priority::binary()</code></dt>




<dd>Binary string. Optional. Either <code><<"normal">></code> (default) or
<code><<"high">></code>.</dd>




<dt><code>content_available::boolean()</code></dt>




<dd>Binary string. Optional.</dd>




<dt><code>delay_while_idle::boolean()</code></dt>




<dd>See GCM reference. Optional.</dd>




<dt><code>time_to_live::integer()</code></dt>




<dd>Optional.</dd>




<dt><code>restricted_package_name::binary()</code></dt>




<dd>Binary string - overrides default on server. Optional.</dd>




<dt><code>dry_run::boolean()</code></dt>




<dd>Optional (defaults to false)</dd>






### <a name="Examples_of_Notification_proplist">Examples of Notification proplist</a> ###


#### <a name="Simplest_possible_single_registration_id_notification">Simplest possible single registration id notification</a> ####

```
  Notification = [
      {'id', <<"registration-id">>},
      {'data', [{msg, <<"Would you like to play a game?">>}]}
  ].
```


#### <a name="Simplest_possible_multiple_registration_id_notification">Simplest possible multiple registration id notification</a> ####


```
  Notification = [
      {'registration_ids', [<<"registration-id-1">>, <<"registration-id-2">>]},
      {'data', [{msg, <<"Would you like to play a game?">>}]}
  ].
```


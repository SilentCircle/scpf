

# Module apns_json #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

APNS JSON notification creation library.

Copyright (c) 2015 Silent Circle LLC

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-json_term">json_term()</a> ###


<pre><code>
json_term() = [<a href="#type-json_term">json_term()</a>] | [{binary() | atom() | integer(), <a href="#type-json_term">json_term()</a>}] | #{} | true | false | null | integer() | float() | binary() | atom() | <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#make_notification-1">make_notification/1</a></td><td>Create a notification consisting of a JSON binary suitable for
transmitting to the Apple Push Service.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="make_notification-1"></a>

### make_notification/1 ###

<pre><code>
make_notification(PL) -&gt; ApnsJsonNotification
</code></pre>

<ul class="definitions"><li><code>PL = [<a href="proplists.md#type-property">proplists:property()</a>]</code></li><li><code>ApnsJsonNotification = binary()</code></li></ul>

Create a notification consisting of a JSON binary suitable for
transmitting to the Apple Push Service.


### <a name="Notification_Properties">Notification Properties</a> ###




<dt><code>{'alert', binary() | proplist()}</code></dt>




<dd>If a <code>binary()</code>, it will be used as the notification text. If a
<code>proplist()</code>, see <a href="#Alert_Properties">Alert Properties</a> for the format of the
<code>proplist()</code>.</dd>




<dt><code>{'badge', integer()}</code></dt>




<dd>Badge count. Optional: 0 removes badge; absence leaves badge
unchanged.</dd>




<dt><code>{'sound', binary()}</code></dt>




<dd>The name of a sound file in the app bundle or in the
<code>Library/Sounds</code> folder of the app’s data container. The sound in this
file is played as an alert. If the sound file doesn’t exist or default
is specified as the value, the default alert sound is played. The audio
must be in one of the audio data formats that are compatible with
system sounds; see
<a href="https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/IPhoneOSClientImp.md#//apple_ref/doc/uid/TP40008194-CH103-SW6">Preparing Custom Alert Sounds</a>
for details.</dd>




<dt><code>{'content-available', integer()}</code></dt>




<dd>Provide this key with a value of 1 to indicate that new content is
available. Including this key and value means that when your app is
launched in the background or resumed,
<code>application:didReceiveRemoteNotification:fetchCompletionHandler:</code> is
called.</dd>




<dt><code>{'category', binary()}</code></dt>




<dd>Provide this key with a string value that represents the identifier
property of the <code>UIMutableUserNotificationCategory</code> object you created
to define custom actions. To learn more about using custom actions, see
<a href="https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/IPhoneOSClientImp.md#//apple_ref/doc/uid/TP40008194-CH103-SW26">Registering Your Actionable Notification Types</a>.</dd>




<dt><code>{'extra', proplist()}</code></dt>




<dd><p>Additional (optional) custom data, which must be an
object (Erlang proplist) as described in the table below.</p><p></p><table class="with-borders">
<tr>
<th><strong>json</strong></th><th><strong>erlang</strong></th>
</tr>
<tr>
<td> <code>number</code> </td>
<td> <code>integer() | float()</code></td>
</tr>
<tr>
<td> <code>string</code> </td>
<td> <code>binary()</code> </td>
</tr>
<tr>
<td> <code>true</code></td>
<td> <code>'true'</code></td>
</tr>
<tr>
<td> <code>false</code></td>
<td> <code>'false'</code></td>
</tr>
<tr>
<td> <code>null</code></td>
<td> <code>'null'</code></td>
</tr>
<tr>
<td> <code>array</code> </td>
<td> <code>[json()]</code></td>
</tr>
<tr>
<td> <code>empty object</code> </td>
<td> <code>[{}]</code></td>
</tr>
<tr>
<td> <code>non-empty object</code> </td>
<td> <code>[{binary() | atom(), json()}]</code></td>
</tr>
</table>
</dd>




#### <a name="Alert_Properties">Alert Properties</a> ####

This describes the proplist that is expected if `alert` is not a binary
string (a binary string such as `<<"This is a message.">>`). In the
following description, `binary()` is to be interpreted as a binary string.



<dt><code>{'title', binary()}</code></dt>




<dd>A short string describing the purpose of the notification. Apple
Watch displays this string as part of the notification interface. This
string is displayed only briefly and should be crafted so that it can
be understood quickly. This key was added in iOS 8.2.</dd>




<dt><code>{'body', binary()}</code></dt>




<dd>The text of the alert message.</dd>




<dt><code>{'title-loc-key', binary() | 'null'}</code></dt>




<dd>The key to a title string in the <code>Localizable.strings</code> file for the
current localization. The key string can be formatted with <code>%@</code> and
<code>%n$@</code> specifiers to take the variables specified in the <code>'title-loc-args'</code> array.
See <a href="https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/TheNotificationPayload.md#//apple_ref/doc/uid/TP40008194-CH107-SW7">Localized Formatted Strings</a>
for more information. This key was added in iOS 8.2.</dd>




<dt><code>{'title-loc-args', [binary()] | 'null'}</code></dt>




<dd>Variable string values to appear in place of the format specifiers
in <code>'title-loc-key'</code>.
See <a href="https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/TheNotificationPayload.md#//apple_ref/doc/uid/TP40008194-CH107-SW7">Localized Formatted Strings</a> for more information.
This key was added in iOS 8.2.</dd>




<dt><code>{'action-loc-key', binary() | 'null'}</code></dt>




<dd>If a string is specified, displays an alert with two buttons.
However, iOS uses the string as a key to get a localized string in the
current localization to use for the right button's title instead of
"View". If the value is <code>null</code>, the system displays an alert with a
single OK button that simply dismisses the alert when tapped.
See
<a href="https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/TheNotificationPayload.md#//apple_ref/doc/uid/TP40008194-CH107-SW7">Localized Formatted Strings</a>
for more information.</dd>




<dt><code>{'loc-key', binary()}</code></dt>




<dd>
A key to an alert-message string in a <code>Localizable.strings</code> file for
the current localization (which is set by the user's language
preference). The key string can be formatted with <code>%@</code> and <code>%n$@</code>
specifiers to take the variables specified in <code>'loc-args'</code>.
See
<a href="https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/TheNotificationPayload.md#//apple_ref/doc/uid/TP40008194-CH107-SW7">Localized Formatted Strings</a>
for more information.
</dd>




<dt><code>{'loc-args', [binary()]}</code></dt>




<dd>
This array of binary strings contains variable values to be substituted
into the format string defined in <code>'loc-key'</code>.
</dd>




<dt><code>{'launch-image', binary()}</code></dt>




<dd>
The filename of an image file in the application bundle; it may include
the extension or omit it. The image is used as the launch image when
users tap the action button or move the action slider. If this property
is not specified, the system either uses the previous snapshot, uses the
image identified by the <code>UILaunchImageFile</code> key in the application's
<code>Info.plist</code> file, or falls back to <code>Default.png</code>.
</dd>




### <a name="Examples_of_Notification_proplist">Examples of Notification proplist</a> ###


#### <a name="Simplest_Possible_Alert">Simplest Possible Alert</a> ####

```
  Notification = [
      {'alert', <<"Would you like to play a game?">>}
  ].
```


#### <a name="Alert_with_sound_and_badge">Alert with sound and badge</a> ####

```
  Notification = [
      {'alert', <<"Would you like to play a game?">>},
      {'badge', 1},
      {'sound', <<"wopr">>}
  ].
```


#### <a name="Alert_with_additional_custom_JSON_data">Alert with additional custom JSON data</a> ####

```
  Notification = [
      {'alert', <<"Would you like to play a game?">>},
      {'extra', [{<<"meta">>, [{<<"movie">>, <<"War Games">>}]]}
  ].
```


#### <a name="Localized_alert_using_'loc-key'_and_'loc-args'">Localized alert using 'loc-key' and 'loc-args'</a> ####

This assumes that the `Localizable.strings` file in the `.lproj` directory
contains

```
  "GAME_PLAY_REQUEST_FORMAT" = "%@ and %@ have invited you to play Monopoly";
```

```
  Notification = [
      {'alert', [{'title', <<"Game invite">>},
                 {'loc-key', <<"GAME_PLAY_REQUEST_FORMAT">>},
                 {'loc-args', [<<"Jenna">>, <<"Frank">>]}]},
      {'sound', <<"chime">>}
  ].
```


### <a name="More_Information">More Information</a> ###

For information on the keys in the proplist, see
[Local and Push Notification Programming Guide](https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/TheNotificationPayload.md#//apple_ref/doc/uid/TP40008194-CH107-SW1).


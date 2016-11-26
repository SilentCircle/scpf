

# Silent Circle Push Framework (SCPF) #

Copyright (c) 2016 Silent Circle

__Version:__ Dec 3 2016 13:19:58

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).


### <a name="Purpose">Purpose</a> ###

The Silent Circle Push Framework (SCPF) provides a private, general
interface to push notification services such as Apple Push Service
(APS, a.k.a. APNS), and Google Cloud Messaging (GCM).  The framework
consists of a general API layer, which ships more generalized
requests to and from service-specific API layers. The general API
layer is named `sc_push`, and it supports the operations described
in this document.

---


### <a name="Use_Cases">Use Cases</a> ###

The basic use cases for SCPF are straightforward.


### <a name="Definitions">Definitions</a> ###

_user_

- An installed application on a specific device
- Correctly set up to receive push notifications from its Push
  Service
- Has a unique identifier (`token`) provided by its Push Service.
  Also known as a "push token".
- Has a user id (`tag`) provided by the application.
- Has a universally unique application installation identity
  (`device_id`), by which the installed application instance, on
  that specific, physical device,can be identified. `device_id` is
the unfortunately misleading historical name for this.

---


### <a name="Use_Case_1:_Register_a_user">Use Case 1: Register a user</a> ###

- Application receives a push token from its Push Service.
- Application registers the "user" (application instance).

---


### <a name="Use_Case_2:_Push_a_notification_to_a_user">Use Case 2: Push a notification to a user</a> ###

- A service needs to send a short textual notification to
a "user".
- The service provides the notification and the identity of
the user in a call to the API.
- The API sends the notification to zero, one, or more
"users", depending on the API call used and its parameters.
See the API calls for details.

---


### <a name="Use_Case_3:_Deregister_a_user">Use Case 3: Deregister a user</a> ###

- The user discontinues the service.
- The service deletes the user's `device_id` using this API.
- All registrations matching that `device_id` are deleted.
- No further push notifications should go to that user.

---


### <a name="Use_Case_4:_Re-register_a_token">Use Case 4: Re-register a token</a> ###

- The push service provides the application with a new token, and
the currently registered token is, or will soon be, invalid. This
can happen for a number of reasons that are out of scope for this
API document.
- The application calls this API to re-register the token, or delete
the old one and register the new one. The reregister API requires
both the device id and tag for uniqueness.
- The registration record is updated with the new token (reregister
API) or deleted and recreated in separate operations.

---


### <a name="Use_Case_5:_Push_a_notification_to_an_unregistered_device">Use Case 5: Push a notification to an unregistered device</a> ###

- The caller provides the service name, app id, and token
(registration ID) to the API, along with notification parameters.
- The API sends the notification to the device.
- This use case is applicable when the registration data is kept
externally, and SCPF is used only to deliver the notifications.

---


### <a name="Supported_Push_Notification_Services">Supported Push Notification Services</a> ###

SCPF currently supports these services:

- Apple Push Notification Service (APS, or APNS)
- Google Cloud Messaging (GCM)

SCPF is designed in such a way that adding a new service should be
relatively straightforward. The basic `sc_push` API should remain
unchanged, so code that currently uses it will require little or no
modification.

This does not apply to code that uses advanced service-specific
features, such as the `collapse-key` GCM feature.

---


### <a name="Interface">Interface</a> ###

The API currently supports the following interfaces:

- Erlang native API
- REST API (default port 8765)

---


### <a name="Authentication">Authentication</a> ###

The Erlang native API is only accessible to Erlang nodes that have
both access to the host on which the API is running (port 4369), and
the Erlang cookie for that node. Since port 4369 will never be
public facing, and moreover will not even be generally accessible,
authentication is neither possible nor necessary for Erlang code.

A significant portion of the Erlang API is indirectly accessible via
the REST API.

The REST API is HTTP-only, without authentication, which means that
until authentication and HTTPS support are added, it should only be
used behind an authenticating reverse proxy server, or on localhost.

---


### <a name="Registration_API">Registration API</a> ###


### <a name="Purpose">Purpose</a> ###

To register a device's push identification with SCPF, and associate
it with a SCPF tag, so that the Push API may direct notifications to
all devices associated with the tag. If the device service and token
is known, a push notification may be sent to a specific device only.


#### <a name="Extended_API">Extended API</a> ####

There is an extended API, available in both Erlang and REST, that
supports sending a push notification to unregistered devices.  To do
this, the service, app ID, and token must be provided by the caller,
and the app ID must have been configured for the SCPF service.

---


### <a name="Summary">Summary</a> ###

- Register for push notifications
- Unregister for push notifications
- Reregister a token
- Get registration info by tag
- Get registration info by service+token
- Get registration info for a device id
- Get SCPF version


### <a name="Overview">Overview</a> ###

The basic purpose of the API is to register  which the
device-specific notification service provides, and associate it with
a tag. This identifier is referred to in this documentation as the
__token__, and corresponds to the _GCM registration ID_, or the
_APNS push token_.  The __tag__ is a string that SCPF uses as a
destination for push notifications. This may, for example, be a user
ID or SIP address.

There is no technical restriction on the format of the tags. They are binaries that are opaque to SCPF (and consequently case-sensitive, if binary strings).
If, for example, it would be useful for the tag to be a URN or URL, there is nothing to prevent that.

__WARNING__: Tags are used in URLs of certain endpoints, so it is advisable to avoid tags that would require urlencoding.

Multiple registations having the same tag will receive a copy of the
push notification that is sent to that tag.  For example, if an
Android device has registered a GCM ID under tag
`sip:12345678@sip.example.com`, and an iOS device has
registered an APNS push token under the same tag, then a
notification sent to the one will also be sent to the other at the
same time.

This API also supports unregistering by tag, token, or device id,
and re-registering a token that has become stale and needs to be
replaced. This corresponds to, for example, the GCM service issuing
a canonical registration ID to replace one that has become invalid
or stale.

Finally, the API is able to look up registration information by tag,
by device id, and by service/token combination.

__NOTE__

- The __token__ must be unique within a given service (e.g. APNS).
- The __device id__ must be unique across all services. A UUID is a
  good choice.
- The __tag__ is a user/application-level grouping identifier. The
device id/tag combination must be unique across all services. It is
strongly advisable to format the tag such that it does not require
url encoding, so it can be used easily in the REST API calls.

---


### <a name="Registration_REST_API">Registration REST API</a> ###


### <a name="Register_for_push_notifications">Register for push notifications</a> ###

__Endpoint__

`/registration/device/:device_id/tag/:tag`

__Method__

`PUT` - Adds or updates a registration by device ID/tag

__Supported Content-Types__

- `application/json`

__Request Parameters__

- `device_id`: A caller-specific resource ID.
- `tag`: The tag under which registrations were made (including`sip:` prefix).
- `app_id`: The client application ID, which is the APNS`appBundleId`/`apns_topic`, or the GCM package ID.
- `service`: The push notification service, currently either `apns`
  or `gcm`.
- `token`: The push service token/registration ID.
- `dist`: Distribution, either `prod` or `dev`. Only really applies
  to Apple's APNS, and should almost always be `prod`. `dev` only
applies to APNS ad-hoc builds that use Development server-side
push certificates, or universal push certificates that are used with
development tokens on the development push service.

__Sample JSON__

```
 {
     "app_id": "com.example.someapp",
     "service": "gcm",
     "tag": "sip:number@sip.example.net",
     "token": "some_very_long_token",
     "dist": "prod"
 }
```

__Response__

- 204 No Content

---


### <a name="Deregister_from_push_notifications_by_service/token">Deregister from push notifications by service/token</a> ###

__Endpoint__

`/registration/service/:service/token/:token`

__Method__

`DELETE` - Deletes a collection by service/token.

__Request Parameters__

- `service`: Currently `gcm` or `apns`, but may support other services
  in future
- `token`: The GCM registration ID or APNS token. Note that this may
delete multiple registrations.

__Responses__

- 204 No Content
- 404 Object Not Found

---


### <a name="Deregister_from_push_notifications_by_device_id">Deregister from push notifications by device id</a> ###

__Endpoint__

`/registration/device/:device_id`

__Method__

`DELETE` - Deletes a collection by device ID.

__Request Parameters__

- `device_id`: A caller-defined key value. Note that this endpoint
  is a collection, and __all__ registrations matching `device_id`
will be deleted.

__Responses__

- 204 No Content
- 404 Object Not Found

---


### <a name="Deregister_from_push_notifications_by_tag">Deregister from push notifications by tag</a> ###

__Endpoint__

`/registration/tag/:tag`

__Method__

`DELETE` - Deletes a collection by tag.

This will delete __all__ registration information for that tag,
which might be multiple tokens across multiple push services.

__Request Parameters__

- `tag`: The tag under which registrations were made (including`sip:` or other prefix).

__Response__

- 204 No Content
- 404 Object Not Found

---


### <a name="Get_registration_information_by_service/token">Get registration information by service/token</a> ###

__Endpoint__

`/registration/service/:service/token/:token`

__Method__

`GET` - Retrieves a collection.

__Request Parameters__

- `service`: Currently `gcm` or `apns`, but may support other
  services in future
- `token`: The GCM registration ID or APNS token

__Response__

The response will be (if a 200 status) a registration collection.

__Example__

```
 GET /registration/service/gcm/token/some_very_long_token
 HTTP/1.1 200 OK
 Content-Type: application/json
 [
     {
         "app_id": "com.example.someapp",
         "service": "gcm",
         "tag": "sip:user@example.com",
         "device_id": "",
         "token": "Some_very_long_token",
         "dist": "prod"
     }
 ]
```

---


### <a name="Get_registration_information_by_tag">Get registration information by tag</a> ###

__Endpoint__

`/registration/tag/:tag`

__Method__

`GET` - Retrieves a collection by tag.

__Request Parameters__

- `tag`: The tag under which registrations were made.

__Response__

The response will be (if a 200 status) a registration collection.

__Example__

```
 GET /registration/tag/sip:user@example.com
 HTTP/1.1 200 OK
 Content-Type: application/json
 [
     {
         "app_id": "com.example.someapp",
         "dist": "prod",
         "service": "apns",
         "tag": "sip:user@example.com",
         "device_id": "foo",
         "token": "apns_token"
     },
     {
         "app_id": "com.example.someapp",
         "dist": "prod",
         "service": "gcm",
         "tag": "sip:user@example.com",
         "device_id": "bar",
         "token": "Some_very_long_token"
     }
 ]
```

---


### <a name="Get_registration_information_by_device_ID">Get registration information by device ID</a> ###

__Endpoint__

`/registration/device/:device_id`

__Method__

`GET` - Retrieves a collection by device ID.

__Request Parameters__

- `device_id`: A caller-defined key value.

__Response__

The response will be (if a 200 status) a registration collection.

__Example__

```
 GET /registration/device_id/:device_id
 HTTP/1.1 200 OK
 Content-Type: application/json
 [
     {
         "app_id": "com.example.someapp",
         "service": "gcm",
         "tag": "sip:user@example.com",
         "device_id": ":device_id",
         "token": "Some_very_long_token",
         "dist": "prod"
     }
 ]
```

---


### <a name="Push_Notification_API">Push Notification API</a> ###


### <a name="Purpose">Purpose</a> ###

To send notifications to devices on a supported push network.

Devices may be registered under some user-specific tag that can be
used as an address for the notification, or, if the device token is
known, the notification may be sent directly using the extended API.
In the latter case, the device need not be registered with SCPF.


### <a name="Summary">Summary</a> ###

- Send a notification to a tag
- Send a notification to a token within a service
- Send a notification by service, app id and token


### <a name="Discussion">Discussion</a> ###

This API permits the caller to send notifications to devices that
may or may not be registered with SCPF.

A notification may be sent using the __tag__, in which case it will
be sent to all devices (__tokens__) associated with that tag; to
a specific service/token combination; or to a service/appid/token.

---


### <a name="Push_Notification_REST_API">Push Notification REST API</a> ###


### <a name="Push_request_format">Push request format</a> ###

A push request is supplied as a JSON dictionary. The minimum
information is the `alert` text. Additional information may be added
on a service-specific basis.

```
 {
     "alert": "Alert text goes here"
 }
```


#### <a name="APNS-specific_Request">APNS-specific Request</a> ####

The `alert` key is mandatory.

```
 {
     "alert": "Alert text goes here",
     "aps": {"badge": 3, "sound": "file.wav"}
 }
```

__Note__

- `aps` dictionary is optional
- `aps` dictionary items are optional


#### <a name="GCM-specific_Request">GCM-specific Request</a> ####

This bears some explanation. Whatever goes in the `data` dictionary
is what the client application receives. The `alert` key gets
special treatment in the code, but anything else is just passed on
to GCM. The `alert` key (and hence the `data` dictionary) is
mandatory.

The optional `gcm` dictionary is what accepts GCM-specific keywords, such as
`collapse_key` and `priority`. These must be one of the following:

- `collapse_key`
- `priority`; "normal" (default) or "high".
- `content_available`
- `delay_while_idle`
- `time_to_live`
- `restricted_package_name`
- `dry_run`

See the GCM documentation for more information on these keywords.

```
 {
     "data": {
         "alert": "Alert text goes here",
         "purpose": "reload_v1_me"
     },
     "gcm": {
         "collapse_key": "foobarbaz",
         "priority": "high"
     }
 }
```

---


### <a name="Response_Format">Response Format</a> ###

Every JSON response has a top-level `results` key. A successful response
contains an array of objects, each of which contain either an `id` key
or an `error` key. Each element of the array corresponds to the alert sent,
if multiple alerts were sent in a single request (__note__: future functionality).

__Success Response__

```
 HTTP/1.1 200 OK
 Content-Type: application/json
 {
     "results": [
         {
             "id": "opaque_identifier"
         }
     ]
 }
```

__Error Responses__

There may a number of different error responses. If the request was correctly
formatted, the response will be in JSON with an HTTP status code of 200. This is true
even if the response has errors, so the JSON must always be checked.

```
 HTTP/1.1 200 OK
 Content-Type: application/json
 {
     "results": [
         {
             "error": "reg_not_found_for_token: \"token\""
         }
     ]
 }
```

If the request was bad, though, the server will return a `400 Bad Request` status
and plain text content, for example, if the key `"alert"` was omitted or misspelled:

```
 HTTP/1.1 400 Bad Request
 Content-Type: text/plain; charset=UTF-8
 {missing_data_or_alert,[{token,<<"token">>},{alet,<<"Test">>},{tag,undefined}
```

---


### <a name="Send_notification_to_service/token">Send notification to service/token</a> ###

__Endpoint__

`/push/send/service/:service/token/:token`

__Method__

`POST` - Send a notification to a specific service and token.

Notifications are only accepted in JSON format.

__Request Parameters__

- `service`: Currently `gcm` or `apns`, but may support other
  services in future
- `token`: The GCM registration ID or APNS token
- `alert`: The notification message text

__Request__

```
 POST /push/send/service/:service/token/:token
 Content-Type: application/json
 {
     "alert": "Alert text goes here"
 }
```

__Response__

Responses are the same for all `/push/send` API calls. See
"Response Format"  section.

---


### <a name="Send_notification_to_tag">Send notification to tag</a> ###

__Endpoint__

`/push/send/tag/:tag`

__Method__

`POST` - Send a notification to a tag.

This will send a notification message to each token that was registered under that tag, so that
multiple devices, even using different services (APNS and GCM, say),
will receive the notification from a single call to this endpoint.

__Request Parameters__

- `tag`: The tag given to the registration API.
- `alert`: The notification message text

__Request__

```
 POST /push/send/tag/:tag
 Content-Type: application/json
 {
     "alert": "Alert text goes here"
 }
```

__Response__

Responses are the same for all `/push/send` API calls. See
"Response Format" section.

---


### <a name="Send_notification_to_service/appid/token">Send notification to service/appid/token</a> ###

__Endpoint__

`/push/send/service/:service/app_id/:app_id/token/:token`

__Method__

`POST` - Send a notification to an APNS token or (GCM registration ID) belonging to a specific app id.

This allows the caller to send a notification to an unregistered device if the device's token is known.

__Request Parameters__

- `service`: The service to use, currently `gcm` or `apns`.
- `app_id`: The client application ID, which is the APNS`appBundleId` or the Android's application ID.
- `token`: The APNS token or GCM registration ID.
- `alert`: The notification message text.

__Request__

```
 POST /push/send/service/:service/app_id/:app_id/token/:token
 Content-Type: application/json
 {
     "alert": "Alert text goes here"
 }
```

__Response__

Responses are the same for all `/push/send` API calls. See
"Response Format" section.

---


### <a name="Miscellaneous_REST_endpoints">Miscellaneous REST endpoints</a> ###


### <a name="Get_SCPF_Version">Get SCPF Version</a> ###

__Endpoint__

`/version`

__Method__

`GET` - Retrieve SCPF service version

__Request Parameters__

There are no request parameters.

__Response__

- 200 OK

__Example__

GET /version
HTTP/1.1 200 OK
Content-Type: application/json
{"version":"vX.Y.Z"}

The format of the version string is not guaranteed to conform to any standard.


### <a name="Building">Building</a> ###


### <a name="Build_Prerequisites">Build Prerequisites</a> ###

- Erlang 18.3 or later (64 bit)
- GNU Make 3.81 or later
- `rebar3`


### <a name="Building_the_application">Building the application</a> ###

```
 git clone git@github.com:example/scpf.git
 cd scpf
 make rel
```


### <a name="Building_documentation">Building documentation</a> ###

Note that Erlang and `pandoc` are required to build the documentation.

```
 make doc
```

The Erlang documentation is in Markdown format and will be in `README.md`, with dependent Markdown files in `doc/`. There is also a `man` page in `doc/man/scpf.1`.


### <a name="Creating_an_unsigned_Debian_package">Creating an unsigned Debian package</a> ###

```
 dpkg-buildpackage -us -uc
```

The output files (`.deb`, `.changes`, `.tar.gz` and `.dsc`) will be
in the parent directory.

For convenience, there is also a `dev_package` make target, which
will create Debian files (such as `.deb`, `.dsc`, etc) in the
`distdir` subdirectory.

---


### <a name="Configuration">Configuration</a> ###

`scpf` installation aims to comply with the [Filesystem Hierarchy
Standard (FHS)](http://refspecs.linuxfoundation.org/fhs.shtml) as
much as possible. When installed via a Debian package, a user,
`scpf`, is created, which is the owner of all its files.

- `/etc/scpf` - Contains configuration files for scpf:
    - vm.args`- edit the environment variables
  `WEBMACHINE_IP` and `WEBMACHINE_PORT`. This configuration will
  change more often than the other. In this file, the default IP is
  set to `&#183;`, and the default port is `8765`.
    - `sys.config` - this contains Erlang-specific
  configuration items.
- `/var/log/scpf` - Contains log files for this application:
    - `console.log` (regular console log messages)
    - `error.log` (error log messages), and
    - `crash.log` - details of crashes.

---


### <a name="Self-testing_the_application">Self-testing the application</a> ###

```
 make ct
```

To inspect test results, open a browser on `_build/test/lib/scpf/logs/index.html`. Note that to get the latest test results, `index.html` must be reloaded in the browser.

---


### <a name="Running">Running</a> ###

The application can be run from the development directory (for
testing), or started and stopped as a Debian service, if it has been
installed.


### <a name="scpf_Command-line_Utility">scpf Command-line Utility</a> ###

Assuming the package has been installed on Debian, `scpf` has a man
page.

```
 man scpf
```

To view it before installation,

```
 make doc
 man doc/man/scpf.1
```


### <a name="Service_Startup">Service Startup</a> ###

This assumes that `systemd` is being used.

```
 sudo systemctl start scpf
```


### <a name="Service_Shutdown">Service Shutdown</a> ###

```
 sudo systemctl stop scpf
```


### <a name="Service_Status">Service Status</a> ###

```
 sudo systemctl status scpf
```


### <a name="Startup_(Development)">Startup (Development)</a> ###

```
 make run
```

This starts an Erlang shell using `config/shell.config`. This configuration is minimal and has no sessions defined in it.


### <a name="Shutdown_(Development)">Shutdown (Development)</a> ###

Exit the shell.

---


### <a name="Internals">Internals</a> ###

SCPF is an Erlang application that runs in its own Erlang node,
named `scpf@host`. For example, if it were running on an AWS host
`push01-euc1`, it could be named something like
`scpf@push01-euc1.example.com`.  The Debian installation package
creates a user, not coincidentally named `scpf`. The `scpf` user
owns the files that are installed, and is the user under which the
Erlang node runs.

Actually, the 'scpf' node really is a collection of Erlang
applications. Some of them, such as `kernel`, `stdlib`, and
`mnesia`, are standard Erlang/OTP applications. Some of them, such
as `webmachine`, are third-party open source applications residing
on github. The rest of them are Silent Circle applications,
including:

- `sc_push` - The SCPF application. This includes an Erlang API for
  use by other applications in the same or other Erlang nodes, and a
  REST API for non-Erlang program access.
- `sc_push_lib` - The SCPF library, which includes `sc_config` and`sc_push_req_mgr`.  Anything in this library may be - and is -
  used by service-specific code in other applications.
- `apns_erlv3` - The Apple Push Notification Service (APNS) Erlang
  API for HTTP/2. This is a service-specific application and includes an API
  that conforms to a standard convention expected by `sc_push`.
- `gcm_erl` - The Google Cloud Messaging Erlang API. Another
service-specific API application.


### <a name="Deprecated_services">Deprecated services</a> ###

- `apns_erl` - This supports the old APNS binary service, and is
  replaced by `apns_erlv3`. It is deprecated and unsupported. It will
be removed at some point in the future.


### <a name="Push_Service_API_Naming_Convention">Push Service API Naming Convention</a> ###

This is a good place to discuss the naming convention used for push
service APIs. Note that it is a _mandatory_ naming convention. The
format of the name is `$SERVICE-$APP_ID`.`SERVICE` is `apnsv3` in
this case. `APP_ID` is the case-sensitive identifier of the
application and depends on the service; in APNS, this is the APNS
`app_bundle_id`, for example `com.example.someapp`.  This
is passed to the SCPF service using the variable `app_id`.

---


### <a name="Applications">Applications</a> ###


### <a name="sc\_push">sc\_push</a> ###

`sc_push` comprises the following components:

- `sc_push_sup` - the top-level supervisor. If any of its child
  processes fail, it will automatically restart it. If the process
  restarts too often within a set time period, the supervisor will
  shut down the `sc_push` application.
- `sc_push_wm_sup` - the webmachine supervisor. This controls the
  processes that make up the REST API, which are run by webmachine,
  a third party Erlang open source application.
- `sc_push_svc_null` - the `null` push service supervisor. Every
  push service has a top-level supervisor named`sc_push_svc_$SERVICE`. In this case, the `SERVICE` is `null`.
  This is the name that is used as the `mod` attribute in the SCPF
  configuration file (__TODO__: document this somewhere).


### <a name="sc\_push\_lib">sc\_push\_lib</a> ###

- `sc_push_lib_sup` - the top-level supervisor.
- `sc_push_reg_api` - the push registration API.
- `sc_push_reg_db` - the push registration database layer.
- `sc_push_req_mgr` - the push request manager.


### <a name="apns\_erlv3">apns\_erlv3</a> ###

- `sc_push_svc_apnsv3` - the `apnsv3` push service supervisor.
- `apns_erlv3_session_sup` - the `apnsv3` session supervisor. This
controls each push session. A push session is an instance of a
service configured for a specific client application.  This allows
per-application, per-service configuration.


### <a name="gcm\_erl">gcm\_erl</a> ###

- `sc_push_svc_gcm` - the `gcm` push service supervisor.
- `gcm_erl_session_sup` - the `gcm` session supervisor.
- `gcm_req_sched` - The GCM request scheduler. GCM requires
exponential backoff and other scheduling rules, controlled by this
process.


#### <a name="Service_configuration">Service configuration</a> ####

Let's say we have an Android app to which we need to push GCM
notifications, unimaginatively named `someapp`, whose Android app ID is `com.example.someapp`.
A service configured to push to this app would be named `gcm-com.example.someapp`.

The basic `gcm` `sys.config` service configuration, without any configured
services, would look like this:

```
 [
  %% Other configurations like sasl and lager
  %% ...
  {gcm_erl, [
             {service, [
                        {name, gcm},
                        {mod, sc_push_svc_gcm},
                        {description, "Google Cloud Messaging Service"}
                       ]},
             {sessions, [
                         %% Sessions go here
                        ]}
            ]}
 ].
```

To add a GCM session for `com.example.someapp`, you would add its
configuration to `sys.config` like this:

```
 [
  %% Other configurations like sasl and lager
  %% ...
  {gcm_erl, [
             {service, [
                        {name, gcm},
                        {mod, sc_push_svc_gcm},
                        {description, "Google Cloud Messaging Service"}
                       ]},
             {sessions, [
                         [ % session 1
                          {name, 'gcm-com.example.someapp'},
                          {config,
                           [
                            {api_key, <<"*** put your Google API key here ***">>},
                            {ssl_opts, [{verify, verify_none}]},
                            {uri, "https://gcm-http.googleapis.com/gcm/send"},
                            {max_attempts, 5},
                            {retry_interval, 1},
                            {max_req_ttl, 600}
                           ]}
                         ] % session 1
                        ]} % sessions
            ]}, % gcm_erl
   %% ...
 ].
```
---

<script>
// Jump directly to a referenced url given in trailing '[]:...'-notation
function goto(tag) { parent.document.location.href = url(tag); }
function url(tag) { var o=document.getElementById(tag); return o ? o.href : '#'+tag; }
</script>



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/apns_cert.md" class="module">apns_cert</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/apns_erlv3_app.md" class="module">apns_erlv3_app</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/apns_erlv3_session.md" class="module">apns_erlv3_session</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/apns_erlv3_session_sup.md" class="module">apns_erlv3_session_sup</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/apns_json.md" class="module">apns_json</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/apns_jwt.md" class="module">apns_jwt</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/apns_lib.md" class="module">apns_lib</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/apns_lib_http2.md" class="module">apns_lib_http2</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/apns_recs.md" class="module">apns_recs</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/apns_types.md" class="module">apns_types</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/gcm_erl.md" class="module">gcm_erl</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/gcm_erl_app.md" class="module">gcm_erl_app</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/gcm_erl_session.md" class="module">gcm_erl_session</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/gcm_erl_session_sup.md" class="module">gcm_erl_session_sup</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/gcm_erl_util.md" class="module">gcm_erl_util</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/gcm_json.md" class="module">gcm_json</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/gcm_req_sched.md" class="module">gcm_req_sched</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_config.md" class="module">sc_config</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_priority_queue.md" class="module">sc_priority_queue</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push.md" class="module">sc_push</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_app.md" class="module">sc_push_app</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_lib.md" class="module">sc_push_lib</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_lib_app.md" class="module">sc_push_lib_app</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_lib_sup.md" class="module">sc_push_lib_sup</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_reg_api.md" class="module">sc_push_reg_api</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_reg_db.md" class="module">sc_push_reg_db</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_reg_resource.md" class="module">sc_push_reg_resource</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_reg_wm_device.md" class="module">sc_push_reg_wm_device</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_reg_wm_service.md" class="module">sc_push_reg_wm_service</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_reg_wm_tag.md" class="module">sc_push_reg_wm_tag</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_req_mgr.md" class="module">sc_push_req_mgr</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_sup.md" class="module">sc_push_sup</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_svc_apnsv3.md" class="module">sc_push_svc_apnsv3</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_svc_gcm.md" class="module">sc_push_svc_gcm</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_svc_null.md" class="module">sc_push_svc_null</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_svc_null_srv.md" class="module">sc_push_svc_null_srv</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_top.md" class="module">sc_push_top</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_wm_common.md" class="module">sc_push_wm_common</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_wm_helper.md" class="module">sc_push_wm_helper</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_wm_send_device.md" class="module">sc_push_wm_send_device</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_wm_send_svc_appid_tok.md" class="module">sc_push_wm_send_svc_appid_tok</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_wm_send_svc_tok.md" class="module">sc_push_wm_send_svc_tok</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_wm_send_tag.md" class="module">sc_push_wm_send_tag</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_wm_sup.md" class="module">sc_push_wm_sup</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_push_wm_version.md" class="module">sc_push_wm_version</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_types.md" class="module">sc_types</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_util.md" class="module">sc_util</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_util_app.md" class="module">sc_util_app</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/scpf/blob/feature/add-sentry/doc/sc_util_srv.md" class="module">sc_util_srv</a></td></tr></table>


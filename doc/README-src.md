# Purpose

The Silent Circle Push Framework (SCPF) provides a private, general
interface to push notification services such as Apple Push Service
(APS, a.k.a. APNS), and Google Cloud Messaging (GCM).  The framework
consists of a general API layer, which ships more generalized
requests to and from service-specific API layers. The general API
layer is named `sc_push`, and it supports the operations described
in this document.

---

# Use Cases

The basic use cases for SCPF are straightforward.

## Definitions

*user*

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

## Use Case 1: Register a user

- Application receives a push token from its Push Service.
- Application registers the "user" (application instance).

---

## Use Case 2: Push a notification to a user

- A service needs to send a short textual notification to
  a "user".
- The service provides the notification and the identity of
  the user in a call to the API.
- The API sends the notification to zero, one, or more
  "users", depending on the API call used and its parameters.
  See the API calls for details.

---

## Use Case 3: Deregister a user

- The user discontinues the service.
- The service deletes the user's `device_id` using this API.
- All registrations matching that `device_id` are deleted.
- No further push notifications should go to that user.

---

## Use Case 4: Re-register a token

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

## Use Case 5: Push a notification to an unregistered device

- The caller provides the service name, app id, and token
  (registration ID) to the API, along with notification parameters.
- The API sends the notification to the device.
- This use case is applicable when the registration data is kept
  externally, and SCPF is used only to deliver the notifications.

---

# Supported Push Notification Services

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

# Interface

The API currently supports the following interfaces:

- Erlang native API
- REST API (default port 8765)

---

# Authentication

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

# Registration API

## Purpose

To register a device's push identification with SCPF, and associate
it with a SCPF tag, so that the Push API may direct notifications to
all devices associated with the tag. If the device service and token
is known, a push notification may be sent to a specific device only.

### Extended API

There is an extended API, available in both Erlang and REST, that
supports sending a push notification to unregistered devices.  To do
this, the service, app ID, and token must be provided by the caller,
and the app ID must have been configured for the SCPF service.

---

## Summary

- Register for push notifications
- Unregister for push notifications
- Reregister a token
- Get registration info by tag
- Get registration info by service+token
- Get registration info for a device id
- Get SCPF version

## Overview

The basic purpose of the API is to register  which the
device-specific notification service provides, and associate it with
a tag. This identifier is referred to in this documentation as the
**token**, and corresponds to the _GCM registration ID_, or the
_APNS push token_.  The **tag** is a string that SCPF uses as a
destination for push notifications. This may, for example, be a user
ID or SIP address.

There is no technical restriction on the format of the tags. They are binaries that are opaque to SCPF (and consequently case-sensitive, if binary strings).
If, for example, it would be useful for the tag to be a URN or URL, there is nothing to prevent that.

**WARNING**: Tags are used in URLs of certain endpoints, so it is advisable to avoid tags that would require urlencoding.


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

**NOTE**

- The **token** must be unique within a given service (e.g. APNS).
- The **device id** must be unique across all services. A UUID is a
  good choice.
- The **tag** is a user/application-level grouping identifier. The
  device id/tag combination must be unique across all services. It is
  strongly advisable to format the tag such that it does not require
  url encoding, so it can be used easily in the REST API calls.

---

# Registration REST API


## Register for push notifications

**Endpoint**

`/registration/device/:device_id/tag/:tag`

**Method**

`PUT` - Adds or updates a registration by device ID/tag

**Supported Content-Types**

- `application/json`

**Request Parameters**

- `device_id`: A caller-specific resource ID.
- `tag`: The tag under which registrations were made (including
  `sip:` prefix).
- `app_id`: The client application ID, which is the APNS
  `appBundleId`/`apns_topic`, or the GCM package ID.
- `service`: The push notification service, currently either `apns`
  or `gcm`.
- `token`: The push service token/registration ID.
- `dist`: Distribution, either `prod` or `dev`. Only really applies
  to Apple's APNS, and should almost always be `prod`. `dev` only
  applies to APNS ad-hoc builds that use Development server-side
  push certificates, or universal push certificates that are used with
  development tokens on the development push service.

**Sample JSON**

	{
	    "app_id": "com.example.someapp",
	    "service": "gcm",
	    "tag": "sip:number@sip.example.net",
	    "token": "some_very_long_token",
	    "dist": "prod"
	}

**Response**

- 204 No Content

---

## Deregister from push notifications by service/token

**Endpoint**

`/registration/service/:service/token/:token`

**Method**

`DELETE` - Deletes a collection by service/token.

**Request Parameters**

- `service`: Currently `gcm` or `apns`, but may support other services
  in future
- `token`: The GCM registration ID or APNS token. Note that this may
  delete multiple registrations.

**Responses**

- 204 No Content
- 404 Object Not Found

---

## Deregister from push notifications by device id

**Endpoint**

`/registration/device/:device_id`

**Method**

`DELETE` - Deletes a collection by device ID.

**Request Parameters**

- `device_id`: A caller-defined key value. Note that this endpoint
  is a collection, and **all** registrations matching `device_id`
  will be deleted.

**Responses**

- 204 No Content
- 404 Object Not Found

---

## Deregister from push notifications by tag

**Endpoint**

`/registration/tag/:tag`

**Method**

`DELETE` - Deletes a collection by tag.

This will delete **all** registration information for that tag,
which might be multiple tokens across multiple push services.

**Request Parameters**

- `tag`: The tag under which registrations were made (including
  `sip:` or other prefix).

**Response**

- 204 No Content
- 404 Object Not Found

---

## Get registration information by service/token

**Endpoint**

`/registration/service/:service/token/:token`

**Method**

`GET` - Retrieves a collection.

**Request Parameters**

- `service`: Currently `gcm` or `apns`, but may support other
  services in future
- `token`: The GCM registration ID or APNS token

**Response**

The response will be (if a 200 status) a registration collection.

**Example**

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

---

## Get registration information by tag

**Endpoint**

`/registration/tag/:tag`

**Method**

`GET` - Retrieves a collection by tag.

**Request Parameters**

- `tag`: The tag under which registrations were made.

**Response**

The response will be (if a 200 status) a registration collection.

**Example**

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

---

## Get registration information by device ID

**Endpoint**

`/registration/device/:device_id`

**Method**

`GET` - Retrieves a collection by device ID.

**Request Parameters**

- `device_id`: A caller-defined key value.

**Response**

The response will be (if a 200 status) a registration collection.

**Example**

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

---

# Push Notification API

## Purpose

To send notifications to devices on a supported push network.

Devices may be registered under some user-specific tag that can be
used as an address for the notification, or, if the device token is
known, the notification may be sent directly using the extended API.
In the latter case, the device need not be registered with SCPF.

## Summary

- Send a notification to a tag
- Send a notification to a token within a service
- Send a notification by service, app id and token

## Discussion

This API permits the caller to send notifications to devices that
may or may not be registered with SCPF.

A notification may be sent using the **tag**, in which case it will
be sent to all devices (**tokens**) associated with that tag; to
a specific service/token combination; or to a service/appid/token.

---

# Push Notification REST API

## Push request format

A push request is supplied as a JSON dictionary. The minimum
information is the `alert` text. Additional information may be added
on a service-specific basis.

	{
	    "alert": "Alert text goes here"
	}

### APNS-specific Request

The `alert` key is mandatory.

	{
	    "alert": "Alert text goes here",
	    "aps": {"badge": 3, "sound": "file.wav"}
	}

**Note**

- `aps` dictionary is optional
- `aps` dictionary items are optional

### GCM-specific Request

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


---

## Response Format

Every JSON response has a top-level `results` key. A successful response
contains an array of objects, each of which contain either an `id` key
or an `error` key. Each element of the array corresponds to the alert sent,
if multiple alerts were sent in a single request (**note**: future functionality).

**Success Response**

	HTTP/1.1 200 OK
	Content-Type: application/json
	{
	    "results": [
	        {
	            "id": "opaque_identifier"
	        }
	    ]
	}

**Error Responses**

There may a number of different error responses. If the request was correctly
formatted, the response will be in JSON with an HTTP status code of 200. This is true
even if the response has errors, so the JSON must always be checked.

	HTTP/1.1 200 OK
	Content-Type: application/json
	{
	    "results": [
	        {
	            "error": "reg_not_found_for_token: \"token\""
	        }
	    ]
	}

If the request was bad, though, the server will return a `400 Bad Request` status
and plain text content, for example, if the key `"alert"` was omitted or misspelled:

	HTTP/1.1 400 Bad Request
	Content-Type: text/plain; charset=UTF-8
	{missing_data_or_alert,[{token,<<"token">>},{alet,<<"Test">>},{tag,undefined}

---

## Send notification to service/token

**Endpoint**

`/push/send/service/:service/token/:token`

**Method**

`POST` - Send a notification to a specific service and token.

Notifications are only accepted in JSON format.

**Request Parameters**

- `service`: Currently `gcm` or `apns`, but may support other
  services in future
- `token`: The GCM registration ID or APNS token
- `alert`: The notification message text

**Request**

	POST /push/send/service/:service/token/:token
	Content-Type: application/json
	{
	    "alert": "Alert text goes here"
	}

**Response**

Responses are the same for all `/push/send` API calls. See
"Response Format"  section.

---

## Send notification to tag

**Endpoint**

`/push/send/tag/:tag`

**Method**

`POST` - Send a notification to a tag.

This will send a notification message to each token that was registered under that tag, so that
multiple devices, even using different services (APNS and GCM, say),
will receive the notification from a single call to this endpoint.

**Request Parameters**

- `tag`: The tag given to the registration API.
- `alert`: The notification message text

**Request**

	POST /push/send/tag/:tag
	Content-Type: application/json
	{
	    "alert": "Alert text goes here"
	}

**Response**

Responses are the same for all `/push/send` API calls. See
"Response Format" section.

---

## Send notification to service/appid/token

**Endpoint**

`/push/send/service/:service/app_id/:app_id/token/:token`

**Method**

`POST` - Send a notification to an APNS token or (GCM registration ID) belonging to a specific app id.

This allows the caller to send a notification to an unregistered device if the device's token is known.

**Request Parameters**

- `service`: The service to use, currently `gcm` or `apns`.
- `app_id`: The client application ID, which is the APNS
  `appBundleId` or the Android's application ID.
- `token`: The APNS token or GCM registration ID.
- `alert`: The notification message text.

**Request**

	POST /push/send/service/:service/app_id/:app_id/token/:token
	Content-Type: application/json
	{
	    "alert": "Alert text goes here"
	}

**Response**

Responses are the same for all `/push/send` API calls. See
"Response Format" section.

---

# Miscellaneous REST endpoints


## Get SCPF Version

**Endpoint**

`/version`

**Method**

`GET` - Retrieve SCPF service version

**Request Parameters**

There are no request parameters.

**Response**

- 200 OK

**Example**


    GET /version
    HTTP/1.1 200 OK
    Content-Type: application/json
    {"version":"vX.Y.Z"}


The format of the version string is not guaranteed to conform to any standard.


# Building

## Build Prerequisites

- Erlang 18.3 or later (64 bit)
- GNU Make 3.81 or later
- `rebar3`

## Building the application

	git clone git@github.com:example/scpf.git
	cd scpf
	make rel

## Building documentation

Note that Erlang and `pandoc` are required to build the documentation.

	make doc

The Erlang documentation is in Markdown format and will be in `README.md`, with dependent Markdown files in `doc/`. There is also a `man` page in `doc/man/scpf.1`.

## Creating an unsigned Debian package

	dpkg-buildpackage -us -uc

The output files (`.deb`, `.changes`, `.tar.gz` and `.dsc`) will be
in the parent directory.

For convenience, there is also a `dev_package` make target, which
will create Debian files (such as `.deb`, `.dsc`, etc) in the
`distdir` subdirectory.

---


# Configuration

`scpf` installation aims to comply with the [Filesystem Hierarchy
Standard (FHS)](http://refspecs.linuxfoundation.org/fhs.shtml) as
much as possible. When installed via a Debian package, a user,
`scpf`, is created, which is the owner of all its files.

- `/etc/scpf` - Contains configuration files for scpf:
    - vm.args` - edit the environment variables
  `WEBMACHINE_IP` and `WEBMACHINE_PORT`. This configuration will
  change more often than the other. In this file, the default IP is
  set to `::`, and the default port is `8765`.
    - `sys.config` - this contains Erlang-specific
  configuration items.
- `/var/log/scpf` - Contains log files for this application:
    - `console.log` (regular console log messages)
    - `error.log` (error log messages), and
    - `crash.log` - details of crashes.

---

# Self-testing the application

	make ct

To inspect test results, open a browser on `_build/test/lib/scpf/logs/index.html`. Note that to get the latest test results, `index.html` must be reloaded in the browser.


---

# Running

The application can be run from the development directory (for
testing), or started and stopped as a Debian service, if it has been
installed.

## scpf Command-line Utility

Assuming the package has been installed on Debian, `scpf` has a man
page.

	man scpf

To view it before installation,

	make doc
	man doc/man/scpf.1

## Service Startup

This assumes that `systemd` is being used.

	sudo systemctl start scpf

## Service Shutdown

	sudo systemctl stop scpf

## Service Status

	sudo systemctl status scpf

## Startup (Development)

	make run

This starts an Erlang shell using `config/shell.config`. This configuration is minimal and has no sessions defined in it.

## Shutdown (Development)

Exit the shell.

---

# Internals

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
- `sc_push_lib` - The SCPF library, which includes `sc_config` and
  `sc_push_req_mgr`.  Anything in this library may be - and is -
  used by service-specific code in other applications.
- `apns_erlv3` - The Apple Push Notification Service (APNS) Erlang
  API for HTTP/2. This is a service-specific application and includes an API
  that conforms to a standard convention expected by `sc_push`.
- `gcm_erl` - The Google Cloud Messaging Erlang API. Another
  service-specific API application.

# Deprecated services

- `apns_erl` - This supports the old APNS binary service, and is
  replaced by `apns_erlv3`. It is deprecated and unsupported. It will
  be removed at some point in the future.

# Push Service API Naming Convention

This is a good place to discuss the naming convention used for push
service APIs. Note that it is a *mandatory* naming convention. The
format of the name is `$SERVICE-$APP_ID`.  `SERVICE` is `apnsv3` in
this case. `APP_ID` is the case-sensitive identifier of the
application and depends on the service; in APNS, this is the APNS
`app_bundle_id`, for example `com.example.someapp`.  This
is passed to the SCPF service using the variable `app_id`.

---

# Applications

## sc\_push

`sc_push` comprises the following components:

- `sc_push_sup` - the top-level supervisor. If any of its child
  processes fail, it will automatically restart it. If the process
  restarts too often within a set time period, the supervisor will
  shut down the `sc_push` application.
- `sc_push_wm_sup` - the webmachine supervisor. This controls the
  processes that make up the REST API, which are run by webmachine,
  a third party Erlang open source application.
- `sc_push_svc_null` - the `null` push service supervisor. Every
  push service has a top-level supervisor named
  `sc_push_svc_$SERVICE`. In this case, the `SERVICE` is `null`.
  This is the name that is used as the `mod` attribute in the SCPF
  configuration file (**TODO**: document this somewhere).

## sc\_push\_lib

- `sc_push_lib_sup` - the top-level supervisor.
- `sc_push_reg_api` - the push registration API.
- `sc_push_reg_db` - the push registration database layer.
- `sc_push_req_mgr` - the push request manager.

## apns\_erlv3

- `sc_push_svc_apnsv3` - the `apnsv3` push service supervisor.
- `apns_erlv3_session_sup` - the `apnsv3` session supervisor. This
  controls each push session. A push session is an instance of a
  service configured for a specific client application.  This allows
  per-application, per-service configuration.

## gcm\_erl

- `sc_push_svc_gcm` - the `gcm` push service supervisor.
- `gcm_erl_session_sup` - the `gcm` session supervisor.
- `gcm_req_sched` - The GCM request scheduler. GCM requires
  exponential backoff and other scheduling rules, controlled by this
  process.

### Service configuration

Let's say we have an Android app to which we need to push GCM
notifications, unimaginatively named `someapp`, whose Android app ID is `com.example.someapp`.
A service configured to push to this app would be named `gcm-com.example.someapp`.

The basic `gcm` `sys.config` service configuration, without any configured
services, would look like this:

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

To add a GCM session for `com.example.someapp`, you would add its
configuration to `sys.config` like this:

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

---

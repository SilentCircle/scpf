

# Module gcm_erl_util #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#sanitize_opts-1">sanitize_opts/1</a></td><td>Search <code>Opts`, which may be either a proplist
`[{service, Service::proplist()}, {sessions, Sessions::proplist()}]</code>,
or a list of sessions, <code>[[{name, string()}, {config, proplist()}]]</code>,
and redact sensitive data, returning the redacted argument.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="sanitize_opts-1"></a>

### sanitize_opts/1 ###

<pre><code>
sanitize_opts(Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Opts = list()</code></li><li><code>Result = list()</code></li></ul>

Search `Opts`, which may be either a proplist
`[{service, Service::proplist()}, {sessions, Sessions::proplist()}]`,
or a list of sessions, `[[{name, string()}, {config, proplist()}]]`,
and redact sensitive data, returning the redacted argument.
Needless to say, the redacted argument is pretty useless for
anything other than logging.

Currently this only redacts `api_key`.




# Module sc_util #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

General utility functions.

Copyright (c) 2012-2016 Silent Circle

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-beam_spec">beam_spec()</a> ###


<pre><code>
beam_spec() = module() | <a href="file.md#type-filename">file:filename()</a> | binary()
</code></pre>




### <a name="type-hex_string">hex_string()</a> ###


<pre><code>
hex_string() = [<a href="#type-xdigit">xdigit()</a>]
</code></pre>




### <a name="type-nybble">nybble()</a> ###


<pre><code>
nybble() = 0..15
</code></pre>




### <a name="type-type_spec">type_spec()</a> ###


<pre><code>
type_spec() = {Type::atom(), Arity::non_neg_integer()}
</code></pre>




### <a name="type-xdigit">xdigit()</a> ###


<pre><code>
xdigit() = 48..57 | 65..70 | 97..102
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bitstring_to_hex-1">bitstring_to_hex/1</a></td><td>Convert bitstring into a list of hex digits.</td></tr><tr><td valign="top"><a href="#ensure_module_loaded-1">ensure_module_loaded/1</a></td><td>Ensure that module <code>Mod</code> is loaded.</td></tr><tr><td valign="top"><a href="#exported_types-1">exported_types/1</a></td><td>
Return the module name and a list of types that were exported by a
BEAM file, presumably using <code>-export_type</code>.</td></tr><tr><td valign="top"><a href="#find_first_error-1">find_first_error/1</a></td><td>
Return the first tail of list <code>L</code> that begins with <code>{error, _}</code>, or
the empty list if no such tail exists.</td></tr><tr><td valign="top"><a href="#get_req_props-2">get_req_props/2</a></td><td>
Given a list of keys, look up each key in a proplist.</td></tr><tr><td valign="top"><a href="#hex_to_bitstring-1">hex_to_bitstring/1</a></td><td>Convert list of hex digits to a bitstring.</td></tr><tr><td valign="top"><a href="#make_child_spec-2">make_child_spec/2</a></td><td>Return a worker child specification based on <code>Opts</code> and <code>Timeout</code>.</td></tr><tr><td valign="top"><a href="#nybble-1">nybble/1</a></td><td>Return an integer value in the range 0..15, corresponding to
<code>Hexdigit</code>.</td></tr><tr><td valign="top"><a href="#opt_val-3">opt_val/3</a></td><td>Get optional value from proplist <code>PL</code> corresponding to key <code>K</code>.</td></tr><tr><td valign="top"><a href="#posix_time-0">posix_time/0</a></td><td>Return current POSIX time in seconds since the epoch.</td></tr><tr><td valign="top"><a href="#posix_time-1">posix_time/1</a></td><td>Return POSIX time in seconds corresponding to <code>{M, S, U}</code> tuple.</td></tr><tr><td valign="top"><a href="#req_binary_or_s-1">req_binary_or_s/1</a></td><td>Check that <code>S</code> is a non-empty string or binary string.</td></tr><tr><td valign="top"><a href="#req_s-1">req_s/1</a></td><td>Check that <code>S</code> is a non-empty string.</td></tr><tr><td valign="top"><a href="#req_val-2">req_val/2</a></td><td>Get required value from proplist <code>PL</code> corresponding to key <code>K</code>.</td></tr><tr><td valign="top"><a href="#req_val_binary_or_s-2">req_val_binary_or_s/2</a></td><td>Get required non-empty string or binary string value from proplist
<code>PL</code>,  corresponding to key <code>K</code>.</td></tr><tr><td valign="top"><a href="#req_val_s-2">req_val_s/2</a></td><td>Get required non-empty string value from proplist <code>PL</code>,
corresponding to key <code>K</code>.</td></tr><tr><td valign="top"><a href="#strclean-2">strclean/2</a></td><td>Validate that <code>S</code> is a valid string, using the supplied function,
<code>Validate/1</code>.</td></tr><tr><td valign="top"><a href="#strtrim-1">strtrim/1</a></td><td>
Remove all leading and trailing whitespace characters.</td></tr><tr><td valign="top"><a href="#to_atom-1">to_atom/1</a></td><td>
Return <code>X</code> as an atom.</td></tr><tr><td valign="top"><a href="#to_bin-1">to_bin/1</a></td><td>
Return the binary conversion of <code>X</code>.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>
Return <code>X</code> as a list.</td></tr><tr><td valign="top"><a href="#val-2">val/2</a></td><td>Get value from proplist <code>PL</code> corresponding to key <code>K</code>.</td></tr><tr><td valign="top"><a href="#val-3">val/3</a></td><td>Get value from proplist <code>PL</code> corresponding to key <code>K</code>.</td></tr><tr><td valign="top"><a href="#xdigit-1">xdigit/1</a></td><td>Return a lowercase hexadecimal digit corresponding to <code>Value</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bitstring_to_hex-1"></a>

### bitstring_to_hex/1 ###

<pre><code>
bitstring_to_hex(X1::bitstring()) -&gt; <a href="#type-hex_string">hex_string()</a>
</code></pre>
<br />

Convert bitstring into a list of hex digits. The bit length
must be an integral multiple of 4.

In other words, `<<10:4>>` will be converted to `"a"`, `<<10>>` will be
converted to `"0a"`, and `<<255,15:4>>` to `"fff"`.

<a name="ensure_module_loaded-1"></a>

### ensure_module_loaded/1 ###

<pre><code>
ensure_module_loaded(Mod) -&gt; true
</code></pre>

<ul class="definitions"><li><code>Mod = atom()</code></li></ul>

throws `{cannot_load_module, {Mod::atom(), Reason::any()}}`

Ensure that module `Mod` is loaded. Throw an exception if it
cannot be loaded.

<a name="exported_types-1"></a>

### exported_types/1 ###

<pre><code>
exported_types(Beam) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Beam = <a href="#type-beam_spec">beam_spec()</a></code></li><li><code>Result = {ok, {Mod::module(), Types::[<a href="#type-type_spec">type_spec()</a>]}} | {error, beam_lib, Reason::term()}</code></li></ul>

Return the module name and a list of types that were exported by a
BEAM file, presumably using `-export_type`.

If there is no abstract code in the BEAM file, return the module
name and an empty list.

<a name="find_first_error-1"></a>

### find_first_error/1 ###

<pre><code>
find_first_error(L) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>L = [{error, any()} | any()]</code></li><li><code>Result = list()</code></li></ul>

Return the first tail of list `L` that begins with `{error, _}`, or
the empty list if no such tail exists.

<a name="get_req_props-2"></a>

### get_req_props/2 ###

<pre><code>
get_req_props(ReqKeys, Props) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>ReqKeys = [Key::any()]</code></li><li><code>Props = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Result = {Found::[{Key::any(), Value::any()}], NotFound::[Key::any()]}</code></li></ul>

Given a list of keys, look up each key in a proplist. Return a tuple
consisting of two lists: a list of matching properties, and a list
of missing keys.

<a name="hex_to_bitstring-1"></a>

### hex_to_bitstring/1 ###

<pre><code>
hex_to_bitstring(L::<a href="#type-hex_string">hex_string()</a>) -&gt; bitstring()
</code></pre>
<br />

Convert list of hex digits to a bitstring.

In other words, `"a"` will be converted to `<<10:4>>`, not to `<<10>>`.
This is so that values like `"fff"` are unambiguously converted to
the 12-bit value `<<255, 15:4>>`. If you want a binary that is a
multiple of 8 bits, make sure the input hex string length is a
multiple of 2.

<a name="make_child_spec-2"></a>

### make_child_spec/2 ###

<pre><code>
make_child_spec(Opts, Timeout) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Opts = [{mod, atom()} | {name, atom()} | {config, <a href="proplists.md#type-proplist">proplists:proplist()</a>}]</code></li><li><code>Timeout = non_neg_integer()</code></li><li><code>Result = {Name::atom(), {Mod::atom(), start_link, Args::[any()]}, permanent, Timeout::non_neg_integer(), worker, [Mod::atom()]}</code></li></ul>

Return a worker child specification based on `Opts` and `Timeout`.
Microseconds are rounded to the nearest second.

`Opts` is a proplist with 3 required values:

* `{mod, atom()}`: Module name of worker.

* `{name, atom()}`: Module ID.

* `{config, proplist()}`: start_link args.


`Timeout` is the child worker start timeout in milliseconds.

<a name="nybble-1"></a>

### nybble/1 ###

<pre><code>
nybble(Hexdigit) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Hexdigit = <a href="#type-xdigit">xdigit()</a></code></li><li><code>Result = <a href="#type-nybble">nybble()</a></code></li></ul>

throws `{invalid_hex_char, C}`

Return an integer value in the range 0..15, corresponding to
`Hexdigit`. `Hexdigit` is a case-insensitive hexadecimal digit.
Throws exception when `C` is not a hexadecimal character.

<a name="opt_val-3"></a>

### opt_val/3 ###

`opt_val(K, PL, Def) -> any()`

Get optional value from proplist `PL` corresponding to key `K`.
Return `Def` if key not found, else return the value.

<a name="posix_time-0"></a>

### posix_time/0 ###

<pre><code>
posix_time() -&gt; <a href="sc_types.md#type-posix_time">sc_types:posix_time()</a>
</code></pre>
<br />

Return current POSIX time in seconds since the epoch.
This is a synonym for `erlang:system_time(seconds)`.

<a name="posix_time-1"></a>

### posix_time/1 ###

<pre><code>
posix_time(X1::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>) -&gt; <a href="sc_types.md#type-posix_time">sc_types:posix_time()</a>
</code></pre>
<br />

Return POSIX time in seconds corresponding to `{M, S, U}` tuple.
Microseconds are rounded to the nearest second.

<a name="req_binary_or_s-1"></a>

### req_binary_or_s/1 ###

`req_binary_or_s(S) -> any()`

throws `{not_a_string, S::term()}`

Check that `S` is a non-empty string or binary string.
The string is stripped of leading and trailing whitespace and checked
for zero length. If the string is empty, an exception results.

<a name="req_s-1"></a>

### req_s/1 ###

`req_s(S) -> any()`

throws `{not_a_string, S::term()}`

Check that `S` is a non-empty string.
The string is stripped of leading and trailing whitespace and checked
for zero length. If the string is empty, an exception results.

<a name="req_val-2"></a>

### req_val/2 ###

`req_val(K, PL) -> any()`

throws `{missing_required_key, K::term()}`

Get required value from proplist `PL` corresponding to key `K`.

<a name="req_val_binary_or_s-2"></a>

### req_val_binary_or_s/2 ###

`req_val_binary_or_s(K, PL) -> any()`

throws `{missing_required_key, K::term()} | {validation_failed, empty_string_not_allowed} | {not_a_string, S::term()}`

Get required non-empty string or binary string value from proplist
`PL`,  corresponding to key `K`. The string is stripped of leading and
trailing whitespace and checked for zero length. If the string is
empty, an exception results.

<a name="req_val_s-2"></a>

### req_val_s/2 ###

`req_val_s(K, PL) -> any()`

throws `{missing_required_key, K::term()} | {validation_failed, empty_string_not_allowed} | {not_a_string, S::term()}`

Get required non-empty string value from proplist `PL`,
corresponding to key `K`. The string is stripped of leading and
trailing whitespace and checked for zero length. If the string is
empty, an exception results.

<a name="strclean-2"></a>

### strclean/2 ###

`strclean(S, Validate) -> any()`

throws `{not_a_string, S::term()}`

Validate that `S` is a valid string, using the supplied function,
`Validate/1`.
The string is stripped of leading and trailing whitespace and `Validate/1`
is run on the result. Throw exception if `S` is not a list, otherwise
do what `Validate/1` does (return `S` or throw exception).

<a name="strtrim-1"></a>

### strtrim/1 ###

<pre><code>
strtrim(Subject::string()) -&gt; string()
</code></pre>
<br />

Remove all leading and trailing whitespace characters.

<a name="to_atom-1"></a>

### to_atom/1 ###

<pre><code>
to_atom(X) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>X = atom() | binary() | list()</code></li><li><code>Result = atom()</code></li></ul>

Return `X` as an atom.

<a name="to_bin-1"></a>

### to_bin/1 ###

<pre><code>
to_bin(X) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>X = binary() | list() | atom() | integer()</code></li><li><code>Result = binary()</code></li></ul>

Return the binary conversion of `X`. If `X` is an atom, convert using
the `latin1` character set.

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(X) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>X = binary() | atom() | integer()</code></li><li><code>Result = list()</code></li></ul>

Return `X` as a list.

<a name="val-2"></a>

### val/2 ###

<pre><code>
val(K, PL) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>K = term()</code></li><li><code>PL = <a href="sc_types.md#type-proplist">sc_types:proplist</a>(any(), any())</code></li><li><code>Result = term() | undefined</code></li></ul>

Get value from proplist `PL` corresponding to key `K`.
Return `undefined` if key not found, else return the value.

<a name="val-3"></a>

### val/3 ###

<pre><code>
val(K, PL, Def) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>K = term()</code></li><li><code>PL = <a href="sc_types.md#type-proplist">sc_types:proplist</a>(any(), any())</code></li><li><code>Def = term()</code></li><li><code>Result = term() | undefined</code></li></ul>

Get value from proplist `PL` corresponding to key `K`.
Return `Def` if key not found, else return the value.

<a name="xdigit-1"></a>

### xdigit/1 ###

<pre><code>
xdigit(Value) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Value = <a href="#type-nybble">nybble()</a></code></li><li><code>Result = <a href="#type-xdigit">xdigit()</a></code></li></ul>

throws `{invalid_nybble, N}`

Return a lowercase hexadecimal digit corresponding to `Value`.
Throws exception when `Value` is not an integer or falls outside the range
0..15.


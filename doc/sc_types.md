

# Module sc_types #
* [Description](#description)
* [Data Types](#types)

Common type specifications.

Copyright (c) 2015 Silent Circle

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-atom_or_binary">atom_or_binary()</a> ###


<pre><code>
atom_or_binary() = atom() | binary()
</code></pre>




### <a name="type-atom_or_string">atom_or_string()</a> ###


<pre><code>
atom_or_string() = atom() | string()
</code></pre>




### <a name="type-binary_or_string">binary_or_string()</a> ###


<pre><code>
binary_or_string() = binary() | string()
</code></pre>




### <a name="type-exml">exml()</a> ###


<pre><code>
exml() = #xmlelement{}
</code></pre>




### <a name="type-gen_result">gen_result()</a> ###


<pre><code>
gen_result() = {result, <a href="#type-exml">exml()</a>} | {error, <a href="#type-exml">exml()</a>}
</code></pre>




### <a name="type-posix_time">posix_time()</a> ###


<pre><code>
posix_time() = integer()
</code></pre>




### <a name="type-prop">prop()</a> ###


<pre><code>
prop(KT, VT) = {KT, VT}
</code></pre>




### <a name="type-proplist">proplist()</a> ###


<pre><code>
proplist(KT, VT) = [<a href="#type-prop">prop</a>(KT, VT)]
</code></pre>




### <a name="type-reg_prop">reg_prop()</a> ###


<pre><code>
reg_prop() = <a href="#type-prop">prop</a>(atom(), <a href="#type-atom_or_binary">atom_or_binary()</a>)
</code></pre>




### <a name="type-reg_proplist">reg_proplist()</a> ###


<pre><code>
reg_proplist() = [<a href="#type-reg_prop">reg_prop()</a>, ...]
</code></pre>




### <a name="type-reg_result">reg_result()</a> ###


<pre><code>
reg_result() = ok | {error, term()}
</code></pre>


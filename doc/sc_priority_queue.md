

# Module sc_priority_queue #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Priority queues have essentially the same interface as ordinary
queues, except that a) there is an `in/3` that takes a priority, and
b) we have only implemented the core API we need.

<a name="description"></a>

## Description ##

Priorities should be integers - the higher the value the higher the
priority - but we don't actually check that.

`in/2` inserts items with priority 0.

We optimise the case where a priority queue is being used just like
an ordinary queue. When that is the case we represent the priority
queue as an ordinary queue. We could just call into the `queue`
module for that, but for efficiency we implement the relevant
functions directly in here, thus saving on inter-module calls and
eliminating a level of boxing.

When the queue contains items with non-zero priorities, it is
represented as a sorted kv list with the inverted priority as the
key and an ordinary queue as the value. Here again we use our own
ordinary queue implemention for efficiency, often making recursive
calls into the same function knowing that ordinary queues represent
a base case.
<a name="types"></a>

## Data Types ##




### <a name="type-pqueue">pqueue()</a> ###


<pre><code>
pqueue() = <a href="#type-squeue">squeue()</a> | {pqueue, [{<a href="#type-priority">priority()</a>, <a href="#type-squeue">squeue()</a>}]}
</code></pre>




### <a name="type-priority">priority()</a> ###


<pre><code>
priority() = integer() | infinity
</code></pre>




### <a name="type-q">q()</a> ###


<pre><code>
q() = <a href="#type-pqueue">pqueue()</a>
</code></pre>




### <a name="type-squeue">squeue()</a> ###


<pre><code>
squeue() = {queue, [any()], [any()], non_neg_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#filter-2">filter/2</a></td><td></td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td></td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#highest-1">highest/1</a></td><td></td></tr><tr><td valign="top"><a href="#in-2">in/2</a></td><td></td></tr><tr><td valign="top"><a href="#in-3">in/3</a></td><td></td></tr><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_queue-1">is_queue/1</a></td><td></td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td></td></tr><tr><td valign="top"><a href="#len-1">len/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#out-1">out/1</a></td><td></td></tr><tr><td valign="top"><a href="#out_p-1">out_p/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="filter-2"></a>

### filter/2 ###

<pre><code>
filter(Pred::fun((any()) -&gt; boolean()), Q::<a href="#type-pqueue">pqueue()</a>) -&gt; <a href="#type-pqueue">pqueue()</a>
</code></pre>
<br />

<a name="fold-3"></a>

### fold/3 ###

<pre><code>
fold(Fun::fun((any(), <a href="#type-priority">priority()</a>, A) -&gt; A), A, Q::<a href="#type-pqueue">pqueue()</a>) -&gt; A
</code></pre>
<br />

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(L::[{<a href="#type-priority">priority()</a>, any()}]) -&gt; <a href="#type-pqueue">pqueue()</a>
</code></pre>
<br />

<a name="highest-1"></a>

### highest/1 ###

<pre><code>
highest(X1::<a href="#type-pqueue">pqueue()</a>) -&gt; <a href="#type-priority">priority()</a> | empty
</code></pre>
<br />

<a name="in-2"></a>

### in/2 ###

<pre><code>
in(Item::any(), Q::<a href="#type-pqueue">pqueue()</a>) -&gt; <a href="#type-pqueue">pqueue()</a>
</code></pre>
<br />

<a name="in-3"></a>

### in/3 ###

<pre><code>
in(X::any(), Priority::<a href="#type-priority">priority()</a>, Q::<a href="#type-pqueue">pqueue()</a>) -&gt; <a href="#type-pqueue">pqueue()</a>
</code></pre>
<br />

<a name="is_empty-1"></a>

### is_empty/1 ###

<pre><code>
is_empty(X1::<a href="#type-pqueue">pqueue()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="is_queue-1"></a>

### is_queue/1 ###

<pre><code>
is_queue(X1::any()) -&gt; boolean()
</code></pre>
<br />

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(A::<a href="#type-pqueue">pqueue()</a>, B::<a href="#type-pqueue">pqueue()</a>) -&gt; <a href="#type-pqueue">pqueue()</a>
</code></pre>
<br />

<a name="len-1"></a>

### len/1 ###

<pre><code>
len(X1::<a href="#type-pqueue">pqueue()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-pqueue">pqueue()</a>
</code></pre>
<br />

<a name="out-1"></a>

### out/1 ###

<pre><code>
out(Q::<a href="#type-pqueue">pqueue()</a>) -&gt; {empty | {value, any()}, <a href="#type-pqueue">pqueue()</a>}
</code></pre>
<br />

<a name="out_p-1"></a>

### out_p/1 ###

<pre><code>
out_p(Q::<a href="#type-pqueue">pqueue()</a>) -&gt; {empty | {value, any(), <a href="#type-priority">priority()</a>}, <a href="#type-pqueue">pqueue()</a>}
</code></pre>
<br />

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(X1::<a href="#type-pqueue">pqueue()</a>) -&gt; [{<a href="#type-priority">priority()</a>, any()}]
</code></pre>
<br />


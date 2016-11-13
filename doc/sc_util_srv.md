

# Module sc_util_srv #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


### <a name="SRV_records">SRV records</a> ###
.

Copyright (c) 2015 Silent Circle

__Authors:__ Ed Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="description"></a>

## Description ##

SRV records essentially allow transparent DNS-level redirects of services to another domain or port. A simple example is when you have an XMPP server and you want users to have addresses like username@example.com, but your XMPP server is really installed on xmpp.example.com. In principle they work the same way as MX records do for email.

For a server `example.com` wanting to delegate its XMPP services to the server at 'xmpp.example.com', here are some example records:

```
  _xmpp-client._tcp.example.com. 18000 IN SRV 0 5 5222 xmpp.example.com.
  _xmpp-server._tcp.example.com. 18000 IN SRV 0 5 5269 xmpp.example.com.
```

The target domain **MUST** be an existing A record of the target server; it cannot be an IP address, and cannot be a CNAME record.

The 18000 in this example is the TTL (time-to-live), it tells other servers how long they should cache your record for - a higher number will reduce DNS traffic, and result in slightly faster connections (since DNS info will be more likely to be cached, and won't need to be re-fetched). A lower TTL is more useful if you are going to be changing your record, since you have to wait for the TTL until all caches have expired your old record.

The 0 and 5 are the record's priority and weight. These values are specific to SRV records, and allow you to have multiple targets with different priorities (e.g. for load balancing or fallback in case of a down server) - lower priority targets are tried first. The weight is used to bias resolvers towards certain targets in case of a priority tie. Most services will not need to change these values, and 0 and 5 are sensible defaults.

Next is the port the service is running on. Clients will typically connect to 5222.


### <a name="How_to_use_SRV_records">How to use SRV records</a> ###

Clients resolve the SRV records for _xmpp-client._tcp.example.com. One or more SRV records will be returned. Clients then select a record based on priority and weight as described in the table below.

```
  Priority     The priority of the server. Clients attempt to contact the server with the lowest priority.
  Weight       A load-balancing mechanism that is used when selecting a target host from those that have the same priority. Clients randomly choose SRV records that specify target hosts to be contacted, with probability proportional to the weight
  Port Number  The port where the server is listening for this service.
  Target       The fully qualified domain name of the host computer.
```


#### <a name="Example_1:_Equal_Priorities">Example 1: Equal Priorities</a> ####

Let's say an SRV resolution request returns the following:

```
  Priority   Weight    Port   Target
     0         30      5222   JHGAJSGHD.example.net
     0         40      5222   KJGOIUTRG.example.net
     0         15      5222   NBGDPRLGH.example.net
     0         15      5222   WMFPSNMGJ.example.net
```


* The lowest priority records are chosen. In this case, all of the records are at priority 0.

* A record is randomly selected from the group such that its probability of selection is proportional to its relative weight. In this example, the weights add up nicely to 100, so they could be thought of as percentages. In this arrangement, KJGOIUTRG.example.net would be chosen 40% of the time, and NBGDPRLGH.example.net 15% of the time.



#### <a name="Example_2:_Different_Priorities">Example 2: Different Priorities</a> ####

```
  Priority   Weight  Port    Target
      0        30    5222    JHGAJSGHD.example.net
      1        40    5222    KJGOIUTRG.example.net
      2        15    5222    NBGDPRLGH.example.net
      3        15    5222    WMFPSNMGJ.example.net
```

Here, the weights are irrelevant. JHGAJSGHD.example.net will be chosen every time. If connection attempts to it fail, then the next highest priority record, KJGOIUTRG.example.net, is chosen, and so on.


### <a name="What_this_module_provides">What this module provides</a> ###

The idea is to call fetch_srv_rrs/1, then pick_server/1 using the `Rest` list, until a host is reachable.


#### <a name="Example">Example</a> ####


```
  RRs = sc_util_srv:fetch_srv_rrs("_some-client._tcp.some.domain"),
  case try_connect(RRs) of
      {ok, Conn, SrvInfo} ->
          use_connection(Conn, SrvInfo);
      {error, Reason} ->
          handle_this(Reason)
  end.
  try_connect([]) ->
      {error, no_more_hosts_to_try};
  try_connect(RRs) ->
     case sc_util_srv:pick_server(RRs) of
         {Host, Port, _TTL, Rest} = SrvInfo ->
             case my_connect(Host, Port) of
                 {ok, Connection} ->
                     {ok, Connection, SrvInfo};
                 {error, connection_failure} ->
                     try_connect(Rest);
                 {error, Reason} ->
                     handle_error(Reason)
             end;
         undefined ->
             {error, no_hosts_available}
     end.
```

<a name="types"></a>

## Data Types ##




### <a name="type-dns_rr_prop">dns_rr_prop()</a> ###


<pre><code>
dns_rr_prop() = {domain, string()} | {type, atom()} | {class, atom()} | {ttl, <a href="#type-dns_ttl">dns_ttl()</a>} | {data, <a href="#type-dns_srv_tuple">dns_srv_tuple()</a>}
</code></pre>




### <a name="type-dns_rr_props">dns_rr_props()</a> ###


<pre><code>
dns_rr_props() = [<a href="#type-dns_rr_prop">dns_rr_prop()</a>]
</code></pre>




### <a name="type-dns_srv_tuple">dns_srv_tuple()</a> ###


<pre><code>
dns_srv_tuple() = {<a href="#type-srv_prio">srv_prio()</a>, <a href="#type-srv_weight">srv_weight()</a>, <a href="#type-srv_port">srv_port()</a>, <a href="#type-srv_host">srv_host()</a>}
</code></pre>




### <a name="type-rr">rr()</a> ###


<pre><code>
rr() = #rr{}
</code></pre>




### <a name="type-rrs">rrs()</a> ###


<pre><code>
rrs() = [<a href="#type-rr">rr()</a>]
</code></pre>




### <a name="type-service_name">service_name()</a> ###


<pre><code>
service_name() = binary() | string()
</code></pre>




### <a name="type-srv_info">srv_info()</a> ###


<pre><code>
srv_info() = {<a href="#type-srv_host">srv_host()</a>, <a href="#type-srv_port">srv_port()</a>, <a href="#type-dns_ttl">dns_ttl()</a>, <a href="#type-rrs">rrs()</a>}
</code></pre>




### <a name="type-weighted_rr">weighted_rr()</a> ###


<pre><code>
weighted_rr() = {<a href="#type-rr">rr()</a>, RunningTotal::non_neg_integer()}
</code></pre>




### <a name="type-weighted_rrs">weighted_rrs()</a> ###


<pre><code>
weighted_rrs() = [<a href="#type-weighted_rr">weighted_rr()</a>]
</code></pre>




### <a name="type-weighted_sums">weighted_sums()</a> ###


<pre><code>
weighted_sums() = {<a href="#type-weighted_rrs">weighted_rrs()</a>, Sum::non_neg_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#fetch_srv_rrs-1">fetch_srv_rrs/1</a></td><td></td></tr><tr><td valign="top"><a href="#pick_server-1">pick_server/1</a></td><td>Pick the server with the highest weighting and priority from
the list of RRs.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="fetch_srv_rrs-1"></a>

### fetch_srv_rrs/1 ###

<pre><code>
fetch_srv_rrs(ServiceName) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>ServiceName = <a href="#type-service_name">service_name()</a></code></li><li><code>Result = {ok, <a href="#type-rrs">rrs()</a>} | {error, any()}</code></li></ul>

<a name="pick_server-1"></a>

### pick_server/1 ###

<pre><code>
pick_server(RRs) -&gt; SrvInfo
</code></pre>

<ul class="definitions"><li><code>RRs = <a href="#type-rrs">rrs()</a></code></li><li><code>SrvInfo = <a href="#type-srv_info">srv_info()</a></code></li></ul>

Pick the server with the highest weighting and priority from
the list of RRs. Return `undefined` if RRs is an empty list.


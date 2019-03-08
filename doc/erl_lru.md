

# Module erl_lru #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = any()
</code></pre>




### <a name="type-lru">lru()</a> ###


<pre><code>
lru() = #lru{max_size = non_neg_integer(), current_size = non_neg_integer(), lookup_cache = #{<a href="#type-key">key()</a> =&gt; {non_neg_integer(), <a href="#type-value">value()</a>}}, usage = <a href="gb_trees.md#type-tree">gb_trees:tree</a>(non_neg_integer(), <a href="#type-key">key()</a>), last = any()}
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = any()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-3">add/3</a></td><td>Add new element to cache with defined key.</td></tr><tr><td valign="top"><a href="#empty-1">empty/1</a></td><td>Check if queue is empty.</td></tr><tr><td valign="top"><a href="#has_key-2">has_key/2</a></td><td>Check if cache has element with key.</td></tr><tr><td valign="top"><a href="#lookup_and_update-2">lookup_and_update/2</a></td><td>Lookup and update cache (LRU function).</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Create new unlimited cache.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Create new limited cache with defined max size.</td></tr><tr><td valign="top"><a href="#pop-1">pop/1</a></td><td>Pop element from head.</td></tr><tr><td valign="top"><a href="#push-3">push/3</a></td><td>Push element to queue tail.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>Get number of elements in the LRU cache.</td></tr><tr><td valign="top"><a href="#take-2">take/2</a></td><td>Take element from queue by key.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-3"></a>

### add/3 ###

<pre><code>
add(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, LRU::<a href="#type-lru">lru()</a>) -&gt; <a href="#type-lru">lru()</a>
</code></pre>
<br />

Add new element to cache with defined key.

Complexity: O(log(N))

<a name="empty-1"></a>

### empty/1 ###

<pre><code>
empty(Lru::<a href="#type-lru">lru()</a>) -&gt; boolean()
</code></pre>
<br />

Check if queue is empty.

Complexity: O(1)

<a name="has_key-2"></a>

### has_key/2 ###

<pre><code>
has_key(Key::<a href="#type-key">key()</a>, Lru::<a href="#type-lru">lru()</a>) -&gt; boolean()
</code></pre>
<br />

Check if cache has element with key.

Complexity: O(1)

<a name="lookup_and_update-2"></a>

### lookup_and_update/2 ###

<pre><code>
lookup_and_update(Key::<a href="#type-key">key()</a>, Lru::<a href="#type-lru">lru()</a>) -&gt; {ok, <a href="#type-value">value()</a>, <a href="#type-lru">lru()</a>} | error
</code></pre>
<br />

Lookup and update cache (LRU function).

Complexity: O(log(N))

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-lru">lru()</a>
</code></pre>
<br />

Create new unlimited cache.

Complexity: O(1)

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(MaxSize::pos_integer()) -&gt; <a href="#type-lru">lru()</a>
</code></pre>
<br />

Create new limited cache with defined max size

Complexity: O(1)

<a name="pop-1"></a>

### pop/1 ###

<pre><code>
pop(LRU::<a href="#type-lru">lru()</a>) -&gt; {<a href="#type-key">key()</a>, <a href="#type-value">value()</a>, <a href="#type-lru">lru()</a>}
</code></pre>
<br />

Pop element from head.

Complexity: O(log(N))

<a name="push-3"></a>

### push/3 ###

<pre><code>
push(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, LRU::<a href="#type-lru">lru()</a>) -&gt; <a href="#type-lru">lru()</a>
</code></pre>
<br />

Push element to queue tail.

Complexity: O(log(N))

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(Lru::<a href="#type-lru">lru()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Get number of elements in the LRU cache.
Complexity: O(1)

<a name="take-2"></a>

### take/2 ###

<pre><code>
take(Key::<a href="#type-key">key()</a>, LRU::<a href="#type-lru">lru()</a>) -&gt; {<a href="#type-value">value()</a>, <a href="#type-lru">lru()</a>} | error
</code></pre>
<br />

Take element from queue by key

Complexity: O(log(N))


[![Build Status](https://travis-ci.org/poroh/erl_lru.svg?branch=master)](https://travis-ci.org/poroh/erl_lru) [![Coverage Status](https://coveralls.io/repos/github/poroh/erl_lru/badge.svg?branch=master)](https://coveralls.io/github/poroh/erl_lru?branch=master)

# Erlang Cache of Least Recently Used items

Idea is to implement functional data structure that implements LRU.

## Usage

```erlang

Size = 3,
LRU0 = erl_lru:new(Size),

%% Add new elements:
LRU1 = erl_lru:add(key1, value1, LRU0),
LRU2 = erl_lru:add(key2, value2, LRU1),
LRU3 = erl_lru:add(key3, value3, LRU2),

%% Updating exist elements (element become reacently updated)
{ok, value2, LRU4} = erl_lru:lookup_and_update(key2, LRU3),

%% Add new element that automatically removes least recently used (key1)
LRU5  = erl_lru:add(key4, value4, LRU4),
false = erl_lru:has_key(key1, LRU5),

...
```

Full module documentation is availble [here](http://github.com/poroh/erl_lru/blob/master/doc/lru.md).

## Performance

All read-only operations has O(1) complexity

All modification operations has O(log(N)) complexity.

## License

MIT (http://github.com/poroh/erl_lru/blob/master/LICENSE)

## Build

```
$ rebar3 compile

```

## Under the hood

Under the hood two data structures:

1. Unordered map: #{key() => {order(), value()}} for quick lookup
2. Binary search tree: gb_tree(order(), key()) for odering element in cache

order() is almost always incrementing integer which represent pseudo-time in the cache:
* When new element is added to cache it has highest order() value
* When exist element is updated its order value is changest to highest
* When cache is overflowed element with lowest order is removed
* When cache become empty highest order value is set to 0



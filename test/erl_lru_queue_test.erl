-module(erl_lru_queue_test).

-include_lib("eunit/include/eunit.hrl").

as_fifo_test() ->
    LRU@0 = erl_lru:new(),
    LRU@1 = erl_lru:push(a, 1, LRU@0),
    LRU@2 = erl_lru:push(b, 2, LRU@1),
    { Key@1, Value@1, LRU@3 } = erl_lru:pop(LRU@2),
    ?assertEqual(a, Key@1),
    ?assertEqual(1, Value@1),
    { Key@2, Value@2, LRU@4 } = erl_lru:pop(LRU@3),
    ?assertEqual(b, Key@2),
    ?assertEqual(2, Value@2),
    ?assertEqual(true, erl_lru:empty(LRU@4)),
    ok.

remove_from_fifo_test() ->
    LRU@0 = erl_lru:new(),
    LRU@1 = erl_lru:push(a, 1, LRU@0),
    LRU@2 = erl_lru:push(todelete, 2, LRU@1),
    LRU@3 = erl_lru:push(b, 2, LRU@2),
    { 2, LRU@4 } = erl_lru:take(todelete, LRU@3),
    { Key@1, Value@1, LRU@5 } = erl_lru:pop(LRU@4),
    ?assertEqual(a, Key@1),
    ?assertEqual(1, Value@1),
    { Key@2, Value@2, LRU@6 } = erl_lru:pop(LRU@5),
    ?assertEqual(b, Key@2),
    ?assertEqual(2, Value@2),
    ?assertEqual(true, erl_lru:empty(LRU@6)),
    ?debugFmt("~p", [ LRU@6 ]),
    ok.

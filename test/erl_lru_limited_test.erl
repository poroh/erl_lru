%%
%% Copyright (c) 2019 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Tests using LRU with limits
%%

-module(erl_lru_limited_test).

-include_lib("eunit/include/eunit.hrl").

basic_replacement_test() ->
    L0 = erl_lru:new(2),
    L1 = erl_lru:add(key1, value1, L0),
    L2 = erl_lru:add(key2, value2, L1),
    L3 = erl_lru:add(key3, value3, L2),
    ?assertEqual(false, erl_lru:has_key(key1, L3)),
    ?assertEqual(true, erl_lru:has_key(key2, L3)),
    ?assertEqual(true, erl_lru:has_key(key3, L3)),
    ?assertMatch({ok, value2, _}, erl_lru:lookup_and_update(key2, L3)),
    {ok, _, L4} = erl_lru:lookup_and_update(key2, L3),
    L5 = erl_lru:add(key4, value4, L4),
    ?assertEqual(true, erl_lru:has_key(key2, L5)),
    ?assertEqual(true, erl_lru:has_key(key4, L5)),
    ?assertEqual(false, erl_lru:has_key(key3, L5)),
    ok.

take_test() ->
    L0 = erl_lru:new(2),
    L1 = erl_lru:add(key1, value1, L0),
    L2 = erl_lru:add(key2, value2, L1),
    ?assertMatch({value2, _}, erl_lru:take(key2, L2)),
    {_, L3} = erl_lru:take(key2, L2),
    %% check that there is no key after take
    ?assertEqual(false, erl_lru:has_key(key2, L3)),
    %% check that there key cannot be updated after taken
    ?assertEqual(error, erl_lru:lookup_and_update(key2, L3)),
    %% check that new added key after take is added into LRU without
    %% replacement any other item
    L4 = erl_lru:add(key3, value3, L3),
    ?assertEqual(true, erl_lru:has_key(key1, L4)),
    ?assertEqual(true, erl_lru:has_key(key3, L4)),
    %% check that adding another element shift out the key1:
    L5 = erl_lru:add(key4, value4, L4),
    ?assertEqual(false, erl_lru:has_key(key1, L5)),
    ?assertEqual(true, erl_lru:has_key(key3, L5)),
    ?assertEqual(true, erl_lru:has_key(key4, L5)),
    %% Check that LRU become empty after all elements taken:
    {value3, L6} = erl_lru:take(key3, L5),
    {value4, L7} = erl_lru:take(key4, L6),
    ?assertEqual(true, erl_lru:empty(L7)),
    %% Check that taken of uknown element returns error:
    ?assertEqual(error, erl_lru:take(key0, L2)),
    ?assertEqual(error, erl_lru:take(key4, L7)),
    ok.

empty_test() ->
    L0 = erl_lru:new(2),
    L1 = erl_lru:add(key1, value1, L0),
    L2 = erl_lru:add(key2, value2, L1),
    {_, L3} = erl_lru:take(key1, L2),
    {_, L4} = erl_lru:take(key2, L3),
    ?assertEqual(true,  erl_lru:empty(L0)),
    ?assertEqual(false, erl_lru:empty(L1)),
    ?assertEqual(false, erl_lru:empty(L2)),
    ?assertEqual(false, erl_lru:empty(L3)),
    ?assertEqual(true,  erl_lru:empty(L4)),
    ok.

add_test() ->
    TestF = fun(Size) ->
                    LNew = erl_lru:new(Size),
                    LFull = lists:foldl(fun(N, LRU) -> erl_lru:add({key, N}, {value, N}, LRU) end,
                                        LNew,
                                        lists:seq(1, Size)),
                    ?assertEqual(Size, erl_lru:size(LFull)),
                    ?assertEqual(Size, erl_lru:size(erl_lru:add(other, other, LFull))),
                    ?assertMatch({{value, 1}, _}, erl_lru:take({key, 1}, LFull))
            end,
    TestF(10000),
    ok.

add_as_update_test() ->
    L0 = erl_lru:new(2),
    L1 = erl_lru:add(key1, value1, L0),
    L2 = erl_lru:add(key2, value2, L1),
    L3 = erl_lru:add(key3, value3, L2),
    ?assertEqual(false, erl_lru:has_key(key1, L3)),
    ?assertEqual(true, erl_lru:has_key(key2, L3)),
    ?assertEqual(true, erl_lru:has_key(key3, L3)),
    L4 = erl_lru:add(key2, value2, L3),
    L5 = erl_lru:add(key4, value4, L4),
    ?assertEqual(true, erl_lru:has_key(key2, L5)),
    ?assertEqual(true, erl_lru:has_key(key4, L5)),
    ?assertEqual(false, erl_lru:has_key(key3, L5)),
    ok.

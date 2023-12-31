-module(sorting_tests).
-include_lib("eunit/include/eunit.hrl").

equal_test() ->
    ?assert(sorting:equal([], [])),
    ?assertNot(sorting:equal([], [1])),
    ?assertNot(sorting:equal([1], [])),
    ?assert(sorting:equal([1], [1])),
    ?assert(sorting:equal([1, 2], [1, 2])),
    ?assertNot(sorting:equal([1, 3], [1, 2])).

random_list_test() ->
    ?assertEqual(sorting:random_list(0, 1, 10), []),
    ?assertEqual(sorting:random_list(5, 10, 1), []),
    List = sorting:random_list(1000, 10, 20),
    ?assert(lists:all(fun(Element) -> (Element >= 10 andalso Element < 20) end, List)).

is_sorted_test() ->
    Asc = fun(A, B) -> A =< B end,
    Desc = fun(A, B) -> A >= B end,
    ?assert(sorting:is_sorted([], Asc)),
    ?assert(sorting:is_sorted([], Desc)),
    ?assert(sorting:is_sorted([1], Asc)),
    ?assert(sorting:is_sorted([1], Desc)),
    ?assert(sorting:is_sorted([1, 2], Asc)),
    ?assertNot(sorting:is_sorted([1, 2], Desc)),
    ?assertNot(sorting:is_sorted([2, 1], Asc)),
    ?assert(sorting:is_sorted([2, 1], Desc)),
    ?assert(sorting:is_sorted([1, 2, 3, 4, 5], Asc)),
    ?assertNot(sorting:is_sorted([1, 2, 3, 4, 5], Desc)),
    ?assertNot(sorting:is_sorted([5, 4, 3, 2, 1], Asc)),
    ?assert(sorting:is_sorted([5, 4, 3, 2, 1], Desc)).

is_sorted_asc_test() ->
    ?assert(sorting:is_sorted_asc([])),
    ?assert(sorting:is_sorted_asc([1])),
    ?assert(sorting:is_sorted_asc([1, 2])),
    ?assertNot(sorting:is_sorted_asc([2, 1])),
    ?assert(sorting:is_sorted_asc([1, 2, 3, 4, 5])),
    ?assertNot(sorting:is_sorted_asc([5, 4, 3, 2, 1])).

is_sorted_desc_test() ->
    ?assert(sorting:is_sorted_desc([])),
    ?assert(sorting:is_sorted_desc([1])),
    ?assertNot(sorting:is_sorted_desc([1, 2])),
    ?assert(sorting:is_sorted_desc([2, 1])),
    ?assertNot(sorting:is_sorted_desc([1, 2, 3, 4, 5])),
    ?assert(sorting:is_sorted_desc([5, 4, 3, 2, 1])).

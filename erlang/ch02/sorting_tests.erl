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

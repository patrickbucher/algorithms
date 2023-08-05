-module(linear_search_tests).
-include_lib("eunit/include/eunit.hrl").

linear_search_test() ->
    ?assertEqual(linear_search:search([], 1), -1),
    ?assertEqual(linear_search:search([1], 1), 0),
    ?assertEqual(linear_search:search([1, 2, 3], 4), -1),
    ?assertEqual(linear_search:search([1, 2, 3, 4, 5], 4), 3),
    ?assertEqual(linear_search:search([5, 1, 3, 2, 4], 4), 4).

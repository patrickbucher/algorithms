-module(binary_search_tests).
-include_lib("eunit/include/eunit.hrl").

binary_search_test() ->
    ?assertEqual(binary_search:search([], 1), -1),
    ?assertEqual(binary_search:search([1], 1), 0),
    ?assertEqual(binary_search:search([1, 2, 3], 4), -1),
    ?assertEqual(binary_search:search([1, 2, 3, 4, 5], 4), 3),
    ?assertEqual(binary_search:search([0, 1, 2, 3, 4, 5, 6, 7], 0), 0),
    ?assertEqual(binary_search:search([0, 1, 2, 3, 4, 5, 6, 7], 1), 1),
    ?assertEqual(binary_search:search([0, 1, 2, 3, 4, 5, 6, 7], 3), 3),
    ?assertEqual(binary_search:search([0, 1, 2, 3, 4, 5, 6, 7], 4), 4),
    ?assertEqual(binary_search:search([0, 1, 2, 3, 4, 5, 6, 7], 7), 7),
    ?assertEqual(binary_search:search([0, 1, 2, 3, 4, 5, 6, 7], 8), -1).

-module(binaries_tests).
-include_lib("eunit/include/eunit.hrl").

add_binary_test() ->
    ?assert(sorting:equal(binaries:add_binary([], []), [])),
    ?assert(sorting:equal(binaries:add_binary([0], []), [])),
    ?assert(sorting:equal(binaries:add_binary([], [0]), [])),
    ?assert(sorting:equal(binaries:add_binary([0], [0]), [0, 0])),
    ?assert(sorting:equal(binaries:add_binary([0], [1]), [1, 0])),
    ?assert(sorting:equal(binaries:add_binary([1], [0]), [1, 0])),
    ?assert(sorting:equal(binaries:add_binary([1], [1]), [0, 1])),
    ?assert(sorting:equal(binaries:add_binary([1, 1, 1], [0, 0, 1]), [1, 1, 0, 1])),
    ?assert(sorting:equal(binaries:add_binary([1, 0, 1, 0], [0, 1, 0, 1]), [1, 1, 1, 1, 0])).

binary_to_decimal_test() ->
    ?assertEqual(binaries:binary_to_decimal([]), 0),
    ?assertEqual(binaries:binary_to_decimal([0]), 0),
    ?assertEqual(binaries:binary_to_decimal([1]), 1),
    ?assertEqual(binaries:binary_to_decimal([0, 1]), 2),
    ?assertEqual(binaries:binary_to_decimal([1, 1]), 3),
    ?assertEqual(binaries:binary_to_decimal([0, 0, 1]), 4),
    ?assertEqual(binaries:binary_to_decimal([1, 0, 1]), 5),
    ?assertEqual(binaries:binary_to_decimal([0, 1, 1]), 6),
    ?assertEqual(binaries:binary_to_decimal([1, 1, 1]), 7),
    ?assertEqual(binaries:binary_to_decimal([0, 0, 0, 1]), 8).

decimal_to_binary_test() ->
    ?assert(sorting:equal(binaries:decimal_to_binary(0), [0])),
    ?assert(sorting:equal(binaries:decimal_to_binary(1), [1])),
    ?assert(sorting:equal(binaries:decimal_to_binary(2), [0, 1])),
    ?assert(sorting:equal(binaries:decimal_to_binary(3), [1, 1])),
    ?assert(sorting:equal(binaries:decimal_to_binary(4), [0, 0, 1])),
    ?assert(sorting:equal(binaries:decimal_to_binary(5), [1, 0, 1])),
    ?assert(sorting:equal(binaries:decimal_to_binary(6), [0, 1, 1])),
    ?assert(sorting:equal(binaries:decimal_to_binary(7), [1, 1, 1])),
    ?assert(sorting:equal(binaries:decimal_to_binary(8), [0, 0, 0, 1])).

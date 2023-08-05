-module(binaries_tests).
-include_lib("eunit/include/eunit.hrl").

add_binary_test() ->
    ?assert(sorting:equal(binaries:add_binary([], []), [])),
    ?assert(sorting:equal(binaries:add_binary([0], []), [])),
    ?assert(sorting:equal(binaries:add_binary([], [0]), [])),
    ?assert(sorting:equal(binaries:add_binary([0], [0]), [0, 0])).

-module(sorting_tests).
-include_lib("eunit/include/eunit.hrl").

equal_test() ->
    ?assert(sorting:equal([], [])),
    ?assertNot(sorting:equal([], [1])),
    ?assertNot(sorting:equal([1], [])),
    ?assert(sorting:equal([1], [1])),
    ?assert(sorting:equal([1, 2], [1, 2])),
    ?assertNot(sorting:equal([1, 3], [1, 2])).

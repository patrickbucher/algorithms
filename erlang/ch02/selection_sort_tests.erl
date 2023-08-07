-module(selection_sort_tests).
-include_lib("eunit/include/eunit.hrl").

selection_sort_test() ->
    ?assert(sorting:equal(selection_sort:sort([]), [])),
    ?assert(sorting:equal(selection_sort:sort([1]), [1])),
    ?assert(sorting:equal(selection_sort:sort([1, 2]), [1, 2])),
    ?assert(sorting:equal(selection_sort:sort([2, 1]), [1, 2])),
    ?assert(sorting:equal(selection_sort:sort([2, 1, 3]), [1, 2, 3])),
    ?assert(sorting:equal(selection_sort:sort([5, 1, 3, 4, 2]), [1, 2, 3, 4, 5])).

selection_sort_big_test() ->
    List = sorting:random_list(1000, 0, 1000),
    Sorted = selection_sort:sort(List),
    ?assert(sorting:is_sorted_asc(Sorted)).

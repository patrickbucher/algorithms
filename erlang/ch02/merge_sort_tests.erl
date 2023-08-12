-module(merge_sort_tests).
-include_lib("eunit/include/eunit.hrl").

merge_sort_test() ->
    ?assert(sorting:equal(merge_sort:sort([]), [])),
    ?assert(sorting:equal(merge_sort:sort([1]), [1])),
    ?assert(sorting:equal(merge_sort:sort([1, 2]), [1, 2])),
    ?assert(sorting:equal(merge_sort:sort([2, 1]), [1, 2])),
    ?assert(sorting:equal(merge_sort:sort([2, 1, 3]), [1, 2, 3])),
    ?assert(sorting:equal(merge_sort:sort([5, 1, 3, 4, 2]), [1, 2, 3, 4, 5])).

merge_sort_big_test() ->
    List = sorting:random_list(1000, 0, 1000),
    Sorted = merge_sort:sort(List),
    ?assert(sorting:is_sorted_asc(Sorted)).

parallel_merge_sort_test() ->
    ?assert(sorting:equal(merge_sort:sort_parallel([]), [])),
    ?assert(sorting:equal(merge_sort:sort_parallel([1]), [1])),
    ?assert(sorting:equal(merge_sort:sort_parallel([1, 2]), [1, 2])),
    ?assert(sorting:equal(merge_sort:sort_parallel([2, 1]), [1, 2])),
    ?assert(sorting:equal(merge_sort:sort_parallel([2, 1, 3]), [1, 2, 3])),
    ?assert(sorting:equal(merge_sort:sort_parallel([5, 1, 3, 4, 2]), [1, 2, 3, 4, 5])).

parallel_merge_sort_big_test() ->
    N = trunc(1.0e6),
    List = sorting:random_list(N, 0, N),
    Sorted = merge_sort:sort_parallel(List),
    ?assert(sorting:is_sorted_asc(Sorted)).

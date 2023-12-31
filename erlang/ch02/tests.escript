#!/usr/bin/env escript

main(_) ->
    compile:file("./sorting.erl"),
    compile:file("./sorting_tests.erl"),
    eunit:test(sorting),

    compile:file("./insertion_sort.erl"),
    compile:file("./insertion_sort_tests.erl"),
    eunit:test(insertion_sort),

    compile:file("./linear_search.erl"),
    compile:file("./linear_search_tests.erl"),
    eunit:test(linear_search),

    compile:file("./binaries.erl"),
    compile:file("./binaries_tests.erl"),
    eunit:test(binaries),

    compile:file("./selection_sort.erl"),
    compile:file("./selection_sort_tests.erl"),
    eunit:test(selection_sort),

    compile:file("./merge_sort.erl"),
    compile:file("./merge_sort_tests.erl"),
    eunit:test(merge_sort),

    compile:file("./binary_search.erl"),
    compile:file("./binary_search_tests.erl"),
    eunit:test(binary_search),

    compile:file("./bubble_sort.erl"),
    compile:file("./bubble_sort_tests.erl"),
    eunit:test(bubble_sort).

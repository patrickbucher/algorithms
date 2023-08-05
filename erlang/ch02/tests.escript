#!/usr/bin/env escript

main(_) ->
    compile:file("./sorting.erl"),
    compile:file("./sorting_tests.erl"),
    eunit:test(sorting),
    compile:file("./insertion_sort.erl"),
    compile:file("./insertion_sort_tests.erl"),
    eunit:test(insertion_sort).

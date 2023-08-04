#!/usr/bin/env escript

main(_) ->
    compile:file("./sorting.erl"),
    compile:file("./sorting_tests.erl"),
    eunit:test(sorting).

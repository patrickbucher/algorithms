#!/usr/bin/env escript

calculate_multipliers(Durations, Factors) ->
    Mul = fun(Acc, X) -> Acc * X end,
    Fn = fun({I, D}) -> {D, lists:foldl(Mul, 1, lists:sublist(Factors, 1, I))} end,
    lists:map(Fn, lists:enumerate(Durations)).

main(_) ->
    Durations = ["second", "minute", "hour", "day", "month", "year", "century"],
    Factors = [1000000, 60, 60, 24, 30, 12, 100],
    Multipliers = calculate_multipliers(Durations, Factors),
    io:format("~p\n~p\n~p\n", [Durations, Factors, Multipliers]).

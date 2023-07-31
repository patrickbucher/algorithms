#!/usr/bin/env escript

calculate_multipliers(Durations, Factors) ->
    Mul = fun(Acc, X) -> Acc * X end,
    Fn = fun({I, D}) -> {D, lists:foldl(Mul, 1, lists:sublist(Factors, 1, I))} end,
    lists:map(Fn, lists:enumerate(Durations)).

factorial(0) -> 1;
factorial(N) when N > 0 -> factorial(N, 1).
factorial(0, Acc) -> Acc;
factorial(N, Acc) when N > 0 -> factorial(N - 1, N * Acc).

find_max_n(OrderFn, LastN, N, GrowthFn, Limit) ->
    Required = OrderFn(N),
    case Required =< Limit of
        true -> find_max_n(OrderFn, N, GrowthFn(N), GrowthFn, Limit);
        false -> LastN
    end.

main(_) ->
    Durations = ["second", "minute", "hour", "day", "month", "year", "century"],
    Factors = [1000000, 60, 60, 24, 30, 12, 100],
    Multipliers = calculate_multipliers(Durations, Factors),
    Orders = [{"lg n", fun(N) -> math:log10(N) end},
              {"√n", fun(N) -> math:sqrt(N) end},
              {"n", fun(N) -> N end},
              {"n lg n", fun(N) -> math:log10(N) end},
              {"n²", fun(N) -> math:pow(N, 2) end},
              {"2^n", fun(N) -> math:pow(2, N) end},
              {"n!", fun(N) -> factorial(N) end}],
    Computations = [{Mul, Ord} || Mul <- Multipliers, Ord <- Orders],
    io:format("~p\n~p\n~p\n~p\n~p\n", [Durations, Factors, Multipliers, Orders, Computations]).

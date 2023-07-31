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
    case Required < Limit of
        true ->
            NextN = try GrowthFn(N) of R -> R catch _:_ -> LastN end,
            case NextN > N of
                true -> find_max_n(OrderFn, N, NextN, GrowthFn, Limit);
                false -> N
            end;
        false -> LastN
    end.

main(_) ->
    Durations = ["second", "minute", "hour", "day", "month", "year", "century"],
    Factors = [1000000, 60, 60, 24, 30, 12, 100],
    Multipliers = calculate_multipliers(Durations, Factors),
    Orders = [{"lg n", fun(N) -> math:log10(N) end},
              {"sqrt(n)", fun(N) -> math:sqrt(N) end},
              {"n", fun(N) -> N end},
              {"n lg n", fun(N) -> math:log10(N) end},
              {"n²", fun(N) -> math:pow(N, 2) end},
              {"n³", fun(N) -> math:pow(N, 3) end},
              {"2^n", fun(N) -> math:pow(2, N) end},
              {"n!", fun(N) -> factorial(round(N)) end}],
    Computations = [{Mul, Ord} || Ord <- Orders, Mul <- Multipliers],
    Results = lists:map(fun({{Dur, Ms}, {Ord, Fn}}) ->
                                N = find_max_n(Fn, 0.0, 10.0, fun(N) -> N * 1.001 end, Ms),
                                {Dur, Ord, N}
                        end, Computations),
    lists:foreach(fun({Dur, Ord, N}) -> io:format("~s ~s: n=~.3e\n", [Dur, Ord, N]) end, Results).

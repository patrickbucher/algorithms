-module(sorting).
-export([equal/2, random_list/3]).

equal([], []) -> true;
equal([_], []) -> false;
equal([], [_]) -> false;
equal([HL|TL], [HR|TR]) ->
    HL =:= HR andalso equal(TL, TR).

random_list(0, _, _) -> [];
random_list(_, Min, Max) when Min >= Max -> [];
random_list(N, Min, Max) ->
    random_list(N, Min, Max, []).

random_list(0, _, _, Acc) -> Acc;
random_list(N, Min, Max, Acc) ->
    Random = rand:uniform(Max - Min - 1) + Min,
    random_list(N - 1, Min, Max, [Random|Acc]).

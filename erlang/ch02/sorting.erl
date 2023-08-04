-module(sorting).
-export([equal/2, random_list/3, is_sorted/2, is_sorted_asc/1, is_sorted_desc/1]).

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

is_sorted([], _) -> true;
is_sorted([_], _) -> true;
is_sorted([H|T], Cmp) -> is_sorted(H, T, Cmp).

is_sorted(_, [], _) -> true;
is_sorted(Last, [H], Cmp) -> Cmp(Last, H);
is_sorted(Last, [H|T], Cmp) -> Cmp(Last, H) andalso is_sorted(H, T, Cmp).

is_sorted_asc(L) -> is_sorted(L, fun(A, B) -> A =< B end).

is_sorted_desc(L) -> is_sorted(L, fun(A, B) -> A >= B end).

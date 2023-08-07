-module(selection_sort).
-export([sort/1]).

sort([]) -> [];
sort([H]) -> [H];
sort(List) -> sort(List, []).

sort([], Acc) -> Acc;
sort([H], Acc) -> Acc ++ [H];
sort(List, Acc) ->
    [Smallest|Rest] = smallest_to_front(List),
    sort(Rest, Acc ++ [Smallest]).

smallest_to_front([H]) -> [H];
smallest_to_front([H|T]) ->
    smallest_to_front([], T, H).

smallest_to_front(Skipped, [], Smallest) -> [Smallest|Skipped];
smallest_to_front(Skipped, [H|T], Smallest) when H < Smallest ->
    smallest_to_front(Skipped ++ [Smallest], T, H);
smallest_to_front(Skipped, [H|T], Smallest) ->
    smallest_to_front(Skipped ++ [H], T, Smallest).

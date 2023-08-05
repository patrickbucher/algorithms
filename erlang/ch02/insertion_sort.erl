-module(insertion_sort).
-export([sort/1]).

sort([]) -> [];
sort([H]) -> [H];
sort(List) -> sort(List, []).

sort([], Acc) -> Acc;
sort([H], Acc) -> insert_into(H, Acc);
sort([H|T], Acc) -> sort(T, insert_into(H, Acc)).

insert_into(E, Acc) -> insert_into([], E, Acc).

insert_into(Left, E, []) -> Left ++ [E];
insert_into(Left, E, [H]) ->
    if E > H -> Left ++ [H, E];
       true -> Left ++ [E, H]
    end;
insert_into(Left, E, [H|T]) when E > H -> insert_into(Left ++ [H], E, T);
insert_into(Left, E, [H|T]) -> Left ++ [E, H|T].

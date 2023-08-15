-module(bubble_sort).
-export([sort/1]).

sort([]) -> [];
sort([H]) -> [H];
sort(List) -> 
    N = length(List),
    sort(List, N, 1, N).

sort(List, N, I, _) when I =:= N -> List;
sort(List, N, I, J) when J =:= I -> sort(List, N, I+1, N);
sort(List, N, I, J) ->
    Right = lists:nth(J, List),
    Left = lists:nth(J-1, List),
    if
        Right < Left ->
            sort(swap(List, J, J-1), N, I, J-1);
        true ->
            sort(List, N, I, J-1)
    end.

swap(List, I, J) when I =:= J -> List;
swap(List, I, J) when I > J -> swap(List, J, I);
swap(List, I, J) ->
    N = length(List),
    X = lists:nth(I, List),
    Y = lists:nth(J, List),
    L = lists:sublist(List, 1, I-1),
    M = lists:sublist(List, I+1, J-I-1),
    R = lists:sublist(List, J+1, N-J),
    L ++ [Y] ++ M ++ [X] ++ R.

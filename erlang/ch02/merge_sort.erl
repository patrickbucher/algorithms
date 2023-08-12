-module(merge_sort).
-export([sort/1]).

sort([]) -> [];
sort([H]) -> [H];
sort(List) -> merge_sort(List).

merge_sort([]) -> [];
merge_sort([H]) -> [H];
merge_sort(List) ->
    N = length(List),
    Q = N div 2,
    Left = merge_sort(lists:sublist(List, 1, Q)),
    Right = merge_sort(lists:sublist(List, Q+1, N-Q)),
    merge(Left, Right, []).

merge([], [], Acc) -> lists:reverse(Acc);
merge(Left, [], Acc) -> lists:reverse(Acc) ++ Left;
merge([], Right, Acc) -> lists:reverse(Acc) ++ Right;
merge([HL|TL], [HR|TR], Acc) when HL < HR ->
    merge(TL, [HR|TR], [HL|Acc]);
merge([HL|TL], [HR|TR], Acc) ->
    merge([HL|TL], TR, [HR|Acc]).

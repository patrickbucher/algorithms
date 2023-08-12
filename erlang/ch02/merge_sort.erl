-module(merge_sort).
-export([sort/1]).

sort([]) -> [];
sort([H]) -> [H];
sort(List) ->
    N = length(List),
    Q = N div 2,
    Left = sort(lists:sublist(List, 1, Q)),
    Right = sort(lists:sublist(List, Q+1, N-Q)),
    merge(Left, Right, []).

merge([], [], Acc) -> lists:reverse(Acc);
merge(Left, [], Acc) -> lists:reverse(Acc) ++ Left;
merge([], Right, Acc) -> lists:reverse(Acc) ++ Right;
merge([HL|TL], [HR|TR], Acc) when HL < HR ->
    merge(TL, [HR|TR], [HL|Acc]);
merge([HL|TL], [HR|TR], Acc) ->
    merge([HL|TL], TR, [HR|Acc]).

-module(binaries).
-export([add_binary/2, binary_to_decimal/1, decimal_to_binary/1]).

add_binary([], []) -> [];
add_binary(L, R) ->
    if length(L) =/= length(R) -> [];
       true -> add_binary(L, R, [], 0)
    end.

add_binary([], [], C, Carry) -> lists:reverse([Carry|C]);
add_binary([HL|TL], [HR|TR], C, Carry) ->
    Bit = HL + HR + Carry,
    case Bit of
        0 -> add_binary(TL, TR, [0|C], 0);
        1 -> add_binary(TL, TR, [1|C], 0);
        2 -> add_binary(TL, TR, [0|C], 1);
        3 -> add_binary(TL, TR, [1|C], 1);
        _ -> []
    end.

binary_to_decimal([]) -> 0;
binary_to_decimal(Bin) -> binary_to_decimal(Bin, 0, 0).

binary_to_decimal([], Acc, _) -> Acc;
binary_to_decimal([H|T], Acc, I) ->
    binary_to_decimal(T, Acc + H * round(math:pow(2, I)), I + 1).

decimal_to_binary(0) -> [0];
decimal_to_binary(X) -> decimal_to_binary(X, []).

decimal_to_binary(0, Acc) -> lists:reverse(Acc);
decimal_to_binary(X, Acc) ->
    Bit = X rem 2,
    Rem = X div 2,
    decimal_to_binary(Rem, [Bit|Acc]).

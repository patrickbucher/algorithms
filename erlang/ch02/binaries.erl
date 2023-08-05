-module(binaries).
-export([add_binary/2]).

add_binary([], []) -> [];
add_binary(L, R) ->
    if length(L) =/= length(R) -> [];
       true -> add_binary(L, R, [])
    end.

add_binary(_, _, C) -> C.

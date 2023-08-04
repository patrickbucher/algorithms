-module(sorting).
-export([equal/2]).

equal([], []) -> true;
equal([_], []) -> false;
equal([], [_]) -> false;
equal([HL|TL], [HR|TR]) ->
    HL =:= HR andalso equal(TL, TR).

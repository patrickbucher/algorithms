-module(binary_search).
-export([search/2]).

search([], _) -> -1;
search(Haystack, Needle) -> search(Haystack, Needle, 0, length(Haystack)).

search([], _, _, _) -> -1;
search([H], H, Lower, _) -> Lower;
search(_, _, Lower, Upper) when Lower >= Upper -> -1;
search(Haystack, Needle, Lower, Upper) ->
    Middle = Lower + (Upper - Lower) div 2,
    Element = lists:nth(Middle+1, Haystack),
    if
        Element =:= Needle ->
            Middle;
        Element > Needle ->
            search(Haystack, Needle, Lower, Middle);
        true ->
            search(Haystack, Needle, Middle + 1, Upper)
    end.

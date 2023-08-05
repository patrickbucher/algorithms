-module(linear_search).
-export([search/2]).

search([], _) -> -1;
search(Haystack, Needle) -> search(Haystack, Needle, 0).

search([], _, _) -> -1;
search([H|_], H, Acc) -> Acc;
search([_|T], H, Acc) -> search(T, H, Acc + 1).

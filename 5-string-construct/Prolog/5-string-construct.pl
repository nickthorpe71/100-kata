% Helper predicates

% count_chars(+List, -Count)
% Counts the number of characters in a list.
count_chars([], 0).
count_chars([_|T], Count) :- count_chars(T, Count1), Count is Count1 + 1.

% string_concatenate(+String1, +String2, -Result)
% Concatenates two string lists.
string_concatenate([], String, String).
string_concatenate([H|T], String, [H|Result]) :- string_concatenate(T, String, Result).

% slice(+List, +Start, +End, -Sliced)
% Slices a list from Start to End (exclusive).
slice(_, 0, 0, []).
slice([H|T], 0, End, [H|Rest]) :- End > 0, NewEnd is End - 1, slice(T, 0, NewEnd, Rest).
slice([_|T], Start, End, Result) :- Start > 0, NewStart is Start - 1, NewEnd is End - 1, slice(T, NewStart, NewEnd, Result).

% string_constructing(+A, +S, -Ops)
% Main function to construct string S from string A.
string_constructing(A, S, Ops) :- construct_string(A, S, 0, [], 0, Ops).

construct_string(_, S, _, Curr, Ops, Ops) :- S == Curr.
construct_string(A, S, Index, Curr, CurrentOps, Ops) :-
    count_chars(S, LenS), count_chars(Curr, LenCurr),
    (Index < LenS, Index < LenCurr, nth0(Index, S, CharS), nth0(Index, Curr, CharC), CharS == CharC -> 
        NewIndex is Index + 1, construct_string(A, S, NewIndex, Curr, CurrentOps, Ops);
    Index >= LenCurr -> 
        string_concatenate(Curr, A, NewCurr), NewOps is CurrentOps + 1, construct_string(A, S, Index, NewCurr, NewOps, Ops);
        slice(Curr, 0, Index, Front), NewIndex is Index + 1, slice(Curr, NewIndex, LenCurr, Back), string_concatenate(Front, Back, NewCurr), NewOps is CurrentOps + 1, construct_string(A, S, 0, NewCurr, NewOps, Ops)).


main :-
  statistics(walltime, [_ | [_]]),
  string_constructing("a", "a", Ops1),
  %string_constructing("aba", "abbabba", Ops2), write(Ops2), nl,
  %string_constructing("a", "aaa", Ops3), write(Ops3), nl,
  %string_constructing("bbaabcbcbc", "bbcccbabcc", Ops4), write(Ops4), nl,
  statistics(walltime, [_ | [ExecutionTime]]),
  format('Execution took ~3d seconds.~n', [ExecutionTime]),
  halt.

:- initialization(main).

% Define a predicate to check if a number is even
is_even(N) :-
  N mod 2 =:= 0.

% Define a predicate to check if a number is odd
is_odd(N) :-
  not(is_even(N)).

% Define a predicate to print whether a number is even or odd
print_even_or_odd(N) :-
  is_even(N),
  write(N), write(' is even.'), nl.
print_even_or_odd(N) :-
  is_odd(N),
  write(N), write(' is odd.'), nl.

% Test examples
:- initialization(main).
main :-
  statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
  print_even_or_odd(12),
  print_even_or_odd(11),
  statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
  format('Execution took ~3d seconds.~n', [ExecutionTime]),
  halt.


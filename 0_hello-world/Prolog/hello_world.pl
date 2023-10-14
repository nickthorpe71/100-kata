% Define the greet module and export the greet/0 predicate
:- module(greet, [greet/0, greet_improved/0, main/0]).

% Set the initialization predicate to greet/0
:- initialization(main).

% Define the greet predicate that prints 'Hello World!'
greet :-
  write('Hello World!'), nl. 

greet_improved :- format("Hello World!~n").

main :-
  greet,
  greet_improved,
  halt.



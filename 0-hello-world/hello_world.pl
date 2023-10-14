% Define the greet module and export the greet/0 predicate
:- module(greet, [greet/0]).

% Set the initialization predicate to greet/0
:- initialization(greet).

% Define the greet predicate that prints 'Hello World!'
greet :-
    write('Hello World!'), nl, halt.

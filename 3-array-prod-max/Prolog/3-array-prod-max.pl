% Define a predicate to generate a list of N random integers
generateRandomNumbers(N, Max, List) :-
  N > 0,                                     % Check if N is greater than 0
  NewN is N - 1,                             % Decrement N
  random(0, Max, R),                         % Generate a random number between 0 and Max and save to R
  generateRandomNumbers(NewN, Max, NewList), % Recursively generate the rest of the list, passing Max along
  append([R], NewList, List).                % Add the random number R to the list
generateRandomNumbers(0, _, []).             % Base case: if N is 0, the list is empty. Added _ to ignore Max when N is 0


% Define a predicate to calculate the product of N max numbers
maxProduct(L, N, R) :-
  N > 0,
  sort(0, @>=, L, Sorted),
  take(N, Sorted, MaxNNumbers),
  product(MaxNNumbers, R).

% Alternate solution
max_product(Numbers, Size, Result) :-
  msort(Numbers, Sorted),
  length(Group, Size),
  append(_, Group, Sorted),
  foldl([A,B,R]>>(R is A*B), Group, 1, Result).

% Define a predicate to take the first N numbers from a list
take(0, _, []).
take(N, [H|T], [H|Rest]) :-
  N > 0,
  N1 is N - 1,
  take(N1, T, Rest).

product(L, P) :-
  product(L, 1, P).
 
product([], P, P).
product([H|T], Acc, P) :-
  NewAcc is Acc * H,
  product(T, NewAcc, P).

% Test case predicate to run a test
testCase(N, List) :-
  % writeln('Testing with list: '),
  % writeln(List),
  max_product(List, N, Product),
  write('Result: '),
  writeln(Product),
  nl.  % New line for better readability

% Main predicate to run all test cases
main :-
  generateRandomNumbers(1000000, 100, RandomNumbers),
  statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
  testCase(5, RandomNumbers),
  statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
  format('Execution took ~3d seconds.~n', [ExecutionTime]),
  halt.

% Initialization directive
:- initialization(main).


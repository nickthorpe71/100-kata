% Define a predicate to generate a list of N random integers
generateRandomNumbers(N, Max, List) :-
  N > 0,                                     % Check if N is greater than 0
  NewN is N - 1,                             % Decrement N
  random(0, Max, R),                         % Generate a random number between 0 and Max and save to R
  generateRandomNumbers(NewN, Max, NewList), % Recursively generate the rest of the list, passing Max along
  append([R], NewList, List).                % Add the random number R to the list
generateRandomNumbers(0, _, []).             % Base case: if N is 0, the list is empty. Added _ to ignore Max when N is 0


% Define a predicate to split a list into two halves 
splitList(L, A, B) :- 
  length(L, N),
  H is N - N//2,
  length(A, H),
  append(A, B, L).

% Define a predicate to reverse a list 
reverseList([], Z, Z).
reverseList([H|T], Z, Acc) :- reverseList(T, Z, [H|Acc]).

% Define a predicate to calculate the sum of products of two lists
sumOfProducts([], [], Acc, Acc).
sumOfProducts([H1|T1], [H2|T2], Acc, Sum) :-
  Product is H1 * H2,
  NewAcc is Acc + Product,
  sumOfProducts(T1, T2, NewAcc, Sum).

% Define a predicate to calculate the minimum sum of products
minSum(L, Sum) :-
  sort(0, @=<, L, Sorted), % Sort the list in ascending order
  splitList(Sorted, FirstHalf, SecondHalf), % Split the list into two halves
  reverseList(SecondHalf, ReversedSecondHalf, []), % Reverse the second half
  sumOfProducts(FirstHalf, ReversedSecondHalf, 0, Sum). % Calculate the sum of products

% Test case predicate to run a test
testCase(List) :-
  % writeln('Testing with list: '),
  % writeln(List),
  minSum(List, Sum),
  write('Result: '),
  writeln(Sum),
  nl.  % New line for better readability

% Main predicate to run all test cases
main :-
  generateRandomNumbers(1000000, 100, RandomNumbers),
  statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
  testCase(RandomNumbers),
  statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
  format('Execution took ~3d seconds.~n', [ExecutionTime]),
  halt.

% Initialization directive
:- initialization(main).

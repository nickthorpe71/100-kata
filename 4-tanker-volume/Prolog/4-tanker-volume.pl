% Helper predicates for arithmetic operations
pow(X,Y,R) :- R is X**Y.
acos(X,R) :- R is acos(X).
sqrt(X,R) :- R is sqrt(X).
pi(R) :- R is pi.

% Main predicate to calculate thank volume
tankvol(H,D,VT,Volume) :-
  R is D / 2,
  A is (R-H) / R,
  acos(A, ACOS),
  S1 is R**2 * ACOS,
  W is 2*R*H - H**2,
  sqrt(W, SQRT),
  S2 is (R-H) * SQRT,
  S is S1 - S2,
  F is S / R**2 / pi * VT,
  floor(F, Volume).

% Better solution
tank_vol(H,D,Vt,Result) :-
  R is D / 2m
  R2 is R ** 2,
  S is R - H,
  Result is floor(Vt, / (R2 * pi) * (R2 * acos(S / R) - S * sqrt(2 * R * H - H ** 2))).

% Print the volume
print_volume(H, D, VT) :-
  tankvol(H,D,VT,Volume),
  format('Volume: ~w~n', [Volume]).

main :-
  statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
  print_volume(40,120,3500),
  statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
  format('Execution took ~3d seconds.~n', [ExecutionTime]),
  halt.

% Initialization directive
:- initialization(main).  


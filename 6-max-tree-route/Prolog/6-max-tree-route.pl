% Define the TreeNode as dynamic predicates
:- dynamic leaf/1.
:- dynamic node/3.

% Predicate to create a new TreeNode
new_tree_node(Value, none, none, leaf(Value)).
new_tree_node(Value, Left, none, node(Value, Left, leaf(0))) :-
    nonvar(Left).
new_tree_node(Value, none, Right, node(Value, leaf(0), Right)) :-
    nonvar(Right).
new_tree_node(Value, Left, Right, node(Value, Left, Right)) :-
    nonvar(Left),
    nonvar(Right).

% Predicate to calculate the maximum sum from root to any leaf
max_sum(leaf(Value), Value).
max_sum(node(Value, Left, Right), MaxSum) :-
    max_sum(Left, LeftSum),
    max_sum(Right, RightSum),
    MaxChildSum is max(LeftSum, RightSum),
    MaxSum is Value + MaxChildSum.

% Test examples
main :-
    statistics(walltime, _),

    new_tree_node(17, Left, Right, Tree),
    new_tree_node(3, LLeft, none, Left),
    new_tree_node(2, none, none, LLeft),
    new_tree_node(-10, LRight, RRight, Right),
    new_tree_node(16, none, none, LRight),
    new_tree_node(1, LRRight, none, RRight),
    new_tree_node(13, none, none, LRRight),
    max_sum(Tree, MaxSum),
    write('Max Sum: '), write(MaxSum), nl,

    statistics(walltime, [_ | [ExecutionTime]]),
    ExecutionInSeconds is ExecutionTime / 1000,
    format('Execution took ~2f seconds.~n', [ExecutionInSeconds]),
    halt.

:- initialization(main).

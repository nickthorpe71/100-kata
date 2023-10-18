# Minimize Sum Of Array (Array Series #1)

Upped to a [7kyu kata](https://www.codewars.com/kata/5a523566b3bfa84c2e00010b/solutions/javascript) today. This session made me want to implement a benchmark feature into the problem templates so I can see how each language performs. Today's problem could be tested by running large arrays through the solution from each language. 

**JS**
Started with JS just to easily get the solution figured out and submitted and for some variety.

**Prolog**
Next was Prolog. I found a nice snippit describing Prolog "Prolog is a declarative programming language, meaning that you describe the logic without specifying the control flow. It uses a format of predicate :- clause. to define rules." I really like how the rules are stand alone logic and are therefore very reusable. 

**C**
While solving the problem in C I discovered the `qsort` function from `stdlib.h` and by extension of this I found void pointers. The qsort function is designed to work on arrays of any data type. This also means I needed to cast the arguments to ints to be able to do arithmitic.

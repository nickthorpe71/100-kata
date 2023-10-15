# Even Or Odd

An easy [kata](https://www.codewars.com/kata/53da3dbb4a5168369a0000fe) to create a funcion that retuns whether a number is even or odd.

## Notes

**Haskell**
I started this one in haskell. I learned that pattern matching and recursion were appropriate tools to solve the given problem. I also learned how to implement tests in haskell using HUnit so I could confirm the function I wrote worked correctly. 

**C**
Next up was C. To solve the problem was easy using the modulo operator but I did learn about the `const` keyword in C which is used to declare a constant which means the data the pointer is pointing to is immutable.

**Prolog**
The first new piece of syntax I came across was the `=:=` equality operator. I noticed that the prolog solution appeared to be using pattern matching but after some research found that prolog doesn't use pattern matching in the same way that languages like haskell (functional languages) do. Prolog uses a different mechanism called "unification" to match and evaluate predicates and rules. You define predicates and rules using facts and clauses. Each clause consists for a head and a body, and unification is sued to satisfy the conditions in the body to prove the truth of the head.

**JS**
The JS implementation was very straight forward.

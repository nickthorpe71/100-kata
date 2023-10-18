# Minimize Sum Of Array (Array Series #1)

Upped to a [7kyu kata](https://www.codewars.com/kata/5a523566b3bfa84c2e00010b/solutions/javascript) today. This session made me want to implement a benchmark feature into the problem templates so I can see how each language performs. Today's problem could be tested by running large arrays through the solution from each language. 

**JS**
Started with JS just to easily get the solution figured out and submitted and for some variety.

**Prolog**
Next was Prolog. I found a nice snippit describing Prolog "Prolog is a declarative programming language, meaning that you describe the logic without specifying the control flow. It uses a format of predicate :- clause. to define rules." I really like how the rules are stand alone logic and are therefore very reusable. 

**C**
While solving the problem in C I discovered the `qsort` function from `stdlib.h` and by extension of this I found void pointers. The qsort function is designed to work on arrays of any data type. This also means I needed to cast the arguments to ints to be able to do arithmitic.

**Haskell**
Through searching how to solve this problem in haskell I discovered `zipWith` which takes a binary function (in this case, multiplication `*`) and two lists. So the idea was to: sort the list, split the list in half, reverse the second half (so you are essentially working with each end), then multiply each list in their new orders.

**General**
The prolog solution was similar to the haskell solution but the haskell solution was syntactically more appealing and easier to understand. This approach of splitting the list in half and reversing it could be done in C or JS but the reversing of the second half of the array isn't something that came to mind when solving in those languages. This would also increase the time complexity, although the code in haskell was arguably the easiest to read. The reversal of the second half of the list is not necessary but more like a convenient way to pair the lowest and highest values.

## Time Complexity

**JS**
- Sorting: O(n log n) where n is the number of elements in the array.
- Slicing: O(k) where k is the number of elements to slice.
- Reducing: O(n/2) since it processes half of the elements.
- Total Complexity: O(n log n) (dominated by the sorting step).

**C**
- Sorting: O(n log n) for sorting. 
- Looping to calculate the sum of products: O(n/2).
- Total Complexity: O(n log n) (dominated by the sorting step).

**Haskell**
- Sorting: O(n log n) using the sort function from Data.List.
- Splitting: O(n) to traverse the list and split it into two halves.
- Reversing the second half: O(n/2) which simplifies to O(n).
- Zipping and summing: O(n/2) to process each pair and calculate the sum.
- Total Complexity: O(n log n) (dominated by the sorting step).

**Prolog**
- Sorting: O(n log n) for built-in sorting predicates.
- Splitting and reversing: O(n) for list operations.
- Calculating the sum of products: O(n) for traversing the lists.
- Total Complexity: O(n log n) (dominated by the sorting step).

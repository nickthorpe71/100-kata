# Product Of Maximums Of Array (Array Series #2)

I decided to do the [next iteration](https://www.codewars.com/kata/5a63948acadebff56f000018) in the array series from the last session.

**Prolog**
Yesterday I felt like I gained dome valuable knowledge of how to work with lists in prolog. Today I decided to start with prolog and test my new knowledge. I ended up defining a `take` predicate to take the first N numbers in the list and return them. With this helper predicate it was fairly easy to cobble together the rest of the solution.

**JS**
Started running low on time so I tackles JS next. Very easy in JS.

**C**
The C implementation was not similar to yesterday except I used a long instead of into to allow for larger results.

**Haskel**
The haskell solution was once again very elegant to read compared to C or Prolog but not much more than JS in my opinion.

**General**
Overall I'm realizing that this project is taking more out of the day than originally intended. Today's session was a little over 2 hours and I felt like I had to rush after getting the prolog solution out. I'm going to consider either dropping some languages or setting up a rotation.

# Benchmarks
5 max with 1,000,000 random numbers
- **Prolog:**   0.4921s
- **JS:**       0.2110s
- **C:** 				0.1128s
- **Haskell:**  2.6008s


# Tanker Fuel Volume

[Today's kata](https://www.codewars.com/kata/55f3da49e83ca1ddae0000ad) was more math focused than data structure and/or algorithm focused. I wanted to try this as an experiment to see how haskell and prolog would differ from c/js. Full transparency, I had to research the underlying math and still don't have a full understanding.

*First day of spanning one kata over multiple days*

**JS** 
Started with JS again to make sure I had a working solution before trying to implement in other languages. I think I need to start with a different language (probably haskell) next time to force myself to understand how to start from a different perspective.

**C**
Moved to C next assuming it would be a pretty easy translation from JS. It was necessary for me to look up the math.h library and how to do floating point calculations. Initially I was doing interger division and the results were inaccurate. I also learned that the -lm flag is necessary when using the math.h library. 

**Haskell**
In haskell I was introduced to the `where` clause which is used for defining variables and functions that are local to a particular scope. With this the code actually looked very similar to the JS solution except with a nice typed input/ouput signature.

**Prolog**
I was very close to dropping prolog today, but I'm glad I didn't. I really enjoy how easy prolog programs are to read now that I'm starting to understand the syntax. That could be a related to the type of problem but it was still eye opening. 


**Benchmarks**
*not overly necessary today but just for practice*
- **JavaScript:** 0.0024s
- **C:** 0.0001s
- **Haskell:** 0.0045s
- **Prolog:** 0.0110s




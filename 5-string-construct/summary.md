# String Construction 

Today's [kata](https://www.codewars.com/kata/58a3a735cebc0630830000c0)

The overall apprach is to calculate the Levenshtein distance for both possible transformations of the string. Whichever has the closest score will be the path that is selected. Notes on Levenshtein distance can be found [here](#levenshtein-distance).

I spent some time working with my approach using the levenshtein distance but ultimately was unsuccessful. I ended up having to look up a solution to this problem as it was taking more time than desired. 

## Levenshtein Distance

The idea of the Levenshtein distance algorithm is to use dynamic programming to compute the minimum number of single character edits (i.e insertions, deletions, or substitutions) required to transform one string into the other. The algorithm uses a matrix where the element in the i-th row and j-th column represents the distance between the first i characters of the first string and the first j characters of the second string.

**JS** 
I initially started the problme in haskell but since it was more difficult than I anticipated I switched over to JS to make sure I could come up with a solution. After spending a while session on coming up with a solution I ended up looking up the answer. 

**Haskell**
After implementing in JS I moved on to haskell. I think I'm finally starting to see how haskell can be easier to read in some situations. It's almost like a recursive solution or more about recognizing a pattern while an iterative solution requires keeping track of more values in your mind. This is just an inital thought though.

**C**
This is the first string based kata I've tackled in this challenge so it gave me a chance to look into the string section of Cs standard lib (string.h). I also found memmove which allows you to move memory from one address to another. This was useful in "deleting" characters from the current string. This kind of pseudo delete was essentially moving the character following the character you're tring to delete to the address of the character you're trying to delete.

**Prolog**
I found the syntax and approach for this problem in prolog to be pretty unpleasant. I noticed that some of the useful things in prolog also exist in erlang/elixir. I might swap out prolog (and potentially haskell) in place of either erlang or elixir.


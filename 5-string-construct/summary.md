# String Construction 

Today's [kata](https://www.codewars.com/kata/58a3a735cebc0630830000c0)

The overall apprach is to calculate the Levenshtein distance for both possible transformations of the string. Whichever has the closest score will be the path that is selected. Notes on Levenshtein distance can be found [here](#levenshtein-distance).


## Levenshtein Distance

The idea of the Levenshtein distance algorithm is to use dynamic programming to compute the minimum number of single character edits (i.e insertions, deletions, or substitutions) required to transform one string into the other. The algorithm uses a matrix where the element in the i-th row and j-th column represents the distance between the first i characters of the first string and the first j characters of the second string.

function stringConstructing(a, s) {
  let i = 0, ops = 0, curr = "";
  
  while(curr != s) {
    while(i < s.length && i < curr.length && curr[i] === s[i]) {
      i++;
    }
    
    if(i >= curr.length) {
      curr += a;
    } else {
      curr = curr.slice(0, i) + curr.slice(i + 1);
    }
    
    ops++;
  }
  
  return ops;
}

/* Explaination 

The Problem
The problem is about constructing string s from string a by either appending the entire string a to the current string or deleting any character from it. The goal is to do this in the least number of operations.

The Solution
This solution employs a double while loop. The outer loop continues until the constructed string (curr) is the same as the target string (s). The inner loop goes character by character to check how much of the current string matches with the target string.

Variables:
i: Keeps track of the index position up to which both strings (curr and s) are identical.
ops: Counter for the operations required.
curr: The string we are constructing to match s.
Approach:
Outer Loop:

This runs as long as curr does not match the target string s.
Inner Loop:

The loop is checking for matching characters between curr and s starting from the last known matching position (i).
As long as the characters at position i in both strings are the same, it continues, incrementing i each time.
After the Inner Loop:

If the value of i is greater than or equal to the length of curr (i.e., the entirety of curr matches with the beginning of s), it means we should append string a to curr.
If not, there's a mismatch at position i of curr, so we remove that character.
Increment Operations:

After each cycle (append or delete), we increment the operations counter (ops).
In Summary:
Start with an empty curr.
Traverse curr and s, matching characters.
If there's a mismatch or if we reach the end of curr, either append a or delete a mismatched character from curr.
Repeat until curr equals s.

*/

function main() {
  console.time('Execution Time');
  console.log(stringConstructing("a", "a"));
  console.log(stringConstructing("aba", "abbabba"));
  console.log(stringConstructing("a", "aaa"));
  console.log(stringConstructing("bbaabcbcbc", "bbcccbabcc"));
  console.timeEnd('Execution Time');
}

main();


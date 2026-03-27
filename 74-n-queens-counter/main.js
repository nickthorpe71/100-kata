/*
  HackerRank-style starter:
  Complete the `countQueenArrangements` function below.

  Return the number of valid ways to place n queens on an n x n board.
  Solve it using recursion and backtracking.
*/

function countQueenArrangements(n) {
  throw new Error("Not implemented");
}

function runTests() {
  const tests = [
    { input: 1, expected: 1 },
    { input: 4, expected: 2 },
    { input: 5, expected: 10 },
    { input: 6, expected: 4 },
  ];

  for (const { input, expected } of tests) {
    const actual = countQueenArrangements(input);
    console.log({
      input,
      expected,
      actual,
      passed: actual === expected,
    });
  }
}

runTests();

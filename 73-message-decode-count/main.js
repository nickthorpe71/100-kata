/*
  HackerRank-style starter:
  Complete the `countDecodings` function below.

  Return the number of valid ways to decode the digit string.
  Solve it using recursion. Memoization is recommended.
*/

function countDecodings(s) {
  const memo = new Map();

  function decode(index) {
    if (index === s.length) {
      return 1;
    }

    if (s[index] === "0") {
      return 0;
    }

    if (memo.has(index)) {
      return memo.get(index);
    }

    let ways = decode(index + 1);

    if (index + 1 < s.length) {
      const twoDigitValue = Number(s.slice(index, index + 2));
      if (twoDigitValue >= 10 && twoDigitValue <= 26) {
        ways += decode(index + 2);
      }
    }

    memo.set(index, ways);
    return ways;
  }

  return decode(0);
}

function runTests() {
  const tests = [
    { input: "12", expected: 2 },
    { input: "226", expected: 3 },
    { input: "06", expected: 0 },
    { input: "11106", expected: 2 },
  ];

  for (const { input, expected } of tests) {
    const actual = countDecodings(input);
    console.log({
      input,
      expected,
      actual,
      passed: actual === expected,
    });
  }
}

runTests();

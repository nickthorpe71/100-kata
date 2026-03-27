/*
  HackerRank-style starter:
  Complete the `nestedSum` function below.

  Given a nested array of integers, return the sum of every integer
  in the structure using recursion.
*/

function nestedSumR(items) {
    let sum = 0;
    for (const item of items) {
        if (typeof item === "number") {
            sum += item;
        }
        if (Array.isArray(item)) {
            sum += nestedSumR(item);
        }
    }
    return sum;
}

function nestedSum(items) {
    const stack = [...items];
    let sum = 0;
    while (stack.length > 0) {
        const next = stack.pop();
        if (typeof next === "number") {
            sum += next;
        } else {
            stack.push(...next);
        }
    }
    return sum;
}
















// ------------------------------------------

function runTests() {
    const tests = [
        { input: [1, [2, 3], 4], expected: 10 },
        { input: [[1, [2]], [], 3, [4, [5, [6]]]], expected: 21 },
        { input: [], expected: 0 },
        { input: [[[-5]], 10, [-2, [3]]], expected: 6 },
    ];

    for (const { input, expected } of tests) {
        const actual = nestedSum(input);
        console.log({
            input: JSON.stringify(input),
            expected,
            actual,
            passed: actual === expected,
        });
    }
}

runTests();

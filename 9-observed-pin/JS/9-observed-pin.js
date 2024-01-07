const possibilitiesMap = {
  1: ["1", "2", "4"],
  2: ["1", "2", "3", "5"],
  3: ["2", "3", "6"],
  4: ["1", "4", "5", "7"],
  5: ["2", "4", "5", "6", "8"],
  6: ["3", "5", "6", "9"],
  7: ["4", "7", "8"],
  8: ["5", "7", "8", "9", "0"],
  9: ["6", "8", "9"],
  0: ["8", "0"],
};

/**
 * @param {string} observed
 * @returns {string[]}
 */
function getPINs(observed) {
  if (!observed) return [];
  if (observed.length === 1) return possibilitiesMap[observed];

  // Convert the observed string into an array of possible digit arrays
  const possibilitiesMatrix = observed
    .split("")
    .map((c) => possibilitiesMap[c]);

  // Recursive function to generate all combinations
  function generateCombinations(matrix, index, current, results) {
    if (index === matrix.length) {
      results.add(current.join(""));
      return;
    }

    for (const digit of matrix[index]) {
      current.push(digit);
      generateCombinations(matrix, index + 1, current, results);
      current.pop(); // Backtrack
    }
  }

  const results = new Set();
  generateCombinations(possibilitiesMatrix, 0, [], results);

  return Array.from(results);
}

function main() {
  console.time("Execution Time");
  console.log("Result:", getPINs("8"));
  console.log("Result:", getPINs("11"));
  console.log("Result:", getPINs("3423"));
  console.timeEnd("Execution Time");
}

main();

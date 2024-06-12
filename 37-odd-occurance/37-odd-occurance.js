/**
 * @param {Number[]} A
 * @returns {Number}
 */
function findOdd(A) {
  const countMap = {};

  for (const n of A) {
    countMap[n] = (countMap[n] || 0) + 1;
  }

  for (const key in countMap) {
    if (countMap[key] % 2 === 1) {
      return Number(key);
    }
  }
  return 0;
}

console.log(findOdd([1, 1, 1, 1, 2, 2, 3, 3, 4, 5, 5, 5, 5])); // 4
console.log(findOdd([1, 1, 1, 2, 2, 3, 3, 4, 5, 5, 5, 5])); // 1
console.log(findOdd([1, 1, 1, 1, 2, 2, 3, 3, 4, 5, 5, 5])); // 4
console.log(findOdd([1, 1, 1, 1, 2, 2, 3, 3, 5, 5, 5])); // 5
console.log(findOdd([])); // 0

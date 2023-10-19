function maxProduct(numbers, size) {
  return numbers.sort((a,b) => b - a)
                .slice(0, size)
                .reduce((acc, curr) => acc * curr, 1);
}

function generateRandomNumbers(n, min, max) {
    return Array.from({length: n}, () => Math.floor(Math.random() * (max - min + 1) + min));
}

function main() {
  console.log(maxProduct([4,3,5], 2));
  console.log(maxProduct([10,8,7,9], 3));
  const randomNumbers = generateRandomNumbers(1000000, 0, 1000);
  console.time("Execution Time");
  console.log(maxProduct(randomNumbers, 5));
  console.timeEnd("Execution Time");
}

main();

function minSum(arr) {
  const sorted = arr.sort((a,b) => a - b);
  const halfLength = sorted.length / 2;
  return sorted.slice(0, halfLength).reduce((res, _, i) => 
    res + sorted[i] * sorted[sorted.length - 1 - i], 0);
}

function generateRandomNumbers(n, min, max) {
    return Array.from({length: n}, () => Math.floor(Math.random() * (max - min + 1) + min));
} 

function main() {
  const randomNumbers = generateRandomNumbers(1000000, 0, 1000);
  console.time("Execution Time");
  console.log(minSum(randomNumbers, "-> 9107682643308652960"));
  console.timeEnd("Execution Time");
}

main();


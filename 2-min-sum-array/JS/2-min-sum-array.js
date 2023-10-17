function minSum(arr) {
  const sorted = arr.sort((a,b) => a - b);
  const halfLength = sorted.length / 2;
  return sorted.slice(0, halfLength).reduce((res, _, i) => 
    res + sorted[i] * sorted[sorted.length - 1 - i], 0);
}


function main() {
  console.log(minSum([5,4,2,3], "-> 22"));
  console.log(minSum([12,6,10,26,3,24], "-> 342"));
}

main();


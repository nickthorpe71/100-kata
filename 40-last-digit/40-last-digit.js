function lastPostPow(base, exp, modulus) {
  const baseMod = base % modulus;
  const expMod = (exp + 3) % modulus;
  return Math.round(baseMod * baseMod ** expMod) % modulus;
}

function lastDigit(numbers) {
  if (numbers.length === 0) return 1;

  let prevBase = null;
  let exp = 1;
  const cyclicModLen = 4;

  for (let i = numbers.length - 1; i > 0; --i) {
    const base = numbers[i];

    if (prevBase === 0) {
      exp = 1;
      prevBase = null;
    } else {
      if (prevBase > 2 && base % cyclicModLen === 2) {
        exp = 0;
      } else {
        exp = lastPostPow(base, exp, cyclicModLen);
      }
      prevBase = base;
    }
  }

  const firstNumber = numbers[0];
  // mod 10 at the end because we only care about the last digit of the result
  return prevBase === 0 ? 1 : lastPostPow(firstNumber, exp, 10);
}

// top solution
// function lastDigit (as){
//   return as.reduceRight((prev, curr, i) => {
//     let exp = prev < 4 ?
//       prev :
//       (prev % 4 + 4);

//     let base = i === 0 ?
//       (curr % 10) :
//       (curr < 4 ? curr : (curr % 4 + 4));

//     return Math.pow(base, exp);
//   }, 1) % 10;
// }

// Test cases
console.log(lastDigit([]), `should be: ${1} - 1`);
console.log(lastDigit([0, 0]), `should be: ${1} - 2`);
console.log(lastDigit([0, 0, 0]), `should be: ${0} - 3`);
console.log(lastDigit([1, 2]), `should be: ${1} - 4`);
console.log(lastDigit([3, 4, 5]), `should be: ${1} - 5`);
console.log(lastDigit([4, 3, 6]), `should be: ${4} - 6`);
console.log(lastDigit([7, 6, 21]), `should be: ${1} - 7`);
console.log(lastDigit([12, 30, 21]), `should be: ${6} - 8`);
console.log(lastDigit([2, 2, 2, 0]), `should be: ${4} - 9`);
console.log(lastDigit([937640, 767456, 981242]), `should be: ${0} - 10`);
console.log(lastDigit([123232, 694022, 140249]), `should be: ${6} - 11`);
console.log(lastDigit([499942, 898102, 846073]), `should be: ${6} - 12`);

const r1 = Math.floor(Math.random() * 100);
console.log(lastDigit([r1]), `should be: ${r1 % 10} - 13`);

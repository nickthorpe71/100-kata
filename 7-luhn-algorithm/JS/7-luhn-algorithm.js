
function validate(n) {
  let sum = 0;

  while (n > 0) {
    let a = n % 10;
    n = Math.floor(n / 10);

    let b = (n % 10) * 2;
    n = Math.floor(n / 10);

    if (b > 9) b -= 9;
    sum += a + b;
  }

  return sum % 10 === 0;
}

function main() {
  console.time('Execution Time');
  console.log(validate(1354143611634));
  console.timeEnd('Execution Time');
}

main();


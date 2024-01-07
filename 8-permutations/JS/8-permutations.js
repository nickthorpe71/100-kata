function permutations(str) {
  if (str.length < 2) return [str];

  let perms = [];
  for (let i = 0; i < str.length; i++) {
    const char = str[i];

    if (str.indexOf(char) != i) continue;

    const remainingChars = str.slice(0, i) + str.slice(i + 1, str.length);

    for (const permutation of permutations(remainingChars)) {
      perms.push(char + permutation);
    }
  }
  
  return perms;
}


function main() {
  console.time('Execution Time');
  
  console.log(permutations("a"));
  console.log(permutations("aabb"));
  console.log(permutations("ab"));

  console.timeEnd('Execution Time');
}

main();


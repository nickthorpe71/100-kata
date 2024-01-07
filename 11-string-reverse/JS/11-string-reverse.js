function spinWords(words){
  return words.split(' ').map(function (word) {
    return (word.length > 4) ? word.split('').reverse().join('') : word;
  }).join(' ');
}

function main() {
  console.time('Execution Time');
  console.log(spinWords("Test Testing lamamama 1 1 1 asd  dfs   "));
  console.timeEnd('Execution Time');
}

main();


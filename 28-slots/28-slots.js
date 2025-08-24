function getPatternValue(pattern) {
  switch (pattern.length) {
    case 1:
      return 1000;
      break;
    case 2:
      if (pattern.includes(4)) {
        return 800;
      } else {
        return 500;
      }
      break;
    case 3:
      if (pattern.includes(3)) {
        return 300;
      } else {
        return 200;
      }
      break;
    case 4:
      return 100;
      break;
    default:
      return 0;
  }
}

function slot(s){
  let pattern = [];
  let currentCount = 1;
  let currentSymbol = s[0]
  for (let i = 1; i < s.length; i++) {
    if (s[i] === currentSymbol) {
      currentCount++;
    } else {
      pattern.push(currentCount);
      currentCount = 1;
      currentSymbol = s[i];
    }
  }

  pattern.push(currentCount);

  return getPatternValue(pattern);
}

console.log(slot("!!!!!"));
console.log(slot("??!!!"));
console.log(slot("!!!?!"));
console.log(slot("!!?!!"));
console.log(slot("!!!!?"));
console.log(slot("!!?!?"));
console.log(slot("!?!?!"));


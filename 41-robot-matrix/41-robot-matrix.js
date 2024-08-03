function robotTransfer(m, k) {
  let successful = 0;
  for (let i = 0; i < m[0].length; i++) {
    for (let j = 0; j < m.length; j++) {
      let moves = 1;
      const start = `${i},${j}`;
      let current = parseV2(m[i][j]);
      while (moves <= k) {
        const next = m[current.x][current.y];
        current = parseV2(next);
        moves++;
        if (next === start) {
          if (moves === k) {
            successful++;
          }
          break;
        }
      }
    }
  }

  return successful;
}

function parseV2(str) {
  return { x: str[0], y: str[2] };
}

function doTest(m, k, expected) {
  const res = robotTransfer(m, k);
  console.log({
    res,
    expected,
    passed: res === expected,
  });
}

doTest(
  [
    ["0,1", "0,0", "1,2"],
    ["1,1", "1,0", "0,2"],
    ["2,1", "2,0", "0,0"],
  ],
  2,
  8
);

doTest(
  [
    ["0,1", "0,0"],
    ["1,1", "1,0"],
  ],
  2,
  4
);

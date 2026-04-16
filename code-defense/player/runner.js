const { walkThread } = require('./solution');
const testOnly = process.argv.includes('--test');
let allCorrect = true;
let ops = 0;
const start = process.hrtime.bigint();

{
    let t = [3, 1, 4, 1, 5, 1];
    if (walkThread(t, 1) !== 3) allCorrect = false;
}
{
    let t = [7, 7, 7];
    if (walkThread(t, 7) !== 3) allCorrect = false;
}
{
    let t = [1, 2, 3];
    if (walkThread(t, 5) !== 0) allCorrect = false;
}
{
    let t = [42];
    if (walkThread(t, 42) !== 1) allCorrect = false;
}
if (!testOnly) {
    let n = 100000;
    let t = new Array(n);
    for (let i = 0; i < n; i++) t[i] = i % 10;
    if (walkThread(t, 0) !== Math.floor(n / 10)) allCorrect = false;
    ops = n;
} else { ops = 13; }

const end = process.hrtime.bigint();
const ms = Number((end - start) / 1000000n);
process.stdout.write(`${ms} ${ops}\n`);
process.exit(allCorrect ? 0 : 1);

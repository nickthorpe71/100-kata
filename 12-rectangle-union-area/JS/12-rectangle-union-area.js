/**
 *
 * @param {number[][]} recs
 * @returns {number}
 */
function calculate(recs) {
    let steps = new Set();
    recs.forEach((e) => {
        steps.add(e[0]) && steps.add(e[2]);
    });

    recs = recs.sort((a, b) => a[0] - b[0]);

    let square = (offset = 0);
    let activeRecs = [];
    [...steps]
        .sort((a, b) => a - b)
        .reduce((prevStep, step) => {
            let count = 0;
            activeRecs
                .sort((a, b) => a[1] - b[1])
                .reduce((prev, cur) => {
                    if (prev < cur[3]) {
                        count += cur[3] - Math.max(cur[1], prev);
                        return cur[3];
                    }
                    return prev;
                }, 0);
            square += count * (step - prevStep);

            activeRecs = activeRecs.filter((r) => r[2] > step);

            for (
                let i = offset;
                i in recs && recs[i][0] === step;
                offset = ++i
            ) {
                activeRecs.push(recs[i]);
            }
            return step;
        }, 0);
    return square;
}

function main() {
    const rectangles = [
        [3, 3, 8, 5], // 10
        [6, 3, 8, 9], // 12
        // [12, 7, 13, 11], // 4
        [11, 6, 14, 12], // // 18
    ]; // overlap 4

    console.time("Execution Time");

    console.log(calculate(rectangles)); // Output: 36

    console.timeEnd("Execution Time");
}

main();

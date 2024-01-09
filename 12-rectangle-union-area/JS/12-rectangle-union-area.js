/**
 *
 * @param {number[][]} recs
 * @returns {number}
 */
function calculate(recs) {
    if (recs.length === 0) return 0;

    // [x] get area of all rectangles
    // [x] find overlapping rectangles
    // [ ] find points of overlaps
    // [x] find area of overlaps
    // [x] subtract overlaps area from total area

    const totalArea = recs.reduce((total, rec) => {
        return total + calculateArea(rec);
    }, 0);
    const overlaps = findOverlaps(recs);
    const overlappingPoints = calculateOverlappingPoints(overlaps);
    const overlappingArea = overlappingPoints.map((rec) => calculateArea(rec));

    // return totalArea - overlappingArea

    return { totalArea, overlaps };
}

function calculateArea(rec) {
    // each rectangle is represented as: [x0, y0, x1, y1]
    // (x0, y0) - coordinates of the bottom left corner
    // (x1, y1) - coordinates of the top right corner
    // xi, yi - positive integers or zeroes (0, 1, 2, 3, 4..)
    // sides of rectangles are parallel to coordinate axes
    return Math.abs(rec[2] - rec[0]) * Math.abs(rec[3] - rec[1]);
}

function findOverlaps(recs) {
    let overlaps = new Set();

    for (let i = 0; i < recs.length - 1; i++) {
        const rec1 = recs[i];
        const rec2 = recs[i + 1];

        // Rectangles overlap if one rectangle's bottom left corner is to the left
        // and below the top right corner of the other rectangle and vice versa.

        const overlapping =
            rec1[0] <= rec2[2] &&
            rec1[1] <= rec2[3] &&
            rec2[0] <= rec1[2] &&
            rec2[1] <= rec1[3];

        if (overlapping) {
            overlaps.add(rec1);
            overlaps.add(rec2);
        }
    }

    return overlaps;
}

function calculateOverlappingPoints(overlaps) {
    return [];
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

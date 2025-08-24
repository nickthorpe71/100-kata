/**
 * @param {Array<number>} arr The input integer array to be searched.
 * @param {number} target The target integer to search within the array.
 * @return {number} The index of target element in the array, or -1 if not found.
 */
function binarySearch(arr, target) {
    let start = 0;
    let end = arr.length - 1;

    while (start <= end) {
        const mid = Math.floor((start + end) / 2);
        const inspected = arr[mid];

        if (inspected === target) {
            return mid;
        } else if (inspected > target) {
            end = mid - 1;
        } else {
            start = mid + 1;
        }
    }

    return -1;
}

async function main() {
    const testGroup = [
        test([1, 2, 3, 6, 9, 11], 6, 3),
        test([1, 3, 6, 9, 11], 6, 2),
        test([], 6, -1),
        test([1, 2, 3, 9, 11], 6, -1),
        test([1, 2, 3, 10, 11, 20], 2, 1),
        test([1, 2, 3, 10, 11, 20], 1, 0),
        test([1, 2, 3, 10, 11, 20], 20, 5)
    ];
    const numCorrect = (await Promise.all(testGroup)).reduce(
        (acc, curr) => acc + curr
    , 0);
    console.log(`Total correct: ${numCorrect}/${testGroup.length}`)
}

async function test(arr, target, expected) {
    const res = binarySearch(arr, target);
    const correct = res === expected;
    console.log(`Expected: ${expected} got ${res}`);
    return correct;
}

main();
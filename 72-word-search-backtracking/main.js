/*
  HackerRank-style starter:
  Complete the `exists` function below.

  Return true if `word` can be formed in `board` by moving
  up, down, left, or right without reusing a cell.
*/

function exists(board, word) {
    for (let i = 0; i < board.length; i++) {
        for (let j = 0; j < board[0].length; j++) {
            const visitedMatrix = Array.from(
                { length: board.length },
                () => Array.from(
                    { length: board[0].length }
                , () => 0)
            );
            const result = traverse(i, j, 0, visitedMatrix, board, word);
            if (result === true) {
                return true;
            }
        }
    }
    return false;
}


function traverse(row, col, wordIndex, visitedMatrix, board, word) {
    if (
        isOutOfBounds(board, row, col) 
        || visitedMatrix[row][col] === 1 
        || board[row][col] !== word[wordIndex]
    ) {
        return false;
    }

    if (wordIndex === word.length - 1) {
        return true;
    }

    visitedMatrix[row][col] = 1;

    for (const dir of [[0,1], [1,0], [0,-1], [-1,0]]) {
        const [rowAdj, colAdj] = dir;
        if (traverse(
            row + rowAdj,
            col + colAdj,
            wordIndex + 1,
            visitedMatrix,
            board,
            word
        )) {
            return true;
        }
    }
    visitedMatrix[row][col] = 0;
    return false;
}

function isOutOfBounds(board, row, col) {
    return board.length === 0
        || row < 0
        || col < 0
        || row >= board.length
        || col >= board[0].length;
}








// ------------------------------------------

function runTests() {
    const board = [
        ["A", "B", "C", "E"],
        ["S", "F", "C", "S"],
        ["A", "D", "E", "E"],
    ];

    const tests = [
        { board, word: "ABCCED", expected: true },
        { board, word: "SEE", expected: true },
        { board, word: "ABCB", expected: false },
        { board: [["A"]], word: "A", expected: true },
    ];

    for (const { board, word, expected } of tests) {
        const actual = exists(board, word);
        console.log({
            word,
            expected,
            actual,
            passed: actual === expected,
        });
    }
}

runTests();

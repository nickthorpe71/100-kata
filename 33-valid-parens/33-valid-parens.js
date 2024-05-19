/**
 * Removes the minimum number of "(" or ")" to make the given string
 * have only matching sets of parentheses.
 * @param {string} str 
 * @returns {string}
 * @complexity Time: O(n) | Space: O(n)
 */
function makeValid(str) {
    const open = [];
    const close = [];

    for (let i = 0; i < str.length; i++) {
        const c = str[i];
        if (c === "(") {
            open.push(i);
        } else if (c === ")" && open.length > 0) {
            open.pop();
        } else if (c === ")") {
            close.push(i);
        }
    }

    const indicesToRemove = new Set([...open, ...close]);
    const res = [];

    for (let i = 0; i < str.length; i++) {
        if (!indicesToRemove.has(i)) {
            res.push(str[i]);
        }
    }

    return res.join("");
}

function main() {
    console.log(makeValid("foobarbaz"));
    console.log(makeValid("(f(o(o(b)ar)ba)z)"));
    console.log(makeValid("f(oo(barbaz"));
    console.log(makeValid("(f(o(o()()()()()))(b)ar)ba)z)"));
    console.log(makeValid("(((((f(o(o(b)ar)ba)z)"));
    console.log(makeValid("(f(o(o(b)ar)ba)z)))))"));
}

main();
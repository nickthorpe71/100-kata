/**
 * Removes the minimum number of "(" or ")" to make the given string
 * have only matching sets of parentheses.
 * @param {string} str 
 * @returns {string}
 * @complexity Time: O(n) | Space: O(n)
 */
function makeValid(str) {
    const stack = [];
    const indicesToRemove = new Set();

    for (let i = 0; i < str.length; i++) {
        const c = str[i];
        if (c === "(") {
            stack.push(i);
        } else if (c === ")") {
            if (stack.length > 0) {
                stack.pop();
            } else {
                indicesToRemove.add(i);
            }
        }
    }

    while (stack.length > 0) {
        indicesToRemove.add(stack.pop());
    }

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
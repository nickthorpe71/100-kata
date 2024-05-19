/**
 * Removes the minimum number of "(" or ")" to make the given string
 * have only matching sets of parentheses.
 * @param {string} str 
 * @returns {string}
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

    const indicesToRemove = [...open, ...close];
    const res = new Array(str.length - indicesToRemove.length);
    let j = 0; // remove group index
    let ri = 0; // result index
    for (let i = 0; i < str.length; i++) {
        if (i !== indicesToRemove[j]) {
            res[ri] = str[i];
            ri++;
        } else {
            j++;
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
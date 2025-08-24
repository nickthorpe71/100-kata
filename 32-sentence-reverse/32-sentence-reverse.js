/**
 * @param {string} sentence
 * @returns {string} reversed sentence
 */
function reverseSentence(sentence) {
    return sentence.split(" ").reverse().join(" ");
}

/**
 * @param {string} sentence
 * @returns {string} reversed sentence
 */
function reverseSentenceFast(sentence) {
    const res = new Array(sentence.length);
    let currIdx = 0;
    let r = sentence.length - 1;
    for (let i = sentence.length - 1; i >= 0; i--) {
        const c = sentence[i];
        if (c === " " || i === 0) {
            const start = i === 0 ? i : i + 1;
            for (let j = start; j <= r; j++) {
                res[currIdx] = sentence[j];
                currIdx++;
            }
            if (i !== 0) {
                res[currIdx] = " ";
                currIdx++;
            }
            r = i - 1;
        }
    }
    return res.join("");
}

(() => {
    const s = "The sky is dark blue";
    console.log(reverseSentenceFast(s));
})();
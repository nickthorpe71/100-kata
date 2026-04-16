function alienOrder(words) {
    const graph = new Map();
    const indegree = new Map();

    for (const word of words) {
        for (const char of word) {
            if (!graph.has(char)) graph.set(char, new Set());
            if (!indegree.has(char)) indegree.set(char, 0);
        }
    }

    for (let i = 0; i < words.length - 1; i++) {
        const word1 = words[i];
        const word2 = words[i + 1];

        if (word1.startsWith(word2) && word1.length > word2.length) {
            return "";
        }

        const len = Math.min(word1.length, word2.length);

        for (let j = 0; j < len; j++) {
            const from = word1[j];
            const to = word2[j];
            if (from !== to && !graph.get(from).has(to)) {
                graph.get(from).add(to);
                indegree.set(to, indegree.get(to) + 1);
                break;
            }
        }
    }

    const queue = [];
    for (const [char, count] of indegree) {
        if (count === 0) {
            queue.push(char);
        }
    }

    const result = [];
    while (queue.length > 0) {
        const char = queue.shift();
        result.push(char);
        for (const neighbor of graph.get(char)) {
            const newIndegree = indegree.get(neighbor) - 1;
            indegree.set(neighbor, newIndegree);
            if (newIndegree === 0) {
                queue.push(neighbor);
            }
        }
    }

    return (result.length !== indegree.size) ? "" : result.join("");
}

function main() {
    console.log("Implement the alien dictionary function for kata 76 and add tests.");
}

if (require.main === module) {
    main();
}

module.exports = {
    alienOrder,
};

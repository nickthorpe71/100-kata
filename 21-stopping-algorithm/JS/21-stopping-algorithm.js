// Simple (Selling A House)
// you are selling a house
// you want to get the best price possible
// you want to stop when you have a good price
function bestAfterXPercent(prices, x) {
    const numInFirstX = Math.round(prices.length * x);

    const bestInFirstX = prices
        .slice(0, numInFirstX)
        .reduce(
            (highest, current) => (current > highest ? current : highest),
            -1
        );

    for (let i = numInFirstX; i < prices.length; i++) {
        if (prices[i] > bestInFirstX) return prices[i];
        if (i === prices.length - 1) return prices[i];
    }
}

// Test
// find the best price in the list and the worst price in the list
// determine where the selected price falls in the list
// need to run the test 100 times to get a good average
// if the selecting algorithm averages in the top 10% of the list the the test passes
function testPerformance(prices, selected) {
    const bestPrice = Math.max(...prices);

    const sortedPrices = [...prices].sort((a, b) => a - b);

    const selectedIndex = sortedPrices.indexOf(selected);

    const selectedPercent = selectedIndex / prices.length;

    return selected === bestPrice;
}

function main() {
    console.time("Execution Time");

    const numRuns = 1000;

    let total = 0;

    for (let i = 0; i < numRuns; i++) {
        const randomPrices = Array.from({ length: 1000 }, () =>
            Math.floor(Math.random() * 1000000)
        );
        const selected = bestAfterXPercent(randomPrices, 0.37);

        const performance = testPerformance(randomPrices, selected);

        total += performance;
    }

    console.log(`Average: ${total / numRuns}`);

    console.timeEnd("Execution Time");
}

main();

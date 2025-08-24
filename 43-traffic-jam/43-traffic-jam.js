const trafficJam = (mainRoad, sideStreets) => {
    for (let i = sideStreets.length - 1; i >= 0; i--) {
        const carsLength = sideStreets[i].length - 1;

        for (let j = 0; j <= carsLength; j++) {
            mainRoad =
                mainRoad.slice(0, i + j * 2 + 1) +
                sideStreets[i][carsLength - j] +
                mainRoad.slice(i + j * 2 + 1);
        }
    }

    return mainRoad.slice(0, mainRoad.indexOf("X") + 1);
};

console.log(
    trafficJam("abcdeXghi", [
        "",
        "",
        "CCCCC",
        "",
        "EEEEEEEEEE",
        "FFFFFF",
        "",
        "",
        "IIIIII",
    ])
); // "abcCdCeCECX"

console.log(trafficJam("abcdefX", [])); // "abcdefX"
console.log(trafficJam("abcXdef", [])); // "abcX"

console.log(trafficJam("Xabcde", [])); // X
console.log(
    trafficJam("abcdefghijklmX", [
        "",
        "",
        "",
        "BBBBBB",
        "",
        "",
        "",
        "",
        "CCCCC",
    ])
); // "abcdBeBfBgBhBiBCjCkClCmCX"

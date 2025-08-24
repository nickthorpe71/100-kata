function decrypt(encryption) {
    let s = Array(26).fill(0);

    for (let i = 0; i < encryption.length; i++) {
        const asciiCode = encryption.charCodeAt(i);
        if (asciiCode >= 97 && asciiCode <= 122) {
            s[asciiCode - 97] += 1;
        }
    }

    return s.join("");
}

console.log(decrypt("$aaaa#bbb*ccfff!z"));

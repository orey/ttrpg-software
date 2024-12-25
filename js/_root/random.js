//======= UTIL FUNCTIONS
function getRandomInt(min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
}

function getRandom(nb) {
    return Math.floor(Math.random() * nb);
}

function getRandomCeiling(nb) {
    return Math.ceil(Math.random() * nb);
}

function testGetRandom(nb, f=getRandom, startswith=0) {
    let tab = new Array(nb).fill(0);
    const THRESHOLD = 100000;
    for (let i=0;i<THRESHOLD;i++)
        tab[f(nb)-startswith]++;
    for (let j=0;j<tab.length;j++)
        process.stdout.write(String(j + startswith).padStart(2, '0') + "|");
    process.stdout.write('\n');
    for (let e of tab)
        process.stdout.write(String(Math.round(e/THRESHOLD*100)).padStart(2, '0') + "|");
    process.stdout.write('\n');
}

function testAllRandom() {
    console.log("Test avec 30");
    testGetRandom(30);
    console.log("Test avec 22");
    testGetRandom(22);
    console.log("Test avec 30 ceiling");
    testGetRandom(30, getRandomCeiling);
    console.log("Test avec 22 ceiling");
    testGetRandom(22, getRandomCeiling);
    console.log("Test dice 4");
    testGetRandom(4, dice, 1);
    console.log("Test dice 6");
    testGetRandom(6, dice, 1);
    console.log("Test dice 8");
    testGetRandom(8, dice, 1);
    console.log("Test dice 10");
    testGetRandom(10, dice, 1);
    console.log("Test dice 12");
    testGetRandom(12, dice, 1);
    console.log("Test dice 20");
    testGetRandom(20, dice, 1);
}




//------------------------------------------------------EXPORTS

if (typeof module !== "undefined" && module.exports) {
    module.exports = {
        getRandomInt,
        getRandom,
        getRandomCeiling,
        testGetRandom,
        testAllRandom
    }
}


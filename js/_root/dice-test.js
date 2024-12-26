/**********************************************
 * Test for dice module
 * Author: rey.olivier@gmail.com
 * Licence: GNU GPL v3
 * Date: October 2020, revised December 2024
 ***********************************************/
"use strict";

const utils = require('./utils');
const dice = require('./dice');
const MAX = 1000000;


//================================================================= Test random FUNCTIONS

function testGetRandom(nb, f=dice.getRandom, startswith=0) {
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
    console.log("------------------------------------testAllRandom");
    console.log("Test avec 30");
    testGetRandom(30);
    console.log("Test avec 22");
    testGetRandom(22);
    console.log("Test avec 30 ceiling");
    testGetRandom(30, dice.getRandomCeiling);
    console.log("Test avec 22 ceiling");
    testGetRandom(22, dice.getRandomCeiling);
    console.log("Test dice 4");
    testGetRandom(4, dice.dice, 1);
    console.log("Test dice 6");
    testGetRandom(6, dice.dice, 1);
    console.log("Test dice 8");
    testGetRandom(8, dice.dice, 1);
    console.log("Test dice 10");
    testGetRandom(10, dice.dice, 1);
    console.log("Test dice 12");
    testGetRandom(12, dice.dice, 1);
    console.log("Test dice 20");
    testGetRandom(20, dice.dice, 1);
}

testAllRandom();

//------------------------------------------------testRollDie
function testRollDie(faces){
    console.log("Testing D" + faces.toString());
    var sum = 0;
    for (var i=0;i<MAX;i++){
        sum += dice.rollDie(faces);
    }
    return sum/MAX;
}

function testAll(){
    console.log("------------------------------------testRollDie");
    console.log("D4:" + testRollDie(4).toString());
    console.log("D6:" + testRollDie(6).toString());
    console.log("D8:" + testRollDie(8).toString());
    console.log("D10:" + testRollDie(10).toString());
    console.log("D12:" + testRollDie(12).toString());
    console.log("D20:" + testRollDie(20).toString());
    console.log("D100:" + testRollDie(100).toString());

}

testAll();

function testAll2(){
    console.log("------------------------------------testRollDie2");
    let testscope = [4,6,8,10,12,20,30,100];
    console.log(utils.fapply(testRollDie,testscope));
}

testAll2();

//------------------------------------------------parseCombi

function testParseCombi(){
    console.log("------------------------------------testParseCombi");
    console.log("Test with void chain");
    console.log(dice.parseCombi(""));
    console.log("Test with 2");
    console.log(dice.parseCombi(2));
    let tests = ["22+1", "22D", "d20", "D20+4", "d20-2", "3d10+44", "2d8-1", "6d6"];
    console.log(utils.fapply(dice.parseCombi, tests));
}

testParseCombi();

//------------------------------------------------rollCombi
function testRollCombi(){
    console.log("------------------------------------testRollCombi");
    let tests = ["3d6","12D10-5","3d4+2","D20","1d30+6","4D8-3","d8+1","1D4","d6-2"];
    console.log(utils.fapply(dice.parseCombi, tests));
    console.log(utils.fapply(dice.rollCombi, tests));
}

testRollCombi();

//-----------------------------------------------rollFourKeepThree
function testRollFourKeepThree(){
    console.log("---------------------------rollFourKeepThree");
    console.log(dice.rollFourKeepThree(true));
    console.log(dice.rollFourKeepThree(true));
    console.log(dice.rollFourKeepThree(true));
    console.log(dice.rollFourKeepThree(true));
}

testRollFourKeepThree();



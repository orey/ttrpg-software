'use strict';

const VERBOSE= true;

const myconsole = {
    log: function(s){
        if (VERBOSE)
            console.log(s);
    }
}


function notANumber(s,defo="0"){
    myconsole.log('"' + s + '" is not a number, replacing by: ' + defo);
}

function interrupt(s){
    myconsole.log(String(s));
    let r = prompt("Do you wish to continue? ")
    if (r.startsWith("n") || r.startsWith("N"))
        process.exit();
}


/*=============================================================
  Assert
  =============================================================*/
class MyTest {
    constructor(name) {
        this.name = name;
        this.ok = 0;
        this.notok = 0;
    }
    assert(formula, test) {
        if (String(formula) === String(test)) {
            myconsole.log("OK");
            myconsole.log(formula);
            myconsole.log(test);
            this.ok += 1;
        }
        else {
            myconsole.log("NOT OK");
            myconsole.log(formula);
            myconsole.log(test);
            this.notok += 1;
        }
    }
    report() {
        myconsole.log("<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>");
        myconsole.log(">>> Results for the test: " + this.name);
        myconsole.log("> Passed: " + String(this.ok));
        myconsole.log("> Not passed: " + String(this.notok));
        myconsole.log("<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>");
    }
}


/*=============================================================
  Dice section
  =============================================================*/

// Random function
function rollDie(faces){
    return Math.floor((Math.random()*faces)+1);
}

// Dice roller
function roll(number, faces, pips){
    let result = pips;
    let temp = 0;
    for (let i=0;i<number;i++){
        temp = rollDie(faces);
        result += temp;
    }
    return result;
}


// Expected input is "2d6+3" or "4d10"
function parseDice(s){
    let nb = 0, faces = 0, pips = 0;
    let dice = "";
    // Is there a pip?
    if ((s.includes("+")) || (s.includes("-")) ) {
        let temp;
        if (s.includes("+")) {
            temp = s.split("+");
            pips = parseInt(temp[1]);
        }
        if (s.includes("-")){
            temp = s.split("-");
            pips = parseInt('-' + temp[1]);
        }
        if (isNaN(pips)){
            notANumber(temp[1],"0");
            pips = 0;
        }
        dice = temp[0];
    }
    else 
        dice = s;
    if ((dice.includes("d")) || (dice.includes("D"))) {
        let temp;
        if (dice.includes("d"))
            temp = dice.split("d");
        if (dice.includes("D"))
            temp = dice.split("D");
        nb = parseInt(temp[0]);
        if (isNaN(nb)) {
            nb = 1;
            notANumber(temp[0],"1");
        }
        faces = parseInt(temp[1]);
        if (isNaN(faces)) {
            faces = 6;
            notANumber(temp[1],"6");
        }
    }
    else {
        myconsole.log('There is neither "d" nor "D" in the string to parse. Exiting function.');
        return false;
    }
    myconsole.log(s + " => " + String([nb, faces, pips]));
    return [nb, faces, pips];
}

function testParseDice(){
    let t = new MyTest("testParseDice");
    t.assert(parseDice("2d6+3"),[2,6,3]);
    t.assert(parseDice("14D6-1"),[14,6,-1]);
    t.assert(parseDice("8d7+145"),[8,7,145]);
    t.assert(parseDice("D6+1"),[1,6,1]);
    t.assert(parseDice("d8-3"),[1,8,-3]);
    t.assert(parseDice("2d12"),[2,12,0]);
    t.assert(parseDice("d24"),[1,24,0]);
    myconsole.log("--- Error cases");
    t.assert(parseDice("2d6+test"),[2,6,0]);
    t.assert(parseDice("d12-grumph"),[1,12,0]);
    t.assert(parseDice("john"),false);
    t.report();
}

testParseDice()


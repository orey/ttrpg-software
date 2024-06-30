/**********************************************
 * Dice module
 * Author: rey.olivier@gmail.com
 * Licence: GNU GPL v3
 * Date: July 2024
 ***********************************************/
"use strict";


/*
  Main random function: Used to throw a dice with 'faces' faces 
 */
function rollDie(faces){
    return Math.floor((Math.random()*faces)+1);
}


/*
  Small Helper to reduce line of codes
*/
function badCombination(str){
    let phrase = "Bad combination";
    if (str)
        console.error(phrase + ": " + str)
    else
        console.error(phrase)
    return false;
}


/*
  Parse the combination Xd/DY+-Z
  Send back an array of X Y Z with Z potentially negative
  Supports 'd' and 'D'
  (See the Lisp code for the lisp version)
*/
function parseCombi(combi) {
    if ((typeof(combi) != "string") || (combi.length < 2))  //we accept 'd6'
        return badCombination();
    
    let xstr = "", ystr = "", zstr = "",
        xdone = false, ydone = false,
        znegative = 1; //false
    for (let mychar of combi) {
        if ((mychar === 'd') || (mychar === 'D')) {
            if (xstr === "") xstr = "1"; //implicit x
            xdone = true;
        }
        else if ((mychar === '+') || (mychar === '-')) {
            if ((!xdone) || (ystr === ""))
                return badCombination("Expecting d or D");
            ydone = true;
            if (mychar === '-') znegative = -1; //true
        }
        else {
            if (ydone)
                zstr += mychar;
            else if (xdone)
                ystr += mychar;
            else
                xstr += mychar;
        }
    }
    if ((!xdone) || (ystr === ""))
        return badCombination("Expecting d or D");
    // convert into numbers
    return [parseInt(xstr),
            parseInt(ystr),
            zstr === "" ? 0 : znegative * parseInt(zstr)];
}


/*
  Main rolling function for all games
  (The Lisp version is much more complex but for fun)
*/
function rollCombi(combination){
    let combi = parseCombi(combination);
    if (!combi) return badCombination();
    let acc = 0;
    for (let i = 0; i < combi[0]; i++)
        acc += rollDie(combi[1]);
    return acc += combi[2];
}


/*
  Second way to draw abilities with 4d6 and keep the 3 best
*/
function rollFourKeepThree(verbose=false){
    let arr = [rollCombi("1d6"), rollCombi("1d6"), rollCombi("1d6"), rollCombi("1d6")];
    if (verbose) console.log(arr);
    let min = Math.min(...arr);
    let index = arr.indexOf(min);
    arr.splice(index, 1);
    if (verbose) console.log(arr);
    return arr.reduce((x, y) => x + y);
}




/*****************************************/

if (typeof module !== "undefined" && module.exports) {
    module.exports = { rollDie,
                       parseCombi,
                       rollCombi,
                       rollFourKeepThree,
                     }
}


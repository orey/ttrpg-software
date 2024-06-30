/**********************************************
 * Utils
 * Author: rey.olivier@gmail.com
 * Licence: GNU GPL v3
 * Date: July 2024
 ***********************************************/
"use strict";

//------------------------------------------------------predicates
function functionp (f) {
    return typeof(f) == 'function' ? true : false;
}

function arrayp (arr) {
    return Array.isArray(arr);
}

function evenp(n) {
   return n % 2 == 0;
}

function oddp(n) {
    return n % 2 != 0;
}

function integerp(value) {
  return !isNaN(value) && 
         parseInt(Number(value)) == value && 
         !isNaN(parseInt(value, 10));
}

//------------------------------------------------------functions
function fapply (f, tab){
    let res = [];
    if ((functionp(f)) && (arrayp(tab))){
        for (let elem of tab){
            res.push(f(elem));
        }
        return res;
    }
    return false;
}


//------------------------------------------------------I-O
function output(s, verbose = false){
    if (verbose)
        console.log(s);
}

function out(s, verbose = true){
    if (verbose)
        console.log(s);
}


//------------------------------------------------------format
function formatPercentage(x) {
    return parseFloat(x).toFixed(2)+"%";
}


//------------------------------------------------------tools
function range(n) {
    if (!integerp(n)) return false;
    return [...Array(n).keys()];
}


//------------------------------------------------------EXPORTS

if (typeof module !== "undefined" && module.exports) {
    module.exports = { functionp,
                       arrayp,
                       fapply,
                       output,
                       out,
                       evenp,
                       oddp,
                       formatPercentage,
                       integerp,
                       range,
                     }
}

/**********************************************
 * generate.js
 * Author: rey.olivier@gmail.com
 * Licence: GNU GPL v3
 * Date: Février 2023
 ***********************************************/
"use strict";

const Adj  = require("./adjectifs.js");
const Rand = require("./random.js");
const Nom  = require("./nom.js");

function generate(n){
    let tab = Rand.chooseInList(Adj.ADJECTIFS, n);
    return tab;
}

console.log("---");
console.log(generate(1));
console.log("---");
console.log(generate(3));
console.log("---");
console.log(generate(5));
console.log("---");
console.log(generate(10));
console.log("---");

console.log(Nom.generateNoms(3, "CVCV"));
console.log("---");
console.log(Nom.generateNoms(3, "VCCV CVV"));
console.log("---");
console.log(Nom.generateNoms(2, "CVC VVCV CVCVV"));
console.log("---");


Nom.testGenerateNoms(25);

console.log("---");
console.log(Nom.generateNoms(50, "PSYCV"));
console.log(Nom.generateNoms(50, "PSYCVC"));
console.log(Nom.generateNoms(50, "PSYCVCV"));
console.log("---");




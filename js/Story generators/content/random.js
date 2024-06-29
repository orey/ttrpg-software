/**********************************************
 * random.js
 * Utilitaires random
 * Author: rey.olivier@gmail.com
 * Licence: GNU GPL v3
 * Date: Février 2023
 ***********************************************/
"use strict";


// Random functions
function getRandomInt(min, max) {
  	return Math.floor(Math.random() * (max - min)) + min;
}

function getRandom(nb) {
    return Math.floor(Math.random() * nb);
}

// Main function for dice roll
function dice(nb) {
    return Math.floor(Math.random() * nb) + 1;
}


// Fonction d'aide : tir random dans une liste
// On s'assure ne pas l'avoir déjà trouvé
function chooseInList(l, nb=1) {
    let tab = []; // main array
    let alreadydrawn = [];
    let rand = 0;
    let index = 0;
    for (let i=0;i<nb;i++) {
        rand = getRandom(l.length);
        if (! alreadydrawn.includes(rand)) {
            alreadydrawn[index] = rand;
            index++;
        }
        else {
            while (alreadydrawn.includes(rand))
                rand = getRandom(l.length);
            alreadydrawn[index] = rand;
            index++;            
        }
        tab[i] = l[rand];
    }
    return tab;
}


function testChooseInList() {
    const list1 = [
        "vénéré", //0
        "vengeur",
        "verbeux",
        "véridique",
        "vertueux",
        "vibrant",
        "vicieux",
        "victorieux",
        "vide",
        "vieilli",
        "vieux",//10
    ];
    console.log("---\nTest chooseInList - 1 choix\n---");
    for (let i=0;i<50;i++)
        process.stdout.write(chooseInList(list1).toString() + "|");
    process.stdout.write('\n');
    console.log("---\nTest chooseInList - 3 choix\n---");
    for (let i=0;i<50;i++)
        process.stdout.write(chooseInList(list1, 3).toString() + "|");
    process.stdout.write('\n');
    console.log("Done");
}


// Test des fonctions random
function testGetRandom(nb, f=getRandom, startswith=0) {
    let tab = new Array(nb).fill(0);
    const THRESHOLD = 100000;
    for (let i=0;i<THRESHOLD;i++)
        tab[f(nb)-startswith]++;
    process.stdout.write(  "TARGET NUMBER |");
    for (let j=0;j<tab.length;j++)
        process.stdout.write(String(j + startswith).padStart(2, '0') + "|");
    process.stdout.write("\nPERCENTAGE:   |");
    for (let e of tab)
        process.stdout.write(String(Math.round(e/THRESHOLD*100)).padStart(2, '0') + "|");
    process.stdout.write('\n');
}


function testAllRandom() {
    console.log("---\nTests basés sur 10 000 tirs\n---");
    console.log("Test avec 30");
    testGetRandom(30);
    console.log("Test avec 22");
    testGetRandom(22);
    console.log("============== Tests de la fonction dice");
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


/*--------------------------------------
 * Exports
 *--------------------------------------*/
if (typeof module !== "undefined" && module.exports) {
    module.exports = {
        getRandom,
        testAllRandom,
        testChooseInList,
        dice,
        chooseInList,
    }
}


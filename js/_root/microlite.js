/**********************************************
 * Implementation of Microlite
 * Author: rey.olivier@gmail.com
 * Licence: GNU GPL v3
 * Date: December 2024
 ***********************************************/
"use strict";

const utils = require('./utils');
const dice = require('./dice');


class Character {
    constructor() {
        this.str = 0;
        this.dex = 0;
        this.mind = 0;
    }
    print() {
        utils.out("STR:  " + this.str);
        utils.out("DEX:  " + this.dex);
        utils.out("MIND: " + this.mind);
    }
}


function characterGenerator(){
    let c = new Character();
    c.str = dice.rollFourKeepThree(true);
    c.dex = dice.rollFourKeepThree(true);
    c.mind = dice.rollFourKeepThree(true);
    c.print()
}

characterGenerator()


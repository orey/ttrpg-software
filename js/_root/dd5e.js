/**********************************************
 * DD5e
 * Author: rey.olivier@gmail.com
 * Licence: GNU GPL v3
 * Date: July 2024
 ***********************************************/
"use strict";

const utils = require('./utils');
const dice = require('./dice');

const ABILITY_WAY_STANDARD = 0;
const STANDARD_SET = [15,14,13,12,10,8];
const ABILITY_WAY_3D6 = 1;
const ABILITY_WAY_2D6PLUS6 = 2;


//----------------------------------------------------Abilities
/*
Ability modifiers
*/
function abilityModifier(ability){
    if ((!utils.integerp(ability)) && (ability <1)) return false;
    if (ability == 1) return -5;
    else return Math.floor(ability/2) - 5;
}



function generateAbilities(way=ABILITY_WAY_STANDARD){
    let res = [];
    switch(way) {
    case ABILITY_WAY_STANDARD:
        return STANDARD_SET;
    case ABILITY_WAY_3D6:
        for (let i in utils.range(6))
            res.push(dice.rollCombi("3d6"));
        return res;
    case ABILITY_WAY_2D6PLUS6:
        for (let i in utils.range(6))
            res.push(dice.rollCombi("2d6+6"));
        return res;
    default:
        return STANDARD_SET;
    }
}
















//------------------------------------------------------EXPORTS

if (typeof module !== "undefined" && module.exports) {
    module.exports = { abilityModifier,
                       generateAbilities,
                       ABILITY_WAY_STANDARD,
                       ABILITY_WAY_3D6,
                       ABILITY_WAY_2D6PLUS6,
                     }
}
 
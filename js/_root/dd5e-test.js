/**********************************************
 * DD5e tests
 * Author: rey.olivier@gmail.com
 * Licence: GNU GPL v3
 * Date: July 2024
 ***********************************************/
"use strict";

const utils = require('./utils');
const dd = require('./dd5e');

//-----------------------------------------------abilityModifier
function testAbilityModifier(){
    console.log("---------------------------testAbilityModifier");
    for (let i of utils.range(31))
        console.log("Ability modifier of " + i + ": " + dd.abilityModifier(i));
}

testAbilityModifier();

//-----------------------------------------------generateAbilities
function testGenerateAbilities(){
    console.log("---------------------------testGenerateAbilities");
    console.log(dd.generateAbilities());
    console.log(dd.generateAbilities(12));
    console.log(dd.generateAbilities(dd.ABILITY_WAY_STANDARD));
    console.log(dd.generateAbilities(dd.ABILITY_WAY_3D6));
    console.log(dd.generateAbilities(dd.ABILITY_WAY_2D6PLUS6));
    console.log(dd.generateAbilities(dd.ABILITY_WAY_4D6));
}

testGenerateAbilities();



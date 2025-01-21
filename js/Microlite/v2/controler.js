"use strict";

const DATA_TYPE = "data-type";
const COMPONENTS = {};

function constructGraphicalElement(myid){
    const elem = document.getElementById(myid);
    const elemtype = elem.getAttribute(DATA_TYPE);
    const strtoexec = `new ${elemtype}("${myid}")`;
    console.log(strtoexec);
    const theclass = eval(strtoexec);
    COMPONENTS[myid] = theclass;
}

function construct() {
    constructGraphicalElement("dice-thrower1");
    constructGraphicalElement("dice-thrower2");
}

function destroy() {
    for (let key in COMPONENTS) {
        COMPONENTS[key].destroy();
    }
}



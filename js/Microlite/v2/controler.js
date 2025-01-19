"use strict";

function constructGraphicalElement(myid){
    const elem = document.getElementById(myid);
    const elemtype = elem.getAttribute("data-type");
    const strtoexec = `new ${elemtype}("${myid}")`;
    console.log(strtoexec);
    const theclass = eval(strtoexec);    
}

constructGraphicalElement("dice-thrower1");
constructGraphicalElement("dice-thrower2");


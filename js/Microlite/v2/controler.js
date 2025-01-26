"use strict";

const CONTROL_EVENT = "CONTROL";
const DATA_TYPE = "data-type"; // HTML attribute for div, data-* are developer's attributes

function controlEventProcessor(obj) {
    myconsole.log("Event received by controller: " + JSON.stringify(obj));
}

class Controller {
    constructor() {
        this.components = {}; // HTML div id : instance of component
        this.pubsub = new PubSub();
        this.pubsub.subscribe(CONTROL_EVENT, controlEventProcessor);
    }
    constructGraphicalElement(myid){
        const elem = document.getElementById(myid);
        const elemtype = elem.getAttribute(DATA_TYPE);
        const strtoexec = `new ${elemtype}("${myid}")`;
        console.log(strtoexec);
        const theclass = eval(strtoexec);
        theclass.pubsub = this.pubsub;
        this.components[myid] = theclass;
    }
}

const mycontroller = new Controller();

function construct() {
    //future : parse a selected area and create in a loop
    mycontroller.constructGraphicalElement("dice-thrower1");
    mycontroller.constructGraphicalElement("dice-thrower2");
    mycontroller.constructGraphicalElement("stats-deter1");
    mycontroller.constructGraphicalElement("stats-deter2");
    
}

function destroy() {
    for (let key in mycontroller.components) {
        mycontroller.components[key].destroy();
    }
}









"use strict";

/*
  This component does not use a shadow DOM.
  A component is defined by:
  -A class
    -The name of the class is the "data-type" attribute of the "div" tag.
    -With a "constructor(id)". The class must be able to build components
    that are instantiated multiple times in the same html page.
    -With the "destroy()" method that removes HTML within the "div".
    -The component has a "pubsub" field that enables to interact with the
    rest of components in a loosely coupled way.
  
 */

class DiceThrower {
    constructor(id){
        this.id = id;
        this.elem = document.getElementById(id);
        this.pubsub = null;
        // Names of attributes must be relative if we have a real component
        this.formula = DiceThrower.buildFormulaFieldName(id);
        this.display = DiceThrower.buildDisplayFieldName(id);
        this.elem.innerHTML = 
`<h2>Dice thrower</h2>
<label for="${this.formula}">Dice formula (xD+/-y):</label>
<input type="text" id="${this.formula}" name="${this.formula}"><br><br>
<button onclick="diceThrow('${id}')">Dice throw</button>
<button onclick="diceClear('${id}')">Clear</button>

<hr><div id="${this.display}" class="${this.display}"></div><hr>`;
        myconsole.log(`Type = dice-thrower, id = ${id}`);
    }
    static buildFormulaFieldName(id) {
        return "dice-formula" + "-" + id;
    }
    static buildDisplayFieldName(id) {
        return "dice-display" + "-" + id;
    }
    destroy() {
        this.pubsub.publish("CONTROL",
                            {message: "DiceThrower " + String(this.id) + " about to be destroyed..."});
        this.elem.innerHTML = "";
    }
    
}

/*
  Here are functions that interact with the component. They have to be sensitive
  to the instance of component and cannot be simple listeners.
 */

 /*================================================================DICE*/
function diceThrow(id) {
    let diceFormula = document.getElementById(DiceThrower.buildFormulaFieldName(id)).value;
    if (diceFormula == "") {
        myconsole.log("Defaulting on 1d20");
        diceFormula = "1d20";
        document.getElementById(DiceThrower.buildFormulaFieldName(id)).value = "1d20";
    }
    const formula = parseDice(diceFormula);
    const myroll = roll(...formula);
    
    const diceDisplay = document.getElementById(DiceThrower.buildDisplayFieldName(id));
    diceDisplay.innerHTML =
`<p><strong>Dice formula:</strong> ${diceFormula}</p>
 <p><strong>Dice formula decrypted:</strong> ${formula}</p>
 <p><strong>Result:</strong> ${myroll}</p>`;
}

function diceClear(id) {
    document.getElementById(DiceThrower.buildFormulaFieldName(id)).value = "";
    document.getElementById(DiceThrower.buildDisplayFieldName(id)).innerHTML = "";
}
            

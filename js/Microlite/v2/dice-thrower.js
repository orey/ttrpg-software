"use strict";

class DiceThrower {
    constructor(id){
        this.id = id;
        this.elem = document.getElementById(id);
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
        this.pubsub = null;
    }
    static buildFormulaFieldName(id) {
        return "dice-formula" + "-" + id;
    }
    static buildDisplayFieldName(id) {
        return "dice-display" + "-" + id;
    }
    destroy() {
        this.pubsub.publish("CONTROL",
                            {message: "About to be destroyed..."});
        this.elem.innerHTML = "";
    }
    
}
    

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
            

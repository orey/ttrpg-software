"use strict";

class DiceThrower {
    constructor(id){
        this.id = id;
        this.elem = document.getElementById(id);
        // Names of attributes must be relative if we have a real component
        this.formula = DiceThrower.formulaField(id);
        this.display = DiceThrower.displayField(id);
        this.elem.innerHTML = 
`<h2>Dice thrower</h2>
<label for="${this.formula}">Dice formula (xD+/-y):</label>
<input type="text" id="${this.formula}" name="${this.formula}"><br><br>
<button onclick="diceThrow('${id}')">Dice throw</button>
<button onclick="diceClear('${id}')">Clear</button>

<hr><div id="${this.display}" class="${this.display}"></div><hr>`;
        myconsole.log(`Type = dice-thrower, id = ${id}`);
    }
    static formulaField(id) {
        return "dice-formula" + "-" + id;
    }
    static displayField(id) {
        return "dice-display" + "-" + id;
    }

}

 /*================================================================DICE*/
function diceThrow(id) {
    let diceFormula = document.getElementById(DiceThrower.formulaField(id)).value;
    if (diceFormula == "") {
        myconsole.log("Defaulting on 1d20");
        diceFormula = "1d20";
        document.getElementById(DiceThrower.formulaField(id)).value = "1d20";
    }
    const formula = parseDice(diceFormula);
    const myroll = roll(...formula);
    
    const diceDisplay = document.getElementById(DiceThrower.displayField(id));
    diceDisplay.innerHTML = `
                     <p><strong>Dice formula:</strong> ${diceFormula}</p>
                     <p><strong>Dice formula decrypted:</strong> ${formula}</p>
                     <p><strong>Result:</strong> ${myroll}</p>
                 `;
}

function diceClear(id) {
    document.getElementById(DiceThrower.formulaField(id)).value = "";
    document.getElementById(DiceThrower.displayField(id)).innerHTML = "";
}
            

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

class StatsDetermination {
    constructor(id){
        this.id = id;
        this.elem = document.getElementById(id);
        this.pubsub = null;
        this.elem.innerHTML = 
`<h2>Stats Determination</h2>
<button onclick="throwFourKeepThreeGraphical('${id}')">Throw 4D6 keep the best 3</button>
<button onclick="caracClear('${id}')">Clear</button>

 <div id="carac-display" class="carac-display">
 <p>Throws:
 <input type="text" class="stat" id="dice1" name="dice1-${id}" readonly>
 <input type="text" class="stat" id="dice2" name="dice2-${id}" readonly>
 <input type="text" class="stat" id="dice3" name="dice3-${id}" readonly>
 <input type="text" class="stat" id="dice4" name="dice4-${id}" readonly>
 </p>
 <p>Total of the 3 best:
 <input type="text" class="stat" id="threebest-${id}" name="threebest-${id}"readonly>
 </div>`;
    }
    destroy() {
        this.pubsub.publish("CONTROL",
                            {message: "StatsDetermination " + String(id) + " about to be destroyed..."});
        this.elem.innerHTML = "";
    }
}

/**================================================================STATS*/
function throwFourKeepThreeGraphical(id) {
    let res, mytab;
    [res, mytab] = throwFourKeepThree();
    for (let i = 0; i < mytab.length;i++) {
        document.getElementById('dice' + String(i+1) + "-" + String(id)).value = mytab[i];
    }
    document.getElementById('threebest'+ "-" + String(id)).value = res;
}

function caracClear(id) {
    document.getElementById('dice1'+ "-" + String(id)).value = "";
    document.getElementById('dice2'+ "-" + String(id)).value = "";
    document.getElementById('dice3'+ "-" + String(id)).value = "";
    document.getElementById('dice4'+ "-" + String(id)).value = "";
    document.getElementById('threebest'+ "-" + String(id)).value = "";
}
           

"using strict";

/*========================================================Dice section*/

// Random function
function rollDie(faces){
    return Math.floor((Math.random()*faces)+1);
}

// Dice roller
function roll(number, faces, pips){
    let result = pips;
    let temp = 0;
    for (let i=0;i<number;i++){
        temp = rollDie(faces);
        result += temp;
    }
    return result;
}

function notANumber(s,defo="0"){
    myconsole.log('"' + s + '" is not a number, replacing by: ' + defo);
}

/*-------------------------------------------------
  Expected input is "2d6+3" or "4d10"
  -------------------------------------------------*/
function parseDice(s){
    let nb = 0, faces = 0, pips = 0;
    let dice = "";
    // Is there a pip?
    if ((s.includes("+")) || (s.includes("-")) ) {
        let temp;
        if (s.includes("+")) {
            temp = s.split("+");
            pips = parseInt(temp[1]);
        }
        if (s.includes("-")){
            temp = s.split("-");
            pips = parseInt('-' + temp[1]);
        }
        if (isNaN(pips)){
            notANumber(temp[1],"0");
            pips = 0;
        }
        dice = temp[0];
    }
    else
        dice = s;
    if ((dice.includes("d")) || (dice.includes("D"))) {
        let temp;
        if (dice.includes("d"))
            temp = dice.split("d");
        if (dice.includes("D"))
            temp = dice.split("D");
        nb = parseInt(temp[0]);
        if (isNaN(nb)) {
            nb = 1;
            notANumber(temp[0],"1");
        }
        faces = parseInt(temp[1]);
        if (isNaN(faces)) {
            faces = 6;
            notANumber(temp[1],"6");
        }
    }
    else {
        myconsole.log('There is neither "d" nor "D" in the string to parse. Exiting function.');
        return false;
    }
    myconsole.log(s + " => " + String([nb, faces, pips]));
    return [nb, faces, pips];
}

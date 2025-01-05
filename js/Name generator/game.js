/*---------------------------
   O. Rey - 14 février 2023
   ---------------------------*/
'using strict;'

const Name = require('./name-fr.js');

function die(nb) {
    return Math.floor(Math.random() * nb) + 1;
}

// Formula can be 2d6+2 or 1d20
function dice(formula, verbose=false) {
    if (verbose)
        console.log(formula);
    let tab = formula.split('+');
    let rest = (tab[1] == undefined ? 0 : parseInt(tab[1]));
    let nb = 0, dic = 0;
    let temp = []
    if (tab[0].includes('d'))
        temp = tab[0].split('d');
    else if (tab[0].includes('D'))
        temp = tab[0].split('D');
    nb = parseInt(temp[0]);
    dic = parseInt(temp[1]);
    if (verbose)
        console.log("%d D %d + %d", nb, dic, rest);
    let cumul = 0;
    for (let i=0;i<nb;i++)
        cumul += die(dic);
    return (cumul + rest);
}

class Perso {
    constructor(name, comp,pdv,cha,psy){
        this.name = name;
        this.comp = dice(comp);
        this.pdv  = dice(pdv);
        this.cha  = dice(cha);
        this.psy  = dice(psy);
    }
    longPrint() {
        console.log("Name:  %s", this.name);
        console.log("COMP: %d" , this.comp);
        console.log("PdV: %d"  , this.pdv);
        console.log("CHA: %d"  , this.cha);
        console.log("PSY: %d"  , this.psy);
    }
    print() {
        console.log("Name:  %s | COMP: %d | PdV: %d | CHA: %d | PSY: %d",
                    this.name,
                    this.comp,
                    this.pdv,
                    this.cha,
                    this.psy
                   );
    }
    
}

const PatternPrenom = [
    "CVC", "CVCVC", "VCVV", "CVCCV"
];

const PatternNom = [
    "CVCVCCVV", "CVVC", "CVCC", "CVVCVCVV"
];


/*console.log(dice("2D6+4"));
console.log(dice("1d20"));
console.log(dice("4d10+1"));
console.log(dice("2D6+12"));

console.log(Name.generateNom("CVVCV"));*/

let tab = [];
for (let i=0;i<4;i++)
    tab[i] = new Perso(Name.generateNom(PatternPrenom[i]) + " " + Name.generateNom(PatternNom[i]),
                       "1D6+6",  //COMP
                       "2D6+12", //PDV
                       "1D6+6",  //CHA
                       "1D6+6"   //PSY
                      );
for (a of tab)
    a.print();

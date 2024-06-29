/*---------------------------
   O. Rey - 8 septembre 2022
   ---------------------------*/
'using strict;'

const Rand = require("./random.js");

// Fréquence des lettres en français
// e, s,   a,  r, t, i, n, u, l, o, d, c, reste
// 13 12  11  10  9  8  7  6  5  4  3  2   1
const CONSONNES=['b','c','d','f','g','h','j','k','l','m','n','n','n','n','n','n','n','p','q','r','s','t','v','w','x','z','ll','st','rr','ss','nn','mm','ph','kh','ch','sh','pp','qu','tt','ff','gu'];

const CONSONNES2=['b','c','c','d','d','d','f','g','h','j','k','l','l','l','l','l','m','n','p','q','r','r','r','r','r','r','r','r','r','r','s','s','s','s','s','s','s','s','s','s','s','s','t','t','t','t','t','t','t','t','t','v','w','x','z'];

const VOYELLES=['a','a','a','a','a','a','a','a','a','a','a','e','e','e','e','e','e','e','e','e','e','e','e','e','i','i','i','i','i','i','i','i','o','o','o','o','u','u','u','u','u','u','y'];


// capitalize the first letter
function capFirst(str) {
    return str[0].toUpperCase() + str.slice(1);
}


// getConsonne
function getConsonne(index=2) {
    if (index==2) {
        let nb = CONSONNES2.length;
        return CONSONNES2[Rand.getRandom(nb)];
    }
    let nb = CONSONNES.length;
    return CONSONNES[Rand.getRandom(nb)];
}

// getVoyelle
function getVoyelle()  {
    let nb = VOYELLES.length;
    return  VOYELLES[Rand.getRandom(nb)];
}

//generate
function generate(s, index=2) {
    let temp = "";
    for (let c of s) {
        switch(c){
        case 'C':
            temp += getConsonne(index);
            break;
        case 'V':
            temp += getVoyelle();
            break;
        // on garde les autres lettres    
        default:
            temp += c;
            break;
        }
    }
    return temp;
}

// Les deux fonctions exportées
// renvoit un seul nom en string
function generateNom(pattern, index=2){
    return capFirst(generate(pattern, index));
}

// renvoit un tableau de noms
function generateNoms(nb, pattern, index=2){
    let tab = [];
    for (let i=0;i<nb;i++)
        tab[i] = capFirst(generate(pattern,index));
    return tab;
}

function usage(){
    console.log("Usage: node nom.js [CVCVCVVVVCCC]");
}

function main(){
    var arguments = process.argv ;  
    console.log(arguments) ;
    if (arguments.length == 2) {
        usage();
    }
    else {
        console.log(generateNoms(100,arguments[2]));
        console.log("---");
        console.log(generateNoms(100,arguments[2], 1));
    }
}

//main();

function testGenerateNoms(nb) {
    console.log(generateNoms(nb,"CVCVC CV CVVCCV"));
    console.log("---");
    console.log(generateNoms(nb, "CVCVC CV CVVCCV", 1));
}


/*--------------------------------------
 * Exports
 *--------------------------------------*/
if (typeof module !== "undefined" && module.exports) {
    module.exports = {
        generateNom,
        generateNoms,
        testGenerateNoms,
    }
}

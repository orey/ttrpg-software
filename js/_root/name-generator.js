/*---------------------------
   O. Rey - 8 septembre 2022
   ---------------------------*/
'using strict;'

const utils = require('./utils');
const dice = require('./dice');

// Fréquence des lettres en français
// e, s,   a,  r, t, i, n, u, l, o, d, c, reste
// 13 12  11  10  9  8  7  6  5  4  3  2   1
const CONSONNES=['b','c','d','f','g','h','j','k','l','m','n','n','n','n','n','n','n','p','q','r','s','t','v','w','x','z','ll','st','rr','ss','nn','mm','ph','kh','ch','sh','pp','qu','tt','ff','gu'];
const CONSONNES2=['b','c','c','d','d','d','f','g','h','j','k','l','l','l','l','l','m','n','p','q','r','r','r','r','r','r','r','r','r','r','s','s','s','s','s','s','s','s','s','s','s','s','t','t','t','t','t','t','t','t','t','v','w','x','z'];
const VOYELLES=['a','a','a','a','a','a','a','a','a','a','a','e','e','e','e','e','e','e','e','e','e','e','e','e','i','i','i','i','i','i','i','i','o','o','o','o','u','u','u','u','u','u','y'];


//======= UTIL FUNCTIONS

function getConsonne(index=2) {
    if (index==2) {
        let nb = CONSONNES2.length;
        return CONSONNES2[dice.getRandom(nb)];
    }
    let nb = CONSONNES.length;
    return CONSONNES[dice.getRandom(nb)];
}

function getVoyelle()  {
    let nb = VOYELLES.length;
    return  VOYELLES[dice.getRandom(nb)];
}

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
        case ' ':
            temp += ' ';
            break;
        default:
            temp += c;
            break;
        }
    }
    return temp;
}

function generateNom(pattern, index=2){
    return utils.capFirst(generate(pattern, index));
}

function generateNoms(nb, pattern, index=2){
    let tab = [];
    let tokens = pattern.split(' ');
    for (let i=0;i<nb;i++){
        mystr = "";
        for (let token of tokens){
            mystr += utils.capFirst(generate(token,index)) + ' ';
        }
        tab.push(mystr.trim());
    }
    return tab;
}


function usage(){
    console.log("Usage: node generate-nom.js [CVCVCVVVVCCC]");
    console.log("C: consonne, V voyelle. Prends aussi les autres lettres");
    process.exit()
}


function main(){
    let argslen = process.argv.length;
    if (argslen < 3)
        usage();
    let str = "";
    for (let i=2;i<argslen;i++)
        str +=  process.argv[i] + ' '
    str = str.trim();
    console.log(str);
    let tab = generateNoms(20, str);
    for (let elem of tab)
        console.log(elem);
}

main();


/*--------------------------------------
 * Exports
 *--------------------------------------*/
if (typeof module !== "undefined" && module.exports) {
    module.exports = {
        generateNom,
    }
}

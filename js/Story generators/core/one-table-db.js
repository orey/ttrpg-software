/**********************************************
 * One table DB
 * Author: rey.olivier@gmail.com
 * Licence: GNU GPL v3
 * Date: November 2021
 * This is a simple storage system aiming at 
 * storing one table and emulating simple graph
 * behavior
 * expected file extension is "otdb"
 ***********************************************/
"use strict";
const fs = require('fs')

const SEP = '|';
const COMMENT = '//';
const QUESTION = '?';

class OneTableDB {
    constructor(filename, verbose=false) {
        this.DB = [];
        try {
            // read contents of the file
            const data = fs.readFileSync(filename, 'UTF-8');

            // split the contents by new line
            const lines = data.split(/\r?\n/);
        
            let i = 0;
            // print all lines
            lines.forEach((line) => {
                if ((line.startsWith(COMMENT)) && verbose) 
                    console.log("Excluding comment:\n%s",line);
                else if ((line.trim == '') && verbose)
                    console.log("Found blank line. Skipping...");
                else {
                    let tab = line.split(SEP);
                    // line is supposed to be a triple
                    if (tab.length != 3) 
                        console.log("Malformed line (not 3 separators).\n%s\nSkipping...", line);
                    else {
                        //console.log(tab);
                        if (verbose)
                            console.log("Line: %s - %s - %s", tab[0], tab[1], tab[2]);
                        this.DB[i] = tab;
                        i += 1;
                    }
                }
                if (verbose)
                    console.log(this.DB);
            });
        }
        catch (err) {
            console.error(err);
            return null;
        }
    }

    /*****************************************/

    select(s, p, o) {
        return this.selectWithMatchingFunction(s, p, o, function (x, y) {
            return (x == y) ? true : false;
        });

    }
    
    selectBlur(s, p, o) {
        return this.selectWithMatchingFunction(s, p, o, function (x, y) {
            // y sub part of x
            return x.includes(y) ? true : false;
        });

    }

    /*
      For the moment the query is simple: variables are accepted in the triple
    */
    selectWithMatchingFunction(s, p, o, f) {
        let variables = 0;
        if (s.startsWith(QUESTION))
            variables += 4; // 100
        if (p.startsWith(QUESTION))
            variables += 2; // 010
        if (o.startsWith(QUESTION))
            variables += 1; // 001
        let len = this.DB.length;
        let line = "";
        let results = [];
        for (let i=0;i<len;i++) {
            line = this.DB[i];
            switch (variables) {
            case 1: 
                // 001 value value ?var
                if (f(line[0], s) && f(line[1], p))
                    results.push(line);
                break;
            case 2:
                // 010 value ?var value
                if (f(line[0], s) && f(line[2], o))
                    results.push(line);
                break;
            case 4:
                // 100 ?var value value
                if (f(line[1], p) && f(line[2], o))
                    results.push(line);
                break;
            case 3:
                // 011 value ?var ?var
                if (f(line[0], s))
                    results.push(line);
                break;
            case 5:
                // 101 ?var value ?var
                if (f(line[1], p))
                    results.push(line);
                break;
            case 6:
            // 110 ?var ?var value
                if (f(line[2], o))
                    results.push(line);
                break;
            case 7:
                // 111 ?var ?var ?var
                results.push(line);
                break;
            }
        }
        return results;
    }
}


/*****************************************/
function test() {
    let db = new OneTableDB("data.otdb", false);
    console.log("001")
    console.log(db.select("Tombouctou", "a", "?"));
    console.log("010")
    console.log(db.select("Hero", "?", "a big sword"));
    console.log("100")
    console.log(db.select("?", "a", "City"));
    console.log("011")
    console.log(db.select("Paris", "?", "?"));
    console.log("101")
    console.log(db.select("?", "has", "?"));
    console.log("110")
    console.log(db.select("a dream", "a","?"));
    console.log("111")
    console.log(db.select("?", "?", "?"));
    let results = db.select("?", "a", "turtle");
    console.log(results.length)
    console.log("******************");
    console.log(db.selectBlur("?", "?", "gen"));
    console.log(db.selectBlur("the", "?", "?"));
}

test()

/*****************************************/

if (typeof module !== "undefined" && module.exports) {
    module.exports = {
        test,
    }
}



/*--------------------------------------------
Filename: classes.js
Author: https://github.io/orey/gme
Creation date: June 18 2022
Under GNU GPL v3 licence
--------------------------------------------
Classes du GME
--------------------------------------------*/
'use strict';

const VERBOSE= true;

const myconsole = {
    log: function(s){
        if (VERBOSE)
            console.log(s);
    }
}

//============================= UTILS

function func() {
    return ( ( ( 1+Math.random() ) * 0x10000 ) | 0 ).toString( 16 ).substring( 1 );
}

function createUUID() {
    // For calling it, stitch '3' in the 3rd group
    return (func() + func() + "-" + func() + "-3"
            + func().substr(0,2) + "-" + func()
            + "-" + func() + func() + func()).toLowerCase();
}

//============================= HTML ZONES

function writeInZone(zone, text){
    document.getElementById(zone).innerHTML = text;
}

function cleanZone(zone){
    document.getElementById(zone).innerHTML = "";
}

//============================= CONSTANTS

const TYPE = {
    SCENE: "Scène",
    PNJ: "PNJ",
    PISTE: "Piste"
}

const STATUS = {
    NORMAL: "Normal",
    ALTEREE: "Altérée",
    INTERROMPUE: "Interrompue"
}

//============================= Document
class Document {
    constructor(type, title, sequence, descr = "", status = STATUS.NORMAL, alternate = ""){
        this.id = createUUID();
        this.type = type;
        this.title = title;
        this.sequence = sequence;
        this.descr = descr;
        this.status = status;
        this.alternate = alternate;
    }

    setStatus(status){
        this.status = status; // pour les scènes
    }

    setAlternate(alternate){
        this.alternate = alternate; // pour les scènes
    }

    toHTML() {
        return documentToHTML(this);
    }
    
    toTableLine() {
        return documentToTableLine(this);
    }
}

//============================= ListeDocs
class ListeDocs {
    liste = [];

    constructor() {}

    push(elem) {
        this.liste.push(elem);
    }

    toHTML(){
        return listeDocsToHtml(this);
    }

    getElemById(id) {
        for (let e of this.liste)
            if (e.id == id)
                return e;
    }
}

//================================================== Document display

function documentToHTML(doc){
    let output = `
<table>
  <thead>
    <tr>
      <th>Type</th>
      <th>Sequence</th>
      <th>Title</th>
`;
    if (doc.type == TYPE.SCENE)
        output += `<th>Status</th>`;
    output += `
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>${doc.type}</td>
      <td>${doc.sequence}</td>
      <td>${doc.title}</td>
`;
    if (doc.type == TYPE.SCENE)
        output += `<td>${doc.status}</td>`;
    output += `
    </tr>
  </tbody>
</table>
<table>
  <thead>
    <tr>
      <th>Description</th>
`;
    if (doc.type == TYPE.SCENE)
        output += `<th>Alternative</th>`;
    output += `
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>${doc.descr}</td>
`;
    if (doc.type == TYPE.SCENE)
        output += `<td>${doc.alternate}</td>`;
    output += `
    </tr>
  </tbody>
</table>
`;
    return output;
}

function documentToTableLine(doc){
    let output =
`<td>${doc.type}</td>
<td>${doc.sequence}</td>
<td>${doc.title}</td>
<td>${doc.descr}</td>
`;
    if (doc.type == TYPE.SCENE)
        output += `<td>${doc.status}</td><td>${doc.alternate}</td>`;
    return output;
}


//================================================== List of documents display

// Envoie vers editDoc(doc.id)
function listeDocsToHtml(myliste){
    // nous parcourons les 3 types de doc
    let outputpnj =
`
<table>
  <thead>
    <tr>
      <th>Type</th>
      <th>Séquence</th>
      <th>Nom</th>
      <th>Description</th>
      <th>Action</th>
    </tr>
  </thead>
  <tbody>
`;
    let outputpistes = outputpnj;
    let outputscenes =
`
<table>
  <thead>
    <tr>
      <th>Type</th>
      <th>Séquence</th>
      <th>Nom</th>
      <th>Description</th>
      <th>Statut</th>
      <th>Alternative</th>
      <th>Action</th>
    </tr>
  </thead>
  <tbody>
`;
    for (let e of myliste.liste){
        if (e.type == TYPE.PNJ)
            outputpnj += "<tr>" + e.toTableLine()
            + `<td><input value="Editer" type="submit" onclick="editDoc('${e.id}');"></td></tr>`;
        if (e.type == TYPE.PISTE)
            outputpistes += "<tr>" + e.toTableLine()
            + `<td><input value="Editer" type="submit" onclick="editDoc('${e.id}');"></td></tr>`;
        if (e.type == TYPE.SCENE)
            outputscenes += "<tr>" + e.toTableLine()
            + `<td><input value="Editer" type="submit" onclick="editDoc('${e.id}');"></td></tr>`;
    }
    outputpnj += `
  </tbody>
</table>
`;
    outputpistes += `
  </tbody>
</table>
`;
    outputscenes += `
  </tbody>
</table>
`;
    let separator = "<br/>";
    return outputscenes + separator + outputpnj + separator + outputpistes;
}

//================================================== Edition d'un doc

function editDoc(id){
    let e = DOCS.getElemById(id);
    myconsole.log(e);
    if (e.type != TYPE.SCENE)
        writeInZone('zoneedition', `
<table>
  <thead>
    <tr>
      <th>Id</th>
      <th>Type</th>
      <th>Sequence</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>${e.id}</td>
      <td>${e.type}</td>
      <td>${e.sequence}</td>
    </tr>
  </tbody>
</table>
<form>
  <label>Nom</label><br/>
  <input type="text" id="edit_title" value='${e.title}'><br/>
  <label>Description</label><br/>
  <textarea name="text" id="edit_descr" rows="20" cols="50">${e.descr}</textarea>
</form>
<input type="submit" value="Mettre à jour" onclick="updateDoc('${e.id}');"><br/>
`);
    else
        writeInZone('zoneedition', `
<table>
  <thead>
    <tr>
      <th>Id</th>
      <th>Type</th>
      <th>Sequence</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>${e.id}</td>
      <td>${e.type}</td>
      <td>${e.sequence}</td>
    </tr>
  </tbody>
</table>
<form>
  <label>Nom</label><br/>
  <input type="text" id="edit_title" value='${e.title}'><br/>
  <label>Description</label><br/>
  <textarea name="text" id="edit_descr" rows="20" cols="50">${e.descr}</textarea><br/>`
        + getRadioButtons(e)
        + `
  <br/><label>Alternative</label><br/>
  <textarea name="text" id="edit_alternate" rows="20" cols="50">${e.alternate}</textarea>
</form><br/>
<input type="submit" value="Mettre à jour" onclick="updateDoc('${e.id}');"><br/>
`);

}

function getRadioButtons(e){
    return `
<form>
<p>Statut</p>
<input type="radio" id="edit_status_normal" name="status" value="${STATUS.NORMAL}"`
        + (e.status == STATUS.NORMAL ? " checked" : "")
        + `
><label for="edit_status_normal">${STATUS.NORMAL}</label><br>
<input type="radio" id="edit_status_alteree" name="status" value="${STATUS.ALTEREE}"`
        + (e.status == STATUS.ALTEREE ? " checked" : "")
        + `
><label for="edit_status_alteree">${STATUS.ALTEREE}</label><br>
<input type="radio" id="edit_status_interrompue" name="status" value="${STATUS.INTERROMPUE}"`
        + (e.status == STATUS.INTERROMPUE ? " checked" : "")
        + `
><label for="edit_status_interrompue">${STATUS.INTERROMPUE}</label> 
</form>
`;
}

function updateDoc(id){
    let e = DOCS.getElemById(id);
    // get title
    e.title = document.getElementById("edit_title").value;
    e.descr = document.getElementById("edit_descr").value;
    myconsole.log(e);
    if (e.type == TYPE.SCENE) {
        let ele = document.getElementsByName('status');
        for (let i = 0; i < ele.length; i++) {
            if(ele[i].checked)
                e.status = ele[i].value;
        }
        e.alternate = document.getElementById("edit_alternate").value;
    }
    cleanZone('zoneedition');
    refresh();

}

//========================================================= Export
function jsonExport(){
    return JSON.stringify(DOCS);
}

function download(filename, text) {
    var element = document.createElement('a');
    element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
    element.setAttribute('download', filename);

    element.style.display = 'none';
    document.body.appendChild(element);

    element.click();

    document.body.removeChild(element);
}

// Start file download.
document.getElementById("dwn-btn").addEventListener("click", function(){
    // Generate download of hello.txt file with some content
    var text = jsonExport();
    var filename = "histoire.json";
    
    download(filename, text);
}, false);

//========================================================= DATA
const DOCS = new ListeDocs();

DOCS.push(new Document(TYPE.PNJ,"Johnny Go",1, "C'est ma vie"));
DOCS.push(new Document(TYPE.PNJ,"Fada Complet",2, "Rouboudou"));
DOCS.push(new Document(TYPE.PNJ,"Pas Glop",3, "Fantastique"));
DOCS.push(new Document(TYPE.PNJ,"Mister President",4, "Schtroumph grognon"));
DOCS.push(new Document(TYPE.PNJ,"Spider Boy",5, "Ca colle"));

DOCS.push(new Document(TYPE.PISTE,"Les Broutons",1, "Blache"));
DOCS.push(new Document(TYPE.PISTE,"Le portail",2, "blouche"));
DOCS.push(new Document(TYPE.PISTE,"La forêt",3, "Falamase"));
DOCS.push(new Document(TYPE.PISTE,"Qui est Caïus ?",4, "Heptagone"));

DOCS.push(new Document(TYPE.SCENE,"Démarrage",1, "C'est le départ"));
DOCS.push(new Document(TYPE.SCENE,"Ensuite",2, "Scène 2"));
DOCS.push(new Document(TYPE.SCENE,"Démarrage",3, "Fouchtra"));
DOCS.push(new Document(TYPE.SCENE,"Démarrage",4, "Parama", STATUS.ALTEREE));
DOCS.push(new Document(TYPE.SCENE,"Démarrage",5, "Surprise à la Cour du roi Loth", STATUS.INTERROMPUE, "blahuche"));


myconsole.log(createUUID());
myconsole.log(DOCS);

function refresh() {
    writeInZone('zoneliste',DOCS.toHTML());
}

refresh();



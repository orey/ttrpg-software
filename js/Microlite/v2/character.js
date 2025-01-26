"use strict";

class GraphicalCharacter {
    constructor(id){
        this.id = id;
        this.elem = document.getElementById(id);
        this.pubsub = null;
        this.elem.innerHTML = 
`<h2>Character Creation</h2>
<label for="name">Name:</label>
<input type="text" id="name" name="name">
<br><br>
<table>
  <tr>
    <td><label for="str">STR:</label></td>
    <td><input type="number" class="stat" id="str" name="str" min="3" onchange="statChange('str');"></td>
    <td></td>
    <td><label for="strbonus">STR Bonus:</label></td>
    <td><input type="number" class="stat" id="strbonus" name="strbonus" readonly></td>
  </tr>
  <tr>
    <td><label for="dex">DEX:</label></td>
    <td><input type="number" class="stat" id="dex" name="dex" min="3" onchange="statChange('dex');"></td>
    <td></td>
    <td><label for="dexbonus">DEX Bonus:</label></td>
    <td><input type="number" class="stat" id="dexbonus" name="dexbonus" readonly></td>
  </tr>
  <tr>
    <td><label for="mind">MIND:</label></td>
    <td><input type="number" class="stat" id="mind" name="mind" min="3" onchange="statChange('mind');"></td>
    <td></td>
    <td><label for="dexbonus">MIND Bonus:</label></td>
    <td><input type="number" class="stat" id="mindbonus" name="mindbonus" readonly></td>
  </tr>
</table>
<br/>
<div>
  <input type="radio" id="jsondisplay" name="fav_display" value="jsondisplay" checked="checked">
  <label for="html">JSON</label>
  <input type="radio" id="textdisplay" name="fav_display" value="textdisplay">
  <label for="css">Text</label><br>
  <button onclick="displayCharacter()">Display Character</button>
</div>
<hr/>
<div id="character-display" name="character-display syle="max-width: 50px;"></div>
<hr/>

<button onclick="saveCharacter()">Save Character</button>
<button onclick="clearCharacter()">Clear Character</button>

<p>Existing Character:<br/>
  <input type="file" id="file-input" accept=".json"><br/>
  <button onclick="loadCharacter()">Load Character</button>
</p>`;
    }
    destroy() {
        this.pubsub.publish("CONTROL",
                            {message: "GraphicalCharacter " + String(this.id) + " about to be destroyed..."});
        this.elem.innerHTML = "";
    }
}

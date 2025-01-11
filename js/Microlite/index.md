# Index

## JS

### Utils

* VERBOSE
*  myconsole
* class PubSub
* const PUBSUB of type PubSub

### Graphical functions

For the dice zone

* diceThrow
* diceClear

For the stats

* throwFourKeepThreeGraphical
* caracClear
* assignTo

For the character zone

* statChange
* calculateStatBonuses
* generateCompleteCharacter
* displayCharacter
* displayCharacterFromObject
* saveCharacter
* loadCharacter
* clearCharacter
* characterToHtml

### Non graphical functions

Dice section

* rollDie
* roll (équivalent de rollCombi)
* notANumber - utilitaire de log
* parseDice
* throwFourKeepThree
* statBonus

Game section

* statBonus
* class Character - this class can serialize itself in JSON and in html
* JsonSerializer: takes an instance of Character and generates the json for the file saving. If true is provided, adds a '\n' at the end of each line.
* JsonReader: returns an instance of Character

Name section

* CONSONNES
* VOYELLES
* getLetter
* capFirst
* generate
* generateNom

#!/bin/sh
echo "------------------------------"
echo "dice.js"
node -e 'require("./dice.js").test()'
echo "------------------------------"
echo "rdfjs.js"
node -e 'require("./rdfjs.js").test()'



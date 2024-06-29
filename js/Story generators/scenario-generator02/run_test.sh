#!/bin/sh
echo "generator01.js"
node -e 'require("./generator02.js").test()'


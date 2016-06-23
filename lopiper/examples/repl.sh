#!/bin/bash

println "var scm = require('./js_std.js');"

evu=phantomjs
#evu="node -i"
#evu=./scheme-electron/run.py
#(scm.screenshot "http://github.com" "test.png")

./_repl_helper.sh | $evu

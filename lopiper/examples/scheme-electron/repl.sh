#!/bin/bash

#evu=phantomjs
#evu="node -i"
evu=./run.py
#(scm.screenshot "http://github.com" "test.png")

./_repl_helper.sh | $evu

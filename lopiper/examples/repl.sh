#!/bin/bash

evu=phantomjs
#evu=node
#(scm.screenshot "http://github.com" "test.png")

./_repl_helper.sh | $evu

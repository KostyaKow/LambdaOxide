#!/bin/bash

cat js_std.js

evalu=phantomjs
#evalu=node

while true; do
   guile ir.scm 2>/tmp/bad_guile.log | $evalu
done

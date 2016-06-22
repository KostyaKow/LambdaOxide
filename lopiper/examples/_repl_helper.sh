#!/bin/bash

echo "var scm = require('./js_std');"

while true; do
   guile ir.scm 2>/tmp/bad_guile.log
done

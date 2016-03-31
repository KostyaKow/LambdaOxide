#!/bin/bash

#TODO: kkleft change this to crates or at least makefiles lol
rustc --crate-name list --crate-type lib list.rs
rustc --crate-name utils --crate-type lib utils.rs
rustc -L . --crate-name err --crate-type lib err.rs
rustc -L . --crate-name types --crate-type lib types.rs
rustc -L . --crate-name lexer --crate-type lib lexer.rs
rustc -L . --crate-name parser --crate-type lib parser.rs
rustc -L . lisp.rs


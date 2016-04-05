Requires 1.9 rust nightlies.

```scheme
$ cargo build

$ cargo run
**> (+ 3 5)
8
**> (define f (lambda (y z) (- y z z)))
success
**> (f 3 5)
-7
**> (define l (cons 1 (cons 2 nil)))
success
**>(car l)
1
**>(cadr l)
2
**>(define l2 (map (lambda (x) (+ x 20)) l))
success
**>(car l2)
21
**>(define check (lambda (lst) (if (null? lst) "empty" "non-empty")))
success
**>(check l)
non-empty
**>(check (cdr (cdr l)))
empty
```

TODO:
   -implement c-types-like ffi
   -implement macro system
   -comparison
   -floats
   -quote
   -add env to print_sexps
   -unit tests with cargo
   -evaluating naked expression like 8 or "blah" doesn't work with intepreter
   -loading multiline statements
   -move built-in functions to separate file
   -make matching arguments easier for build-ins
   -build-in cons
   -convert sexps to use vec (??? maybe)
   -implement variable-number arguments with *
   -multiple expressions in lambda, etc.

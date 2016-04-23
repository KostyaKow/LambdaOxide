Requires 1.9 rust nightlies.

```scheme
$ git clone https://github.com/KostyaKow/RustyParenthesis && cd RustyParenthesis

$ cargo build

$ cargo run
**> 8
8
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

- TODO
   - [ ] implement c-types-like ffi (using libffi?)
   - [ ] implement macro system
   - [ ] comparison
   - [x] floats
   - [ ] quote
   - [ ] add env to print_sexps
   - [ ] unit tests with cargo
      - [x] lexer test
      - [ ] parsing tests
      - [x] church numerals
      - [ ] standard library
      - [ ] test for expected failures
      - [x] test sum with ints and floats
      - [ ] test other operators with numberes
   - [ ] switch Sexps to not use Cons and do it manually
   - [x] evaluating naked expression like 8 or "blah" doesn't work with intepreter
   - [x] loading multiline statements
   - [ ] move built-in functions to separate file
   - [x] make matching arguments easier for build-ins
   - [ ] build-in cons
   - [ ] convert sexps to use vec (??? maybe)
   - [ ] switch form list::Cons to list::List
   - [ ] implement variable-number arguments with * or .
   - [ ] multiple expressions in lambda, etc.
   - [ ] convert built-ins to only handle 2 arguments, and make wrappers in lisp
   - [ ] investigate why multi-line map definition hangs in core.lam
   - [x] Fib example
   - [ ] ; comment
      - [x] ; at beggining of line
      - [ ] ; after code
   - [ ] multi-line comments
   - [ ] if we get err in file while interpreting, abort and report error
   - [ ] comparison operator other than = (>, <)
   - [ ] logic operators
      - [ ] not
      - [ ] and
      - [ ] or
   - [ ] make embeddable library
      - [x] separate lib and bin


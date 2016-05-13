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
   - [ ] if and or shortcircuit
   - [ ] rename main.rs
   - [ ] grpahics engine TODO's
      - [ ] quad tree for engine
      - [ ] https://en.wikipedia.org/wiki/Logo_(programming_language)
   - [ ] apply (apply + (list 1 2 3))
      - [x] crappy/slow lisp implementation that only supports 3 arguments
   - [ ] explicit type casting for strings/ints/floats
   - [ ] add prompt or input
   - [ ] cond macro
   - [ ] add built-in list (function), is_string, is_float, is_int, is_func
   - [ ] set (so colorfulcircle.lo gets list of triangles).
   - [x] implement print
      - [x] implement print for strings
      - [x] fix print for everything else
   - [ ] change the way we store sexps
      - [x] investigate new Sexps style
      - [ ] build-in cons
      - [ ] modify cells
   - [ ] check argument types and number for build-ins
   - [ ] remove check at beginning of every build-in functions (maybe make a wrapper)
   - [x] int-float coarcing
   - [x] adding strings
   - [ ] make matching arguments easier for build-ins
      - [x] extract float
      - [ ] extract int
      - [x] extract string
   - [ ] make argument extractor for build-in types ```rust struct ExtractedArgs { strings : Vec<Strings>, floats : Vec<f64>, exps : Vec<Sexps> } fn extract_args(args : Sexps, format : Vec<String>) -> ExtractedArgs;```
   - [ ] implement c-types-like ffi (using libffi?)
   - [ ] implement macro system
   - [ ] strings
      - [x] make strings addable. Cast all arguments to string if one of arguments to + is string
      - [ ] string indexing, slices, etc.
   - [x] other number functions
      - [x] mul
      - [x] div
   - [ ] logic operators
      - [x] and, and or take variable number of arguments
      - [x] not
      - [x] and
      - [x] or
         - [ ] short-circuit
   - [x] comparison
      - [x] =
      - [x] <
      - [x] >
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
   - [ ] repl and syntax highlights
      - [x] loading multiline statements
      - [ ] repl history
   - [ ] move built-in functions to separate file
   - [ ] build-in cons
   - [ ] convert sexps to use vec (??? maybe)
   - [ ] switch form list::Cons to list::List
   - [ ] implement variable-number arguments with * or .
   - [ ] multiple expressions in lambda, etc. (we can just use do for now)
   - [ ] convert built-ins to only handle 2 arguments, and make wrappers in lisp
   - [x] investigate why multi-line map definition hangs in core.lam
   - [x] Fib example
   - [ ] ; comment
      - [x] ; at beggining of line
      - [ ] ; after code
   - [ ] multi-line comments
   - [ ] if we get err in file while interpreting, abort and report error
   - [x] do
   - [x] sleep
   - [ ] when testing multi-line expressions (when missing parenthesis), we can concatentate Lexeme Vectors instead of concating strings and re-lexing whole string.
   - [x] make embeddable library
      - [x] separate lib and bin
      - [ ] figure out a way to modify external environment from language
   - [ ] optimize
      - [ ] compiler
      - [ ] have both vector and cons

Differences with standard/SICP Scheme:
   - cannot use special form `(define  (func arg1 arg2) (...))`, have to use `(define func (lambda (arg1 arg2) (...)))`
   - "begin" is called "do"
   - Definition cannot contain multiple expressions
   - no let binding
   - no cond
   - no macro system
   - no quote '
   - no set! (page 220)
   - print instead of display
   - null?, nil

TODO soon:
   - [ ] `(do (define x (lambda () (print "hi")))); x` x is still defined in this scope
   - [ ] fix list-ref and make it built-in and check performance for tictactoe
   - [ ] remove extr tables from Callable
   - [ ] tail call optimization
   - [ ] stop using err for nil checks and sym table lookups. Panic on err or at least put back drop for exp




(lambda f (a b) (+ (+ 3 5) (+ a b)))
(lambda printf (x))
(lambda main () (print (f 30 4)))

cat output > test.bc
//http://stackoverflow.com/questions/29180737/how-to-generate-executable-from-llvm-ir
llc -march=x86-64 test.bc -o test.s
//as test.s
gcc test.s -o a.out

TODO:
- [ ] JIT/llvm
   - [x] in progress
- [ ] do something like rustc --explain E0123 with my error codes
- [ ] implement ErrCode::{MisformedFloat, MisformedInt, BadChar}
   - [x] MisFormedNum
- [ ] add single character ''
- [ ] stack trace display
- [x] Make new generic library/crate utils with generic rust stuff.
- [x] check lexer lexer
- [ ] check utils code
- [ ] check parser code
- [ ] don't overoptimize lexer and parser because they don't run often
- [ ] Unit test for every time
- [ ] comments in middle of a line
- [ ] multi-line comments
- [ ] lexer errors for when something begins with a number, but not a number (0sdf or -32fds or 32..12 or 4-3)
- [ ] String::from("blah") or "blah".to_string()?
- [ ] for stack trace, need to implement lambda to work better with define'd names
- [ ] fix quotes for '(+ 3 5) (currently quotes only work for stuff like 'sdf)
- [ ] unit tests
- [ ] performance tests
- [ ] re-write symbol table and make it more robust and faster
- [ ] macros
- [ ] start investigating compiler
- [ ] better error debug output
- [ ] set! set-cell! set-car!
- [ ] possibly for/while/loop so we don't kill the stack
- [ ] support classic scheme define syntax (define (f x) exp exp)
- [ ] let statemnts
- [ ] cond
- [ ] garbage collection
- [ ] low-level fast operators (+ 4 "blah") (+' 3 3) //+' sum prime optimized version
- [ ] stack tracing
- [ ] tail call optimization
-

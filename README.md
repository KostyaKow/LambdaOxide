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
   add env to print_sexps



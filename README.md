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
**> (define c (cons 1 (cons 2 nil)))
success
**>(car c)
1
**>(cadr c)
2
**>(if (null? c) "empty" "non-empty")
non-empty
```

TODO:
   add env to print_sexps



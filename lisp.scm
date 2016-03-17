(define (create-table scope sexps binds)
   (buildin))
(define (match x conds) (buildin))

(define (if x y z) ()
   (match x (true (eval y) (false (eval z))



(define (parse lex-vec)
   (let synt-tree (new Tree)
   (for (l in lex-vec)
      ())))

"
;syntax
;;;;type system
types       : values => [typeclass]
   Nil      : nil => [eq, cmp, add, sub, mul, div, not, boolish]
   Num      : .., -1000.1, .., -1, 0, 1, 10000, .. => [eq, cmp, add, sub, mul, div, not, boolish]
   Bool     : true, false => [eq, not]
   Str      : \" \" => [not, eq, add, sub, mul, boolish]
   Char     : ' ' => [not, eq, cmp, mul, add, boolish]
   List     : cons, nil => [not, eq???, cmp???, mul, add, boolish]

typeclasses : [methods] => [dependencies]
   boolish  : [boolify]
   not      : [!] => [Boolish]
   eq       : [=, !=]
   cmp      : [>, <, >=, <=] => [eq]
   add      : [+]
   sub      : [-]
   mul      : [*]
   div      : [/]

notes
   not typeclass casts to bool
   nil is typeclass and value
   list and nil types share value nil
   short-circuit or and and
   macros
      (macro (f a b) (eval a) (eval b))
      (macro (f) (car _))
   parameter expansion
      (define (f) (display _))

build-in macros
   (if (x) (y) (z))
   (lambda (x) (exp))
   (lambda-rec name (x) (exp))
   ;;;;;replaced with macro (lambda* exp) ;all arguments are passed as list of sexps
   ;for defining macro
   (macro name (exp))
   (invoke-normal macro-name) ;invoke macro like a function
   (def id binding)
   (display x) ;display a variable
   (let ((x exp) (y exp)) (exp))
   _ ;function argument as a list
   (eval x)
   ;;(return x) ;return early ?????

build-in functions
   (cons a b)
   (car x)
   (cdr x)
   (list x y z)
   (apply f (x y z))
   =, >, <
   +, -, *, /
   (cond ((> a b) 'greater)
         (else 'blah))

library macros
   (def test (lambda* (print (car _))))
   ;(test 3) => (print 3)
   ;(test (+ 3 5)) => (print '(+ 3 5))

   ;(macro name (exp))
   ;(name (blah test))
   ;(name "eval" blah)
   ;(name "eval-1st" blah blah) ???
   (def macro
      (lambda*
         (cond ((null? _) (syntax-error "macro needs parameters")
               (else (def (car _) (
               ((eq? (car _) "eval" )
                (

   (macro define ())
   ;(define (f x y z) (exp))

   (macro or
      (cond ((null? _) false)
            ((= (car _) true) true)
            (else (or (cdr )))))

   or, and


standard library (t= means typeclass of given variable):
   ;boolify :: (boolish a) => a -> Bool
   (define (boolify x) (if (x) true false)
   ;! :: (boolish a) => a -> Bool
   (define (! x) (if (boolify x) false true))
   ;!= :: (eq a, eq b) => a -> b -> Bool
   (define (!= a b) (not (= a b)))
   ;>= :: (cmp a, cmp b) => a -> b -> Bool
   (define (>= a b) (or (> a b) (= a b)))
   :<= :: (cmp a, cmp b) => a -> b -> Bool
   (define (<= a b) (or (< a b) (= a b)))
   ;begin :: _ -> a
   (define (begin (_)
      (fold (lambda (last acc) last) (_) nil)
   ;null :: (eq t) => t -> Bool
   (define (null? x) (= x nil))
   ;map :: (a -> b) -> Lst a -> Lst b
   (define (map f lst)
      (if (null? lst)
         nil
         (cons (f (car lst)) (map f (cdr lst)))))
   ;filter :: (a -> Bool) -> Lst a -> Lst a
   (define (filter f lst)
      (if (null? lst)
         nil
         (if (f (car lst))
            (cons (car lst) (filter f (cdr lst)))
            (filter f (cdr lst)))))
   ;fold :: (a -> b -> b) -> Lst a -> b -> b
   (define (fold f lst init)
      (if (null lst)
         init
         (fold f (cdr lst) (f (car lst) init))))
   ;list :: _ -> Lst a
   (define (list _)
      (fold cons (_) nil))


"

;#lang racket

;we could use this in lisp repl loop to define expression
;(define exp (read))
;stdin:
;(+ 3 5 (- 3 4)
;)
;can also convert from string
;(define exp-str "(define (f x y) (+ x y 4 2))")
;(define exp (str->exp exp-str))

;but for now we'll test with hardcoded exp
;(define exp-lisp '(define x (+ 3 5)))
(define exp-lisp '(define (f x y) (+ x y 4 2)))

(define (sym-lst-to-py-lst exp)
   (if (null? exp)
      ""
      (string-append
       (if (symbol? (car exp)) (symbol->string (car exp)) (number->string (car exp)))
       (if (null? (cdr exp)) "" ", ")
       (sym-lst-to-py-lst (cdr exp)))))

;for defines
(define (gen-def exp tab)
   (if (null? exp) "None" (begin
      (let* ((name-args (car exp)) (body (cdr exp)))
         (cond
            ((string? name-args)
             (gen-assign name-args body))
            (else
             (string-append "def "
                (symbol->string (car name-args))
                "("
                (sym-lst-to-py-lst (cdr name-args))
                "):\n"
                (to-py (cdr exp) (+ tab 1)))))))))

;generate python =
(define (gen-assign name exp tab)
   (string-append (symbol->string name) " = " (to-py exp tab)))

(define (is-ass? exp)
   (and (> (length exp) 2)
        (or (eq? (car exp) 'define) (eq? (car exp) 'def))
        (symbol? (cadr exp))))
(define (is-call? exp) (> (length exp) 0))
(define (is-def? exp)
   (and
      (> (length exp) 2)
      (or (eq? (car exp) 'define) (eq? (car exp) 'def))
      (not (symbol? (cadr exp)))))

(define (gen-tab-spaces ntabs)
   (if (= ntabs 0)
      ""
      (string-append "   " (gen-tab-spaces (- ntabs 1)))))

(define (get-func-name s)
   (cond ((eq? s '+) "sum")
         ((eq? s '-) "diff")
         (else (symbol->string s))))

(define (gen-call exp tab)
   (string-append (gen-tab-spaces tab)
                  (get-func-name (caar exp)) "("
                  (sym-lst-to-py-lst (cdar exp)) ")"))

(define (to-py exp tab)
   (define (helper exp)
      (cond
         ((null? exp) '())
         ((is-def? exp) (gen-def (cdr exp) tab))
         ((is-ass? exp) (gen-assign (cadr exp) (cddr exp) tab))
         ((is-call? exp) (gen-call exp tab))
         (else "null"))) ;also need lambda
   (helper exp))


(display (to-py exp-lisp 0))


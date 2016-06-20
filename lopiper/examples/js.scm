;#lang racket

;we could use this in lisp repl loop to define expression
;(define exp (read))
;stdin:
;(+ 3 5 (- 3 4)
;)
;can also convert from string
;(define exp-str "(define (f x y) (+ x y 4 2))")
;(define exp (str->exp exp-str))
;
;but for now we'll test with hardcoded exp
;
;(define (sym-lst-to-py-lst exp)
;   (if (null? exp)
;      ""
;      (string-append
;       (if (symbol? (car exp)) (symbol->string (car exp)) (number->string (car exp)))
;       (if (null? (cdr exp)) "" ", ")
;       (sym-lst-to-py-lst (cdr exp)))))
;
;;for defines
;(define (gen-def exp tab)
;   (if (null? exp) "None" (begin
;      (let* ((name-args (car exp)) (body (cdr exp)))
;         (cond
;            ((string? name-args)
;             (gen-assign name-args body))
;            (else
;             (string-append "def "
;                (symbol->string (car name-args))
;                "("
;                (sym-lst-to-py-lst (cdr name-args))
;                "):\n"
;                (to-py (cdr exp) (+ tab 1)))))))))
;
;;generate python =
;(define (gen-assign name exp tab)
;   (string-append (symbol->string name) " = " (to-py exp tab)))
;
;(define (gen-tab-spaces ntabs)
;   (if (= ntabs 0)
;      ""
;      (string-append "   " (gen-tab-spaces (- ntabs 1)))))
;
;(define (get-func-name s)
;   (cond ((eq? s '+) "sum")
;         ((eq? s '-) "diff")
;         (else (symbol->string s))))
;
;(define (gen-call exp tab)
;   (string-append (gen-tab-spaces tab)
;                  (get-func-name (caar exp)) "("
;                  (sym-lst-to-py-lst (cdar exp)) ")"))

(define (ir-null? exp) (null? exp))
(define (ir-num? exp) (number? exp))
(define (ir-str? exp) (string? exp))
(define (ir-sym? exp) (symbol? exp))
(define (ir-cons? exp) (pair? exp))

(define (ir-gen-null) 'ir-null)
(define (ir-gen-num n) `(ir-num ,n))
(define (ir-gen-str s) `(ir-str ,s))
(define (ir-gen-sym s) `(ir-sym ,s))

(define (ir-gen-err msg) '(ir-err ,msg))

(define (ir-get-tmp-name) "test")
(define (ir-store name val) pass)

(define (gen-ir-cons exp nest)
   (define (is-lamb? exp) pass)
   (define (is-if? exp) pass)
   (define (is-cond? exp) pass)
   (define (is-def? exp)
      (and (> (length exp) 2)
           (or (eq? (car exp) 'define) (eq? (car exp) 'def))))
   (define (is-def-ass? exp)
      (and (is-def? exp) (symbol? (cadr exp))))
   (define (is-def-func? exp)
      (and (is-def? exp) (pair (cadr exp)) (not (symbol? (cadr exp)))))

   (define (is-call? exp)
      (and (pair? exp) (> (length exp) 0)))

   (let ((name-internal (car exp))
         (name
            (cond ((ir-cons? name-internal)
                   (begin (ir-store (ir-get-tmp-name) (exp-ir name-internal)
         (args (cdr exp))
      (if (ir-cons? name) (ir-store (ir-get-tmp-name) (exp->ir name))
         ((is-def-func? exp) (gen-def (cdr exp) nest))
         ((is-def-ass? exp) (gen-assign (cadr exp) (cddr exp) nest))
         ((is-call? exp) (gen-call exp nest))))


(define (exp->ir exp nest)
   (cond
      ((ir-null? exp) (ir-gen-null))
      ((ir-num? exp) (ir-gen-num exp))
      ((ir-str? exp) (ir-gen-str exp))
      ((ir-sym? exp) (ir-gen-sym exp))
      ((ir-cons? exp) (gen-ir-cons exp nest))
      (else (ir-gen-err "exp->ir call else called"))))

(define (is-ir-null? ir) (eq? ir 'null))

(define (ir->js ir) "")

(define exp-lisp '((define x (+ 3 5)) (define (f x y) (+ x y 4 2))))

(display (ir->js (map exp->ir exp-lisp)))

;(display (to-ir exp-lisp 0))




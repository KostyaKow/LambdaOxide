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

;for exp->ir
(define (ir-null? exp) (null? exp))
(define (ir-num? exp) (number? exp))
(define (ir-str? exp) (string? exp))
(define (ir-sym? exp) (symbol? exp))
(define (ir-cons? exp) (pair? exp))

(define (ir-gen-null) (ir-tag 'null)) ;'ir-null)
(define (ir-gen-num n) (cons (ir-tag 'num) n))
(define (ir-gen-str s) (cons (ir-tag 'str) s))
(define (ir-gen-sym s) (cons (ir-tag 'sym) s))
;end exp->ir

;for generic helpers
(define (to-string exp)
   (cond ((symbol? exp) (symbol->string exp))
         ((number? exp) (number->string exp))
         (else "to-string: unknown type to convert to string")))

(define (ir-gen-err msg) (cons (ir-tag 'err) msg))

;(define (ir-get-tmp-name) (string-append "tmp" (number->string (random 1000))))
(define curr-tmp 0)
(define (ir-get-tmp-name)
   (set! curr-tmp (+ curr-tmp 1))
   (string-append "tmp" (number->string curr-tmp)))

(define (ir-store name val)
   `(,(ir-tag 'assign) (,(ir-tag 'sym) ,name) ,(exp->ir val)))

(define (ir-tag tag)
   (cons 'ir tag))
(define (ir-tag? x)
   (and (ir-cons? x) (ir-cons? (car x)) (eq? (caar x) 'ir)))

;end generic helpers

;for gen-ir-cons
(define (ir-def? exp)
   (and (> (length exp) 2)
        (or (eq? (car exp) 'define) (eq? (car exp) 'def))))

(define (ir-def-func? exp)
   (and (ir-def? exp) (pair? (cadr exp)) (not (symbol? (cadr exp)))))
(define (ir-def-ass? exp)
   (and (ir-def? exp) (symbol? (cadr exp))))

(define (ir-lamb? exp) (eq? (car exp) 'lambda)) ;check format
(define (ir-if? exp) (eq? (car exp) 'if)) ;check more stuff here
(define (ir-cond? exp) (eq? (car exp) 'cond)) ;check this stuff
(define (ir-call? exp)
   (and (pair? exp) (> (length exp) 0)))

(define (ir-gen-if exp)
   `(,(ir-tag 'if) ,(cadr exp) ,(cddr exp)))
(define (ir-gen-cond exp)
   (ir-gen-err "cond not supported yet"))
(define (ir-gen-call name args)
   `(,(ir-tag 'call) ,name ,(map exp->ir args)))

(define (ir-gen-lambda args body)
   `(,(ir-tag 'lambda) ,args ,(exp->ir body)))

;end gen-ir-cons

(define (gen-ir-cons exp)

   (define (get-func-name exp)
      (let ((test-name (car exp)))
         (cond ((ir-sym? test-name) test-name)
               ((ir-cons? test-name)
                (let ((tmp-name (ir-get-tmp-name)))
                  (ir-store tmp-name (exp->ir test-name))
                  (string->symbol tmp-name)))
               (else ir-gen-err (string-append "bad function name: " (to-string test-name))))))

   (let ((args (cdr exp))
         (name (get-func-name exp)))
      (cond
         ((ir-def-func? exp) (ir-store (caar args) (ir-gen-lambda (cdr (car args)) (cadr args))))
         ((ir-lamb? exp) (ir-gen-lambda (car args) (cdr args)))
         ((ir-def-ass? exp) (ir-store (car args) (cdr args)))
         ((ir-if? exp) (ir-gen-if exp))
         ((ir-cond? exp) (ir-gen-cond exp))
         ((ir-call? exp) (ir-gen-call (car exp) (cdr exp)))
         (else (ir-gen-err "bad gen-ir-cons cond")))))

(define (exp->ir exp)
   (cond
      ((ir-tag? exp) exp)
      ((ir-null? exp) (ir-gen-null))
      ((ir-num? exp) (ir-gen-num exp))
      ((ir-str? exp) (ir-gen-str exp))
      ((ir-sym? exp) (ir-gen-sym exp))
      ((ir-cons? exp) (gen-ir-cons exp)) ;(cons 'block (gen-ir-cons exp)))
      (else (ir-gen-err "exp->ir call else called"))))

(define (tag-remove-ir-rec e)
   (if (and (ir-cons? e) (ir-cons? (car e)) (eq? (caar e) 'ir))
      (cons (cdar e)
            (if (ir-cons? (cdr e))
               (map (lambda (x) (if (ir-cons? x) (map tag-remove-ir-rec x) x)) (cdr e))
               (cdr e)))
            ;(map (lambda (x) (if (ir-cons? x) (map tag-remove-ir-rec x) x))
            ;     (cdr e)))
      e))

(define (runner exp)
   (tag-remove-ir-rec `(,(ir-tag 'block) ,(map exp->ir exp))))

(define (ir->js ir) ir)

(define exp-lisp '((define x (+ 3 5)) (define (f x y) (+ x y 4 2))))
;(define exp-lisp '((define x (+ 3 5))))

;(display (ir->js (exp->ir exp-lisp)))
;(display "\n")


(define (print-ir ir)
   (map (lambda (x)
          (display x)
          (display "\n"))
        ir)
   (display "\n"))

(print-ir (runner exp-lisp))

;(display (to-ir exp-lisp 0))




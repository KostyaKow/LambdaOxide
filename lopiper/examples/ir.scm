;TODO:
;convert all blocks to lambda's with 0 args
;function call and assignment statements both append ;. So var x = scm_sum(3, 5);; has 2 semicolons

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

(define (make-tabs ntabs)
   (if (= ntabs 0)
      ""
      (string-append "   " (make-tabs (- ntabs 1)))))

(define (lst->comma-str exp)
   (if (null? exp)
      ""
      (string-append
       (cond ((symbol? (car exp))
              (symbol->string (car exp)))
             ((number? (car exp))
              (number->string (car exp)))
             (else (car exp))) ;lst->comma-str ???
       (if (null? (cdr exp)) "" ", ")
       (lst->comma-str (cdr exp)))))

(define fold
   (lambda (combine init lst)
         (if (null? lst)
            init
            (fold combine (combine (car lst) init) (cdr lst)))))
(define (map f lst)
   (if (null? lst)
      '()
      (if (not (pair? lst))
         (cons (f lst) '())
         (cons (f (car lst)) (map f (cdr lst))))))

;for generic helpers
(define (to-string exp)
   (cond ((symbol? exp) (symbol->string exp))
         ((number? exp) (number->string exp))
         (else "to-string: unknown type to convert to string")))

(define (ir-gen-err msg) (list (ir-tag 'err) msg))

;(define (ir-get-tmp-name) (string-append "tmp" (number->string (random 1000))))
(define curr-tmp 0)
(define (ir-get-tmp-name)
   (set! curr-tmp (+ curr-tmp 1))
   (string-append "tmp" (number->string curr-tmp)))

(define (ir-store name val)
;   `(,(ir-tag 'assign) (,(ir-tag 'sym) ,name) ,(exp->ir val)))
   (list (ir-tag 'assign) (exp->ir name) val))

(define (ir-tag tag)
   (cons 'ir tag))
(define (ir-tag? x)
   (and (pair? x) (pair? (car x)) (eq? (caar x) 'ir)))

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
(define (ir-begin? exp) (and (pair? exp) (eq? (car exp) 'begin)))
(define (ir-call? exp)
   (and (pair? exp) (> (length exp) 0)))

(define (ir-gen-if exp)
   (list (ir-tag 'if) (exp->ir (cadr exp)) (exp->ir (caddr exp)) (exp->ir (cadddr exp))))
(define (ir-gen-cond exp)
   (ir-gen-err "cond not supported yet"))
(define (ir-gen-call name args)
   (list (ir-tag 'call) (exp->ir name) (map exp->ir args)))

(define (ir-gen-lambda args body)
;   `(,(ir-tag 'lambda) ,(map exp->ir args) ,(exp->ir body)))
;   (list (ir-tag 'lambda) (map exp->ir args) (map exp->ir body)))
   ;(display body)
   ;(list (ir-tag 'lambda) (map exp->ir args) (list (ir-tag 'block) (map (lambda (x) (exp->ir x)) body))))
   (list (ir-tag 'lambda) (map exp->ir args) (list (map (lambda (x) (exp->ir x)) body))))

(define (ir-gen-begin exp)
   (list (ir-tag 'block) (map exp->ir exp)))

;end gen-ir-cons

(define (gen-ir-cons exp)

   (define (get-func-name exp)
      (let ((test-name (car exp)))
         (cond ((symbol? test-name) test-name)
               ((pair? test-name)
                (let ((tmp-name (ir-get-tmp-name)))
                  (ir-store tmp-name (exp->ir test-name))
                  (string->symbol tmp-name)))
               (else ir-gen-err (string-append "bad function name: " (to-string test-name))))))

   (let ((args (cdr exp))
         (name (get-func-name exp)))
      (cond
         ((ir-def-func? exp) (ir-store (caar args) (ir-gen-lambda (cdr (car args)) (cdr args)))) ;last was (cadr args)
         ((ir-lamb? exp) (ir-gen-lambda (car args) (cdr args)))
         ((ir-def-ass? exp) (ir-store (car args) (exp->ir (cadr args))))
         ((ir-begin? exp) (ir-gen-begin args)) ;exp or args???
         ((ir-if? exp) (ir-gen-if exp))
         ((ir-cond? exp) (ir-gen-cond exp))
         ((ir-call? exp) (ir-gen-call (car exp) (cdr exp)))
         (else (ir-gen-err "bad gen-ir-cons cond")))))

;for exp->ir
(define (ir-gen-null) (ir-tag 'null)) ;'ir-null)
(define (ir-gen-num n) (list (ir-tag 'num) n))
(define (ir-gen-str s) (list (ir-tag 'str) s))
(define (ir-gen-sym s) (list (ir-tag 'sym) s))
;end exp->ir

(define (exp->ir exp)
   (cond
      ((ir-tag? exp) exp)
      ((null? exp) (ir-gen-null))
      ((number? exp) (ir-gen-num exp))
      ((string? exp) (ir-gen-str exp))
      ((symbol? exp) (ir-gen-sym exp))
      ((pair? exp) (gen-ir-cons exp)) ;(cons 'block (gen-ir-cons exp)))
      (else (ir-gen-err "exp->ir call else called"))))

(define (is-tag? e) (and (pair? e) (pair? (car e)) (eq? (caar e) 'ir)))

(define (tag-remove-ir-rec e)
   (if (pair? e)
      (if (eq? (car e) 'ir)
         (if (pair? (cdr e)) (map tag-remove-ir-rec (cdr e)) (cdr e))
         (cons (if (is-tag? e) (cdar e) (map tag-remove-ir-rec (car e)))
               (map tag-remove-ir-rec (cdr e))))
      e))

(define (runner exp)
   (list (ir-tag 'block) (map exp->ir exp)))

(define (print-ir ir nest)
   (if (pair? ir)
      (if (eq? (car ir) 'block)
         (begin
            (display "[")
            (map (lambda (x) (print-ir x (+ nest 1))) (cdr ir))
            (display "]"))
         (begin
            (display "(")
            (map (lambda (x) (display "(") (print-ir x nest) (display ")")) ir)
            (display ")")))
      (begin (display ir) (display " "))))

(define (print-ir1 ir nest)
   (display (car ir)) (display "\n")
   (map (lambda (x)
          (display x)
          (display "\n"))
        (cadr ir)) ;(cadr ir)) ;to display block, just use ir
   (display "\n"))

;(define exp-lisp '((define (f x y) (- 3 10) (+ x 4 2))))
;(display (ir->js (exp->ir exp-lisp)))
(define exp-lisp '((define x (+ 3 5)) (define (f x y) (- 3 10) (+ x y 4 2)))) ;works
;(define exp-lisp '((define x (+ 3 5)) (define (f x y) (- 3 10) (+ x y 4 2))) (f 3 5)) ;broken

;(define exp-lisp '((begin (+ 3 2) (- 2 16)) (def (f x) (+ x 5)) (def y 10) (if (> (f y) 15) (console.log "greater") (console.log "smaller"))))
;(define exp-lisp '((def y 15))) ;broken
;(define exp-lisp '((+ 3 (- 5 10))))
;(define exp-lisp '((define x (+ 3 5))))
;(display (to-ir exp-lisp 0))

;(print-ir1 (runner exp-lisp) 0)
;(print-ir1 (tag-remove-ir-rec (runner exp-lisp)) 0)

(define (get-type ir)
   (if (and (pair? ir) (pair? (car ir)) (eq? (caar ir) 'ir))
      (cdar ir)
      (begin
         (display "bad ir: ")
         (display ir)
         'bad)))

(define (get-data ir)
   (if (and (pair? ir) (pair? (car ir)) (eq? (caar ir) 'ir))
      (cdr ir)
      'bad))

(define (is-type? ir type) (eq? (get-type ir) type))

(define (gen-js-blk data nest)
   (define newl "") ;newl "\n"
   (define semi ";")
   (string-append (make-tabs nest) "(function () {" newl (fold string-append "" (map (lambda (x) (string-append (make-tabs (+ nest 1)) (ir->js x nest))) (reverse data))) newl "})()" semi newl))

(define (iden x) x)

(define (lookup-func name)
   (cond ((string=? name "+") "scm.sum")
         ((string=? name "-") "scm.diff")
         ((string=? name "*") "scm.mul")
         (else name)))

(define (gen-js-if data nest) "if a else b")

(define (gen-js-lambda data nest)
   ;"function () { /*lambda*/ }\n"
   ;(display "KK")
   ;(display (cadr data))
   ;(display "KK-----------\n")
   (define newl "");(define newl "\n")
   (string-append
      (make-tabs nest)
      "(function ("
      (lst->comma-str (map (lambda (x) (cadr x)) (car data)))
      ") {var ret = null;"
      (fold string-append ""
            (map (lambda (x) (string-append newl (make-tabs (+ nest 2)) "ret = " (ir->js x nest)))
                 (reverse (caadr data))))
      newl (make-tabs (+ nest 2)) "return ret;"
      newl (make-tabs ( + nest 1)) "})"))

(define (gen-js-call data nest)
   ;(display data)
   ;(display (cadr data))
   (define semi "") ;";"
   (string-append
      (lookup-func (ir->js (car data) nest)) "("
      ;(lst->comma-str (map (lambda (x) (cadr x)) (cadr data)))
      ;(lst->comma-str (map (lambda (x) (ir->js (cadr x) nest)) (cadr data)))
      (lst->comma-str (map (lambda (x) (ir->js x nest)) (cadr data)))
      ;(lst->comma-str (map (lambda (x) (car (ir->js x nest))) (cadr data)))
      ")" semi))

(define (gen-js-assign data nest)
   ;(display (cadadr data))
   ;(display (cadr data))
   ;(display "-----------------------")
   ;(display (ir->js (cadr data) nest))
   ;(display "-----------------------")
   (define newl "") ;(define newl "\n")
   (string-append "var " (ir->js (car data) nest) " = " (ir->js (cadr data) nest) ";" newl))

(load "scm_lib.scm")
(define (emit-js-init)
   ;(read-f "js_std.js"))
   "var scm = require('./js_std.js');")

(define (ir->js ir nest)
   (let ((data (get-data ir)))
      (cond
         ((is-type? ir 'block) (gen-js-blk (car data) nest))
         ((is-type? ir 'begin) (gen-js-blk data nest))
         ((is-type? ir 'if) (gen-js-if data nest))
         ((is-type? ir 'lambda) (gen-js-lambda data nest))
         ((is-type? ir 'call) (gen-js-call data nest))
         ((is-type? ir 'assign) (gen-js-assign data nest))
         ((is-type? ir 'num) (number->string (car data))) ;(number->string data)
         ((is-type? ir 'sym) (symbol->string (car data)))
         ((is-type? ir 'str) (string-append "\"" (car data) "\""))
         (else (string-append "BAD IR TYPE:" (symbol->string (get-type ir)))))))

(define (repl-loop)
   (define (helper)
      (define exp (str-to-exp (stdin-read)))
      (if (eq? exp 'exit)
         '()
         (begin (let ((comp-exp (exp->ir exp)))
                  ;(display (print-ir1 comp-exp 1))
                  ;(display (print-ir1 (list (ir-tag 'block) (list comp-exp)) 1))

                  (display (ir->js comp-exp 0))
                  (display "\n")
                  )
                ;(display "\n")
                ;(display (ir->js (runner `(,exp)) 0))
                '())))
                ;(helper))))
   ;(display (emit-js-init))
   (helper))

(define (run-file) '())

;(display (ir->js (runner exp-lisp) 0))
;(repl-loop)
(repl-loop)


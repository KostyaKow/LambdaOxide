;; Read a text file
(define (read-f path)
   (define ret "")
   (call-with-input-file path
     (lambda (input-port)
       (let loop ((x (read-char input-port)))
         (if (not (eof-object? x))
             (begin
               ;(display x)
               (set! ret (string-append ret (string x)))
               (loop (read-char input-port)))))))
   ret)

(define (str-to-exp str)
   (read (open-input-string str)))

(use-modules (ice-9 rdelim))

(define (stdin-read)
   ;(define ret "")
   ;(let loop ((x (read-char (current-input-port))))
   ;   (if (not (eof-object? x))
   ;      (begin
   ;         ;(display x)
   ;         (set! ret (string-append ret (string x)))
   ;         (loop (read-char (current-input-port))))))
   ;ret)
   (read-line (current-input-port)))

;(display "you entered") (display (stdin-read))

;(define z (read-f "out.js.scm"))
;(display z)

;; Write to a text file
;(call-with-output-file "b.txt"
;  (lambda (output-port)
;    (display "hello, world" output-port)))

;guile ir.scm | phantomjs

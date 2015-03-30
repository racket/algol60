#lang racket/base

(require drracket/check-syntax
         racket/runtime-path
         syntax/modread
         racket/match
         racket/set
         rackunit)
(define-runtime-path made-up-example-file.rkt "made-up-example-file.rkt")

(define the-program
  (string-append
   "#lang algol60\n"
   "begin\n"
   "  integer procedure f(theargument);\n"
   "    integer theargument;\n"
   "  printnln(theargument);\n"
   "end\n"))

(define theargument-positions
  (regexp-match-positions* #rx"theargument" the-program))

(define directives
  (let ([p (open-input-string the-program)])
    (port-count-lines! p)
    (parameterize ([current-namespace (make-base-namespace)])
      (with-module-reading-parameterization
       (λ ()
         (show-content
          (read-syntax
           made-up-example-file.rkt
           p)))))))

(define binding-arrow-directives
  (set-remove
   (for/set ([directive (in-list directives)])
     (match directive
       [`#(syncheck:add-arrow/name-dup/pxpy
           ,start-left ,start-right
           ,_ ,_
           ,end-left ,end-right
           ,_ ...)
        (list (cons start-left start-right)
              (cons end-left end-right))]
       [_ #f]))
   #f))


;; make sure there is a link from the argument to the declaration of its type
(check-true (set-member? binding-arrow-directives
                         (list (list-ref theargument-positions 0)
                               (list-ref theargument-positions 1))))

;; make sure there is a link from the argument to the use of the argument in the body
(check-true (set-member? binding-arrow-directives
                         (list (list-ref theargument-positions 0)
                               (list-ref theargument-positions 2))))

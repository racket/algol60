(module base mzscheme
  (require "algol60.ss"
           "prims.ss")

  (provide (all-from "prims.ss")
           
           (rename a60:sin sin)
           (rename a60:cos cos)
           arctan
           (rename a60:sqrt sqrt)
           (rename a60:abs abs)
           ln
           (rename a60:exp exp)
           
           #%top
           #%app
           #%datum)
  
  (define (a60:abs k v)
    (k (abs (v))))
  
  (define (a60:sqrt k v)
    (k (sqrt (v))))
  
  (define (a60:sin k v)
    (k (sin (v))))
  
  (define (a60:cos k v)
    (k (cos (v))))
  
  (define (a60:exp k v)
    (k (exp (v))))
  
  (define (arctan k v)
    (k (atan (v))))
  
  (define (ln k v)
    (k (log (v)))))
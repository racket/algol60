(module prims mzscheme
  (provide != ! & \|
           => ==
           sign entier
           prints printn
           printsln printnln)
  
  (define (!= a b)
    (not (= a b)))
  
  (define (! a)
    (unless (boolean? a)
      (raise-type-error '! "boolean" a))
    (not a))
  
  (define (& a b)
    (unless (boolean? a)
      (raise-type-error '& "boolean" 0 a b))
    (unless (boolean? b)
      (raise-type-error '& "boolean" 1 a b))
    (and a b))
  
  (define (\| a b)
    (unless (boolean? a)
      (raise-type-error '\| "boolean" 0 a b))
    (unless (boolean? b)
      (raise-type-error '\| "boolean" 1 a b))
    (or a b))
  
  (define (=> a b)
    (unless (boolean? a)
      (raise-type-error '=> "boolean" 0 a b))
    (unless (boolean? b)
      (raise-type-error '=> "boolean" 1 a b))
    (or (not a) b))
  
  (define (== a b)
    (unless (boolean? a)
      (raise-type-error '== "boolean" 0 a b))
    (unless (boolean? b)
      (raise-type-error '== "boolean" 1 a b))
    (eq? a b))
  
  (define (sign k v)
    (k (let ([v (v)])
         (cond
           [(< v 0) -1]
           [(> v 0) 1]
           [else 0]))))
  
  (define (entier k v)
    (k (inexact->exact (floor (v)))))
  
  (define (printsln k v)
    (k (printf "~a~n" (v))))
  
  (define (printnln k v)
    (k (printf "~a~n" (v))))
  
  (define (prints k v)
    (k (printf "~a" (v))))
  
  (define (printn k v)
    (k (printf "~a" (v)))))

  
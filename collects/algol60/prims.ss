(module prims mzscheme
  (provide != ! & \|
           => ==
           sign entier

	   a60:sin
           a60:cos
           a60:arctan
           a60:sqrt
           a60:abs
           a60:ln
           a60:exp

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
  
  (define (a60:arctan k v)
    (k (atan (v))))
  
  (define (a60:ln k v)
    (k (log (v))))
  
  (define (printsln k v)
    (k (printf "~a~n" (v))))
  
  (define (printnln k v)
    (k (printf "~a~n" (v))))
  
  (define (prints k v)
    (k (printf "~a" (v))))
  
  (define (printn k v)
    (k (printf "~a" (v)))))

  
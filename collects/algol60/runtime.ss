(module runtime mzscheme
  
  (provide (struct a60:array (vec dimens))
           (struct a60:switch (choices))
           undefined
           check-boolean
           goto
           get-value
           set-target!
           make-array
           array-ref
           array-set!
           make-switch
           switch-ref)
           
  (define-struct a60:array (vec dimens))
  (define-struct a60:switch (choices))
  
  (define undefined (letrec ([x x]) x))
  (define (check-boolean b) b)
  (define (goto f n) (if (number? f) (n f) (f)))
  (define (get-value v) (v))
  (define (set-target! t v) (t v))
  
  (define (make-array . dimens)
    (make-a60:array
     ((let loop ([dimens dimens])
        (if (null? dimens)
            (lambda () undefined)
            (let ([start (car dimens)]
                  [end (cadr dimens)])
              (let ([build (loop (cddr dimens))])
                (lambda () 
                  (let ([len (add1 (- end start))])
                    (let ([v (make-vector len)])
                      (let loop ([len len])
                        (unless (zero? len)
                          (vector-set! v (sub1 len) (build))
                          (loop (sub1 len))))
                      v))))))))
     dimens))
  
  (define (array-ref a . indices)
    (let loop ([v (a60:array-vec a)][indices indices][dimens (a60:array-dimens a)])
      (let ([i (vector-ref v (- (car indices) (car dimens)))])
        (if (null? (cdr indices))
            i
            (loop i (cdr indices) (cddr dimens))))))
  
  (define (array-set! a val . indices)
    (let loop ([v (a60:array-vec a)][indices indices][dimens (a60:array-dimens a)])
      (if (null? (cdr indices))
          (vector-set! v (- (car indices) (car dimens)) val)
          (loop (vector-ref v (- (car indices) (car dimens))) (cdr indices) (cddr dimens)))))
  
  (define (make-switch . choices)
    (make-a60:switch (list->vector choices)))
  (define (switch-ref sw index)
    (unless (and (number? index)
                 (integer? index)
                 (exact? index)
                 (<= 1 index (vector-length (a60:switch-choices sw))))
      (error "bad switch index: " index))
    ((vector-ref (a60:switch-choices sw) (sub1 index)))))

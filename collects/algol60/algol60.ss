#cs(module algol60 mzscheme
     (require "parse.ss"
              "simplify.ss"
              "prims.ss"
              (lib "match.ss")
              (lib "list.ss"))
     
     (provide compile-simplified)
     
     (define (compile-simplified stmt)
       (datum->syntax-object 
        #'here
        (compile-a60 stmt 'void (empty-context))))

     (define (compile-a60 stmt next-label context)
       (match stmt
         [($ a60:block decls statements)
          (compile-block decls statements next-label context)]
         [else
          (compile-statement stmt next-label context)]))
     
     ;; run-time support:
     
     (define-struct a60:array (vec dimens))
     (define-struct a60:switch (choices))
     
     (define undefined (letrec ([x x]) x))
     (define (check-boolean b) b)
     (define (goto f) (f))
     (define (get-value v) (v))
     
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
             (loop (vector-ref v (- (car indices) (car dimens)) (cdr indices) (cddr dimens))))))
     
     (define (make-switch . choices)
       (make-a60:switch (list->vector choices)))
     (define (switch-ref sw index)
       (vector-ref (a60:switch-choices sw) index))
     
     (define (compile-block decls statements next-label context)
       (let ([context (foldl (lambda (decl context)
                               (match decl
                                 [($ a60:proc-decl result-type var arg-vars by-value-vars arg-specs body)
                                  (add-procedure context var result-type arg-vars by-value-vars arg-specs)]
                                 [($ a60:type-decl type ids)
                                  (add-atoms context ids type)]
                                 [($ a60:array-decl type arrays)
                                  (add-arrays context (map car arrays) (map cdr arrays) type)]
                                 [($ a60:switch-decl name exprs)
                                  (add-switch context name)]))
                             (add-labels
                              context
                              (map car statements))
                             decls)])
         `(letrec (,@(apply
                      append
                      (map (lambda (decl)
                             (match decl
                               [($ a60:proc-decl result-type var arg-vars by-value-vars arg-specs body)
                                `([,var
                                   (lambda ,arg-vars
                                     (let ,(map (lambda (var)
                                                  `[,var (get-value ,var)])
                                                by-value-vars)
                                       ,(let ([result-var (gensym 'prec-result)])
                                          `(let ([,result-var undefined])
                                             ,(compile-a60 body `(lambda () ,result-var)
                                                           (add-settable-procedure
                                                            (add-bindings
                                                             context
                                                             arg-vars
                                                             by-value-vars
                                                             arg-specs)
                                                            var
                                                            result-type
                                                            result-var))))))])]
                               [($ a60:type-decl type ids)
                                (map (lambda (id) `[,id undefined]) ids)]
                               [($ a60:array-decl type arrays)
                                (map (lambda (array) 
                                       `[,(car array) (make-array
                                                       ,@(apply
                                                          append
                                                          (map
                                                           (lambda (bp)
                                                             (list
                                                              (compile-expression (car bp) context)
                                                              (compile-expression (cdr bp) context)))
                                                           (cdr array))))])
                                     arrays)]
                               [($ a60:switch-decl name exprs)
                                `([,name (make-switch ,@(map (lambda (e) `(lambda () ,(compile-expression e context)))
                                                             exprs))])]
                               [else (error "can't compile decl")]))
                           decls))
                   ,@(cdr 
                      (foldr (lambda (stmt next-label+compiled)
                               (cons (car stmt)
                                     (cons
                                      `[,(car stmt)
                                        (lambda ()
                                          ,(compile-statement (cdr stmt) (car next-label+compiled) context))]
                                      (cdr next-label+compiled))))
                             (cons next-label null)
                             statements)))
            (,(caar statements)))))

     (define (compile-statement statement next-label context)
       (match statement
         [($ a60:block decls statements)
          (compile-block decls statements next-label context)]
         [($ a60:if test ($ a60:goto then) ($ a60:goto else))
          `(if (check-boolean ,(compile-expression test context)) (goto ,then) (goto ,else))]
         [($ a60:goto label)
          `(goto ,(compile-expression label context))]
         [($ a60:dummy)
          `(,next-label)]
         [($ a60:call proc args)
          `(begin
             (,(compile-expression proc context) 
              ,@(map (lambda (arg) (compile-argument arg context))
                     args))
             (,next-label))]
         [($ a60:assign vars val)
          `(begin
             (let ([val ,(compile-expression val context)])
               ,@(map (lambda (avar)
                        (let ([var (a60:variable-name avar)])
                          (cond
                            [(call-by-name-variable? var context)
                             `(set-target! ,var val)]
                            [(procedure-result-variable? var context)
                             `(set! ,(procedure-result-variable-name var context) val)]
                            [(settable-variable? var context)
                             (if (own-variable? var context)
                                 `(set-box! ,var val)
                                 `(set! ,var val))]
                            [(array-element? var context)
                             `(array-set! ,var val ,@(map (lambda (e) (compile-expression e context)) 
                                                          (a60:variable-indices avar)))]
                            [else (error "confused by assignment")])))
                      vars))
             (,next-label))]
         [else (error "can't compile statement")]))
     
     (define (compile-expression expr context)
       (match expr
         [(? (lambda (x) (and (syntax? x) (number? (syntax-e x)))) n) n]
         [(? (lambda (x) (and (syntax? x) (boolean? (syntax-e x)))) n) n]
         [(? (lambda (x) (and (syntax? x) (string? (syntax-e x)))) n) n]
         [(? identifier? i) i]
         [(? symbol? i) (datum->syntax-object #f i)]
         [($ a60:subscript array index)
          ;; Must be a switch index
          `(switch-ref ,array ,(compile-expression index context))]
         [($ a60:binary op e1 e2)
          `(,op ,(compile-expression e1 context) ,(compile-expression e2 context))]
         [($ a60:unary op e1)
          `(,op ,(compile-expression e1 context))]
         [($ a60:variable var subscripts)
          (cond
            [(call-by-name-variable? var context)
             `(get-value ,var)]
            [(or (procedure-result-variable? var context)
                 (label-variable? var context)
                 (settable-variable? var context))
             (if (own-variable? var context)
                 `(unbox ,var)
                 var)]
            [(array-element? var context)
             `(array-ref ,var ,@(map (lambda (e) (compile-expression e context)) subscripts))]
            [else (error 'compile-expression "confused by variable use: ~a in: ~a" var 
                         (var-binding var context))])]
         [($ a60:app func args)
          `(,(compile-expression func context)
            ,@(map (lambda (e) (compile-argument e context))
                   args))]
         [else (error 'compile-expression "can't compile expression ~a" expr)]))
     
     (define (compile-argument arg context)
       (cond
         [(a60:variable? arg)
          `(case-lambda
             [() ,(compile-expression arg context)]
             [(val)  ,(compile-statement (make-a60:assign (list arg) 'val) 'void context)])]
         [else `(lambda () ,(compile-expression arg context))]))
     
     ;; --------------------
     
     (define (empty-context) null)
     
     (define (add-labels context l)
       (cons (map (lambda (lbl) (cons (if (symbol? lbl)
                                          (datum->syntax-object #f lbl)
                                          lbl)
                                      'label)) l)
             context))
     
     (define (add-procedure context var result-type arg-vars by-value-vars arg-specs)
       (cons (list (cons var 'procedure))
             context))
     
     (define (add-settable-procedure context var result-type result-var)
       (cons (list (cons var `(settable-procedure ,result-var)))
             context))
     
     (define (add-atoms context ids type)
       (cons (map (lambda (id) (cons id type)) ids)
             context))

     (define (add-arrays context names dimensionses type)
       (cons (map (lambda (name dimensions)
                    (cons name `(array ,(length dimensions))))
                  names dimensionses)
             context))

     (define (add-switch context name)
       (cons (list (cons name 'switch))
             context))
     
     (define (add-bindings context arg-vars by-value-vars arg-specs)
       (cons (map (lambda (var)
                    (cons var
                          (if (ormap (lambda (x) (bound-identifier=? var x)) by-value-vars)
                              #'integer ; guess!
                              'by-name)))
                  arg-vars)
             context))
     
     (define (var-binding var context)
       (if (null? context)
           #f
           (let ([m (ormap (lambda (b)
                             (and (module-identifier=? var (car b))
                                  (cdr b)))
                           (car context))])
             (or m (var-binding var (cdr context))))))
     
     (define (call-by-name-variable? var context)
       (let ([v (var-binding var context)])
         (eq? v 'by-name)))
     
     (define (procedure-result-variable? var context)
       (let ([v (var-binding var context)])
         (and (pair? v)
              (eq? (car v) 'settable-procedure)
              (cadr v))))
     
     (define procedure-result-variable-name procedure-result-variable?)

     (define (label-variable? var context)
       (let ([v (var-binding var context)])
         (eq? v 'label)))
         
     (define (settable-variable? var context)
       (let ([v (var-binding var context)])
         (or (box? v)
             (and (syntax? v)
                  (memq (syntax-e v) '(integer real Boolean))))))
     
     (define (own-variable? var context)
       (let ([v (var-binding var context)])
         (box? v)))

     (define (array-element? var context)
       (let ([v (var-binding var context)])
         (and (pair? v)
              (eq? (car v) 'array)
              (cadr v))))
     )

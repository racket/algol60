#cs(module compile "cmzscheme.ss"
     (require "parse.ss"
              "simplify.ss"
              "prims.ss"
              "runtime.ss"
              "moreprims.ss"
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
                                   (lambda (kont . ,arg-vars)
                                     (let ,(map (lambda (var)
                                                  `[,var (get-value ,var)])
                                                by-value-vars)
                                       ,(let ([result-var (gensym 'prec-result)]
                                              [done (gensym 'done)])
                                          `(let* ([,result-var undefined]
                                                  [,done (lambda () (kont ,result-var))])
                                             ,(compile-a60 body done
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
          (at (expression-location proc)
              `(,(compile-expression proc context)
                (lambda (val) (,next-label))
                ,@(map (lambda (arg) (compile-argument arg context))
                       args)))]
         [($ a60:assign vars val)
          `(begin
             (let ([val ,(compile-expression val context)])
               ,@(map (lambda (avar)
                        (let ([var (a60:variable-name avar)])
                          (cond
                            [(null? (a60:variable-indices avar))
                             (cond
                               [(call-by-name-variable? var context)
                                `(set-target! ,var val)]
                               [(procedure-result-variable? var context)
                                `(set! ,(procedure-result-variable-name var context) val)]
                               [(or (settable-variable? var context)
                                    (array-element? var context))
                                (if (own-variable? var context)
                                    `(set-box! ,var val)
                                    `(set! ,var val))]
                               [else (raise-syntax-error #f "confused by assignment" (expression-location var))])]
                            [else
                             `(array-set! ,(compile-expression (make-a60:variable var null) context)
                                          val ,@(map (lambda (e) (compile-expression e context)) 
                                                     (a60:variable-indices avar)))])))
                      vars))
             (,next-label))]
         [else (error "can't compile statement")]))
     
     (define (compile-expression expr context)
       (match expr
         [(? (lambda (x) (and (syntax? x) (number? (syntax-e x)))) n) n]
         [(? (lambda (x) (and (syntax? x) (boolean? (syntax-e x)))) n) n]
         [(? (lambda (x) (and (syntax? x) (string? (syntax-e x)))) n) n]
         [(? identifier? i) (compile-expression (make-a60:variable i null) context)]
         [(? symbol? i) (datum->syntax-object #f i)]
         [($ a60:subscript array index)
          ;; Must be a switch index
          (at array
              `(switch-ref ,array ,(compile-expression index context)))]
         [($ a60:binary op e1 e2)
          (at op
              `(,op ,(compile-expression e1 context) ,(compile-expression e2 context)))]
         [($ a60:unary op e1)
          (at op
              `(,op ,(compile-expression e1 context)))]
         [($ a60:variable var subscripts)
          (let ([sub (lambda (v)
                       (if (null? subscripts)
                           v
                           `(array-ref ,v ,@(map (lambda (e) (compile-expression e context)) subscripts))))])
            (cond
              [(call-by-name-variable? var context)
               (sub `(get-value ,var))]
              [(or (procedure-result-variable? var context)
                   (procedure-variable? var context)
                   (label-variable? var context)
                   (settable-variable? var context)
                   (array-element? var context))
               (if (own-variable? var context)
                   (sub `(unbox ,var))
                   (sub var))]
              [else (raise-syntax-error
                     #f
                     (expression-location var))]))]
                   
         [($ a60:app func args)
          (at (expression-location func)
              `(,(compile-expression func context)
                values
                ,@(map (lambda (e) (compile-argument e context))
                       args)))]
         [else (error 'compile-expression "can't compile expression ~a" expr)]))
     
     (define (expression-location expr)
       (if (syntax? expr)
           expr
           (match expr
             [($ a60:subscript array index) (expression-location array)]
             [($ a60:binary op e1 e2) op]
             [($ a60:unary op e1) op]
             [($ a60:variable var subscripts) (expression-location var)]
             [(array-element? var context) (expression-location var)]
             [($ a60:app func args)
              (expression-location func)]
             [else #f])))
     
     (define (compile-argument arg context)
       (cond
         [(and (a60:variable? arg) 
               (not (procedure-variable? (a60:variable-name arg) context)))
          `(case-lambda
             [() ,(compile-expression arg context)]
             [(val)  ,(compile-statement (make-a60:assign (list arg) 'val) 'void context)])]
         [(identifier? arg)
          (compile-argument (make-a60:variable arg null) context)]
         [else `(lambda () ,(compile-expression arg context))]))
     
     (define (at stx expr)
       (if (syntax? stx)
           (datum->syntax-object #'here expr stx)
           expr))
     
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
           'procedure
           (let ([m (ormap (lambda (b)
                             (and (module-identifier=? var (car b))
                                  (cdr b)))
                           (car context))])
             (or m (var-binding var (cdr context))))))
     
     (define (call-by-name-variable? var context)
       (let ([v (var-binding var context)])
         (eq? v 'by-name)))
     
     (define (procedure-variable? var context)
       (let ([v (var-binding var context)])
         (eq? v 'procedure)))

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

#cs(module simplify mzscheme
     (require "parse.ss"
              "prims.ss"
              (lib "match.ss"))
     
     (provide simplify print-simplified)
      
     ;; flatten/label-block : list-of-decl list-of-stmt -> block-stmt
     ;; Desugars `for', converts `if' so that it's always of the form
     ;; `if <test> then goto <label> else goto <label>', flattens
     ;; compound statements into the enclosing block, and gives every
     ;; statement exactly one label. The result usually has lots of
     ;; "dummy" statements that could easily be eliminated by merging
     ;; labels.
     (define (flatten/label-block decls statements)
       (define extra-decls null)
       (define new-statements
         (let loop ([l statements])
           (if (null? l)
               null
               (match (car l)
                 [($ a60:block decls statements)
                  (cons (cons (gensym 'block) (flatten/label-block decls statements))
                        (loop (cdr l)))]
                 [($ a60:compound statements)
                  (loop (append statements (cdr l)))]
                 [($ a60:branch test then else)
                  (if (and (a60:goto? then) (a60:goto? else))
                      (cons (cons (gensym 'branch) (car l))
                            (loop (cdr l)))
                      (let ([then-label (gensym 'then)]
                            [else-label (gensym 'else)]
                            [cont-label (gensym 'if-cont)])
                        (loop
                         (list*
                          (make-a60:if test (make-a60:goto then-label) (make-a60:goto else-label))
                          (make-a60:label then-label then)
                          (make-a60:goto cont-label)
                          (make-a60:label else-label else)
                          (make-a60:label cont-label (make-a60:dummy))
                          (cdr l)))))]
                 [($ a60:for variable val-exprs body)
                  (let ([body-label (gensym 'for-body)]
                        [cont-label (gensym 'for-cont)])
                    (letrec ([make-init+test+increment+loop
                              (lambda (value)
                                (match value
                                  [($ a60:for-number value)
                                   (values (make-a60:assign (list variable) (make-a60:binary #'+ #'0 value)) ; +0 => number
                                           #'#t
                                           (make-a60:dummy)
                                           #f)]
                                  [($ a60:for-step start step end)
                                   (values (make-a60:assign (list variable) start)
                                           (make-a60:binary #'<=
                                                            (make-a60:binary #'*
                                                                             (make-a60:binary #'- variable end)
                                                                             (make-a60:app #'sign (list step)))
                                                            #'0)
                                           (make-a60:assign (list variable) (make-a60:binary #'+ variable step))
                                           #t)]
                                  [($ a60:for-while value test)
                                   (values (make-a60:assign (list variable) value)
                                           test
                                           (make-a60:assign (list variable) value)
                                           #t)]))])
                      (if (= 1 (length val-exprs))
                          (let-values ([(init test inc loop?) (make-init+test+increment+loop (car val-exprs))])
                            (loop (list*
                                   init
                                   (make-a60:label body-label (make-a60:dummy))
                                   (make-a60:branch test 
                                                    (make-a60:compound
                                                     (list
                                                      body
                                                      inc
                                                      (if loop?
                                                          (make-a60:goto body-label)
                                                          (make-a60:dummy))))
                                                    (make-a60:dummy))
                                   (cdr l))))
                          (let* ([stage-name (datum->syntax-object #f (gensym 'stage-number))]
                                 [switch-name (datum->syntax-object #f (gensym 'stage-switch))]
                                 [end-switch-name (datum->syntax-object #f (gensym 'stage-switch))]
                                 [stage-var (make-a60:variable stage-name null)]
                                 [start-labels (map (lambda (x) (gensym 'stage)) (append val-exprs (list 'extra)))]
                                 [end-labels (map (lambda (x) (gensym 'stage)) val-exprs)])
                            (set! extra-decls (list* stage-name 
                                                     (cons switch-name start-labels)
                                                     (cons end-switch-name end-labels)
                                                     extra-decls))
                            (loop
                             (append
                              (list (make-a60:assign (list stage-var) #'0))
                              (let loop ([start-labels start-labels][end-labels end-labels][val-exprs val-exprs])
                                (if (null? val-exprs)
                                    (list (make-a60:label (car start-labels) (make-a60:dummy)))
                                    (let-values ([(init test inc loop?) (make-init+test+increment+loop (car val-exprs))])
                                      (list*
                                       (make-a60:label (car start-labels) (make-a60:dummy))
                                       init
                                       (make-a60:branch test 
                                                        (make-a60:goto body-label)
                                                        (make-a60:compound                                                   
                                                         (list
                                                          (make-a60:assign (list stage-var) (make-a60:binary #'+ #'1 stage-var))
                                                          (make-a60:goto (make-a60:subscript switch-name stage-var)))))
                                       (make-a60:label (car end-labels) (make-a60:dummy))
                                       inc
                                       (if loop?
                                           (make-a60:goto (car start-labels))
                                           (make-a60:goto (cadr start-labels)))
                                       (loop (cdr start-labels)
                                             (cdr end-labels)
                                             (cdr val-exprs))))))
                              (list 
                               (make-a60:goto cont-label)
                               (make-a60:label body-label (make-a60:dummy))
                               body
                               (make-a60:goto (make-a60:subscript end-switch-name stage-var))
                               (make-a60:label cont-label (make-a60:dummy)))
                              (cdr l)))))))]
                 [($ a60:label name statement)
                  (cons (cons name (make-a60:dummy))
                        (loop (cons statement (cdr l))))]
                 [else
                  (cons (cons (gensym 'other) (car l))
                        (loop (cdr l)))]))))
       (make-a60:block
        (append
         (map (lambda (decl)
                (match decl
                  [($ a60:proc-decl result-type var arg-vars by-value-vars arg-specs body)
                   (make-a60:proc-decl result-type var arg-vars by-value-vars arg-specs
                                       (simplify body))]
                  [else decl]))
              decls)
         (map (lambda (extra)
                (if (identifier? extra)
                    (make-a60:type-decl #'integer (list extra))
                    (make-a60:switch-decl (car extra) (map (lambda (x) 
                                                             (make-a60:variable (datum->syntax-object #f x) null)) 
                                                           (cdr extra)))))
              extra-decls))
        (if (null? new-statements)
            (list (cons (gensym 'other) (make-a60:dummy)))
            new-statements)))
     
     (define (simplify stmt)
       (match stmt
         [($ a60:block decls statements)
          (flatten/label-block decls statements)]
         [($ a60:compound statements)
          (flatten/label-block null statements)]
         [else stmt]))
     
     (define (print-simplified stmt indent)
       (match stmt
         [($ a60:block decls statements)
          (print-simplified-block decls statements indent #t)]
         [else
          (print-simplified-statement stmt '|| indent)]))
     
     (define (print-list sep args printer)
       (unless (null? args)
         (let loop ([args args])
           (printer (car args))
           (unless (null? (cdr args))
             (begin
               (printf "~a " sep)
               (loop (cdr args)))))))
     
     (define (print-syntax s) (printf "~a" (syntax-e s)))
     
     (define (print-simplified-block decls statements indent begin?)
       (when begin? (printf/indent indent "BEGIN~n"))
       (for-each
        (lambda (decl)
          (match decl
            [($ a60:proc-decl result-type var arg-vars by-value-vars arg-specs body)
             (printf/indent (add1 indent) "")
             (when result-type 
               (print-type result-type) 
               (printf " "))
             (printf "PROCEDURE ~a" (syntax-e var))
             (printf "(")
             (print-list "," arg-vars print-syntax)
             (printf ")~n")
             (unless (null? by-value-vars)
               (printf/indent (+ 2 indent) "VALUE ")
               (print-list "," by-value-vars print-syntax)
               (printf ";~n"))
             (print-simplified body (+ 2 indent))]
            [($ a60:type-decl type ids)
             (printf/indent (add1 indent) "")
             (when (box? type)
               (print "OWN "))
             (print-type (if (box? type) (unbox type) type))
             (printf " ")
             (print-list "," ids print-syntax)
             (printf ";~n")]
            [($ a60:array-decl type arrays)
             (printf/indent (add1 indent) "")
             (when type
               (when (box? type)
                 (print "OWN "))
               (print-type type)
               (printf " "))
             (printf "ARRAY ")
             (print-list "," arrays (lambda (array)
                                      (printf "~a[" (syntax-e (car array)))
                                      (print-list "," 
                                                  (cdr array)
                                                  (lambda (bp)
                                                    (print-expression (car bp))
                                                    (printf ":")
                                                    (print-expression (cdr bp))))
                                      (printf "]")))
             (printf ";~n")]
            [($ a60:switch-decl name exprs)
             (printf/indent (add1 indent) "SWITCH ~a := " (syntax-e name))
             (print-list "," exprs print-expression)
             (printf ";~n")]
            [else (printf/indent (add1 indent) "...decl ~a...~n" decl)]))
        decls)
       (map (lambda (lstmt)
              (print-simplified-statement (cdr lstmt) (car lstmt) (add1 indent)))
            statements)
       (printf/indent indent "END;~n"))

     (define (print-simplified-statement stmt label indent)
       (let ([label (if (syntax? label) (syntax-e label) label)])
         (printf "~a:~a~a" label (make-string (max 0 (- 19 (string-length (symbol->string label)))) #\space)
                 (make-string (* indent 2) #\space)))
       (match stmt
         [($ a60:block decls statements)
          (printf "BEGIN~n")
          (print-simplified-block decls statements indent #f)]
         [($ a60:if test ($ a60:goto then) ($ a60:goto else))
          (printf "IF ")
          (print-expression test)
          (printf " THEN GOTO ~a ELSE GOTO ~a;~n" then else)]
         [($ a60:goto label)
          (printf "GOTO ")
          (cond
            [(syntax? label) (printf "~a" (syntax-e label))]
            [(a60:subscript? label)
             (printf "~a[" (syntax-e (a60:subscript-array label)))
             (print-expression (a60:subscript-index label))
             (printf "]")]
            [else (printf "~a" label)])
          (printf ";~n")]
         [($ a60:dummy)
          (printf ";~n")]
         [($ a60:call proc args)
          (print-expression proc)
          (printf "(")
          (print-list "," args print-expression)
          (printf ");~n")]
         [($ a60:assign vars val)
          (print-list ":=" vars print-expression)
          (printf " := ")
          (print-expression val)
          (printf ";~n")]
         [else (printf "...stmt ~a...~n" stmt)]))
     
     (define (print-expression expr)
       (match expr
         [(? (lambda (x) (and (syntax? x) (number? (syntax-e x)))) n) (printf "~a" (syntax-e n))]
         [(? (lambda (x) (and (syntax? x) (boolean? (syntax-e x)))) n) (printf "~a" 
                                                                               (if (syntax-e n)
                                                                                   "TRUE"
                                                                                   "FALSE"))]
         [(? (lambda (x) (and (syntax? x) (string? (syntax-e x)))) n) (printf "`~a'" (syntax-e n))]
         [(? identifier? i) (printf "~a" (syntax-e i))]
         [($ a60:binary op e1 e2)
          (printf "(")
          (print-expression e1)
          (printf " ~a " (syntax-e op))
          (print-expression e2)
          (printf ")")]
         [($ a60:unary op e1)
          (printf "(~a " (syntax-e op))
          (print-expression e1)
          (printf ")")]
         [($ a60:variable s subscripts)
          (printf "~a" (syntax-e s))
          (unless (null? subscripts)
            (printf "[")
            (print-list "," subscripts print-expression)
            (printf "]"))]
         [($ a60:app func args)
          (printf "~a(" (syntax-e func))
          (print-list "," args print-expression)
          (printf ")")]
         [else (printf "...expr...")]))
          
     (define (print-type type)
       (printf "~a" (syntax-e type)))
          
     (define (printf/indent indent . rest)
       (display (make-string (+ 20 (* 2 indent)) #\space))
       (apply printf rest))

     )
   
#cs(module compile mzscheme
     (require "parse.ss"
              (lib "match.ss")
              (lib "list.ss"))
     
     (provide compile-simplified)

     ;; The compiler generates references to prim.ss and
     ;; runtime.ss exports, as well as MzScheme forms
     ;; and functions. The `ctx' argument provides
     ;; an appropriate context for those bindings (in
     ;; the form of a syntax object to use with d->s-o).
     (define (compile-simplified stmt ctx)
       (datum->syntax-object 
	ctx
	(parameterize ([current-compile-context ctx])
	  (compile-a60 stmt 'void (empty-context)))))

     (define current-compile-context (make-parameter #f))
     
     (define (compile-a60 stmt next-label context)
       (match stmt
         [($ a60:block decls statements)
          (compile-block decls statements next-label context)]
         [else
          (compile-statement stmt next-label context)]))
     
     (define (compile-block decls statements next-label context)
       (let* ([labels-with-numbers (map car statements)]
              [labels (map (lambda (l)
                             (if (stx-number? l)
				 (datum->syntax-object
				  l
				  (string->symbol (format "~a" (syntax-e l)))
				  l
				  l)
                                 l))
                           labels-with-numbers)]
              [context (foldl (lambda (decl context)
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
                               labels)
                              decls)])
	 (let ([bindings
		(append
		 (apply
		  append
		  (map (lambda (decl)
			 (match decl
			   [($ a60:proc-decl result-type var arg-vars by-value-vars arg-specs body)
			    `([,var
			       (lambda (kont . ,arg-vars)
                                 ;; Extract by-value variables
				 (let ,(map (lambda (var)
					      `[,var (get-value ,var)])
					    by-value-vars)
                                   ;; Set up the result variable and done continuation:
				   ,(let ([result-var (gensym 'prec-result)]
					  [done (gensym 'done)])
				      `(let* ([,result-var undefined]
					      [,done (lambda () (kont ,result-var))])
                                         ;; Include the compiled body:
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
							  (compile-expression (car bp) context 'num)
							  (compile-expression (cdr bp) context 'num)))
						       (cdr array))))])
				 arrays)]
			   [($ a60:switch-decl name exprs)
			    `([,name (make-switch ,@(map (lambda (e) `(lambda () ,(compile-expression e context 'des)))
							 exprs))])]
			   [else (error "can't compile decl")]))
		       decls))
		 (cdr
		  (foldr (lambda (stmt label next-label+compiled)
			   (cons label
				 (cons
				  `[,label
				    (lambda ()
				      ,(compile-statement (cdr stmt) 
							  (car next-label+compiled)
							  context))]
				  (cdr next-label+compiled))))
			 (cons next-label null)
			 statements
			 labels)))])
	   (let ([dup  (check-duplicate-identifier (filter identifier? (map car bindings)))])
	     (when dup
	       (raise-syntax-error
		#f
		"name defined twice"
		dup)))
	   `(letrec ,bindings (,(caar statements))))))

     (define (compile-statement statement next-label context)
       (match statement
         [($ a60:block decls statements)
          (compile-block decls statements next-label context)]
         [($ a60:branch test ($ a60:goto then) ($ a60:goto else))
          `(if (check-boolean ,(compile-expression test context 'bool))
               (goto ,(check-label then context))
               (goto ,(check-label else context)))]
         [($ a60:goto label)
          (at (expression-location label)
              `(goto ,(compile-expression label context 'des)))]
         [($ a60:dummy)
          `(,next-label)]
         [($ a60:call proc args)
          (at (expression-location proc)
              `(,(compile-expression proc context 'func)
                (lambda (val) (,next-label))
                ,@(map (lambda (arg) (compile-argument arg context))
                       args)))]
         [($ a60:assign vars val)
          `(begin
             (let ([val ,(compile-expression val context 'numbool)])
               ,@(map (lambda (avar)
                        (let ([var (a60:variable-name avar)])
                          (at var
                              (cond
                                [(null? (a60:variable-indices avar))
                                 (cond
                                   [(call-by-name-variable? var context)
                                    => (lambda (spec)
                                         `(set-target! ,var ',var (coerce ',(spec-coerce-target spec) val)))]
                                   [(procedure-result-variable? var context)
                                    `(set! ,(procedure-result-variable-name var context) 
                                           (coerce ',(spec-coerce-target (procedure-result-spec var context)) val))]
                                   [(or (settable-variable? var context)
                                        (array-element? var context))
                                    => (lambda (spec)
                                         `(,(if (own-variable? var context) 'set-box! 'set!)
                                           ,var
                                           (coerce ',(spec-coerce-target spec) val)))]
                                   [else (raise-syntax-error #f "confused by assignment" (expression-location var))])]
                                [else
                                 (let ([spec (array-element? var context)])
                                   `(array-set! ,(compile-expression (make-a60:variable var null) context 'numbool)
                                                (coerce ',(spec-coerce-target spec) val)
                                                ,@(map (lambda (e) (compile-expression e context 'num)) 
                                                       (a60:variable-indices avar))))]))))
                      vars))
             (,next-label))]
         [else (error "can't compile statement")]))
     
     (define (compile-expression expr context type)
       (match expr
         [(? (lambda (x) (and (syntax? x) (number? (syntax-e x)))) n) 
	  (if (eq? type 'des)
	      ;; Need a label:
	      (check-label (datum->syntax-object expr
						 (string->symbol (number->string (syntax-e expr)))
						 expr
						 expr)
			   context)
	      ;; Normal use of a number:
	      (begin
		(check-type 'num type expr) 
		(as-builtin n)))]
         [(? (lambda (x) (and (syntax? x) (boolean? (syntax-e x)))) n) (check-type 'bool type expr) (as-builtin n)]
         [(? (lambda (x) (and (syntax? x) (string? (syntax-e x)))) n)  (check-type 'string type expr) (as-builtin n)]
         [(? identifier? i) (compile-expression (make-a60:variable i null) context type)]
         [(? symbol? i) ; either a generated label or 'val:
	  (unless (eq? expr 'val)
	    (check-type 'des type expr))
	  (datum->syntax-object #f i)]
         [($ a60:subscript array index)
          ;; Maybe a switch index, or maybe an array reference
	  (at array
	      (cond
	       [(array-element? array context)
		`(array-ref ,array ,(compile-expression index context 'num))]
	       [(switch-variable? array context)
		`(switch-ref ,array ,(compile-expression index context 'num))]
	       [else (raise-syntax-error
		      #f
		      "confused by variable"
		      array)]))]
         [($ a60:binary t argt op e1 e2)
	  (check-type t type expr)
          (at op
              `(,(as-builtin op) ,(compile-expression e1 context argt) ,(compile-expression e2 context argt)))]
         [($ a60:unary t argt op e1)
	  (check-type t type expr)
          (at op
              `(,(as-builtin op) ,(compile-expression e1 context argt)))]
         [($ a60:variable var subscripts)
          (let ([sub (lambda (v)
                       (if (null? subscripts)
                           v
                           `(array-ref ,v ,@(map (lambda (e) (compile-expression e context 'num)) subscripts))))])
            (cond
              [(call-by-name-variable? var context)
               (sub `(get-value ,var))]
	      [(primitive-variable? var context)
	       => (lambda (name)
		    (sub (datum->syntax-object
			  (current-compile-context)
			  name
			  var
			  var)))]
              [(and (procedure-result-variable? var context)
                    (not (eq? type 'func)))
               (at var
                   (procedure-result-variable-name var context))]
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
                     "confused by expression"
                     (expression-location var))]))]
                   
         [($ a60:app func args)
          (at (expression-location func)
              `(,(compile-expression func context 'func)
                values
                ,@(map (lambda (e) (compile-argument e context))
                       args)))]
         [($ a60:if test then else)
          `(if (check-boolean ,(compile-expression test context 'bool))
	       ,(compile-expression then context type)
	       ,(compile-expression else context type))]
         [else (error 'compile-expression "can't compile expression ~a" expr)]))
     
     (define (expression-location expr)
       (if (syntax? expr)
           expr
           (match expr
             [($ a60:subscript array index) (expression-location array)]
             [($ a60:binary type argtype op e1 e2) op]
             [($ a60:unary type argtype op e1) op]
             [($ a60:variable var subscripts) (expression-location var)]
             [(array-element? var context) (expression-location var)]
             [($ a60:app func args)
              (expression-location func)]
             [else #f])))
     
     (define (compile-argument arg context)
       (cond
         [(and (a60:variable? arg) 
               (not (let ([v  (a60:variable-name arg)])
		      (or (procedure-variable? v context)
			  (label-variable? v context)
			  (primitive-variable? v context)))))
          `(case-lambda
             [() ,(compile-expression arg context 'any)]
             [(val)  ,(compile-statement (make-a60:assign (list arg) 'val) 'void context)])]
         [(identifier? arg)
          (compile-argument (make-a60:variable arg null) context)]
         [else `(lambda () ,(compile-expression arg context 'any))]))

     (define (check-type got expected expr)
       (or (eq? expected 'any)
	   (case got
	     [(num) (memq expected '(num numbool))]
	     [(bool) (memq expected '(bool numbool))]
	     [(des) (memq expected '(des))]
	     [(func) (memq expected '(func))]
	     [else #f])
	   (raise-syntax-error #f
			       (format "type mismatch (~a != ~a)" got expected)
			       expr)))

     (define (check-label l context)
       (if (or (symbol? l)
	       (label-variable? l context))
	   l
	   (raise-syntax-error
	    #f
	    "undefined label"
	    l)))
     
     (define (at stx expr)
       (if (syntax? stx)
           (datum->syntax-object (current-compile-context) expr stx)
           expr))

     (define (as-builtin stx)
       ;; Preserve source loc, but change to reference to
       ;; a builtin operation by changing the context:
       (datum->syntax-object
	(current-compile-context)
	(syntax-e stx)
	stx
	stx))

     ;; --------------------
     
     (define (empty-context)
       `(((sign prim sign)
	  (entier prim entier)

	  (sin prim a60:sin)
	  (cos prim a60:cos)
	  (acrtan prim a60:arctan)
	  (sqrt prim a60:sqrt)
	  (abs prim a60:abs)
	  (ln prim a60:ln)
	  (exp prim a60:exp)

	  (prints prim prints)
	  (printn prim printn)
	  (printsln prim printsln)
	  (printnln prim printnln))))
     
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
       (cons (list (cons var `(settable-procedure ,result-var ,result-type)))
             context))
     
     (define (add-atoms context ids type)
       (cons (map (lambda (id) (cons id type)) ids)
             context))

     (define (add-arrays context names dimensionses type)
       (cons (map (lambda (name dimensions)
                    (cons name `(array ,type ,(length dimensions))))
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
                              (list 'by-name 
                                    (let ([spec (ormap (lambda (spec)
                                                         (and (ormap (lambda (x) (bound-identifier=? var x))
                                                                     (cdr spec))
                                                              (car spec)))
                                                       arg-specs)])
                                      (or spec
                                          #'integer)))))) ; guess!
                  arg-vars)
             context))
     
     (define (var-binding var context)
       (if (null? context)
           'free
           (let ([m (ormap (lambda (b)
			     (if (symbol? (car b))
				 ;; primitives:
				 (and (eq? (syntax-e var) (car b))
				      (cdr b))
				 ;; everything else:
				 (and (bound-identifier=? var (car b))
				      (cdr b))))
                           (car context))])
             (or m (var-binding var (cdr context))))))
     
     (define (primitive-variable? var context)
       (let ([v (var-binding var context)])
	 (and (pair? v)
	      (eq? (car v) 'prim)
	      (cadr v))))

     (define (call-by-name-variable? var context)
       (let ([v (var-binding var context)])
         (and (pair? v)
              (eq? (car v) 'by-name)
              (cadr v))))
     
     (define (procedure-variable? var context)
       (let ([v (var-binding var context)])
         (eq? v 'procedure)))

     (define (procedure-result-variable? var context)
       (let ([v (var-binding var context)])
         (and (pair? v)
              (eq? (car v) 'settable-procedure)
              (cdr v))))
     
     (define (procedure-result-variable-name var context)
       (let ([v (procedure-result-variable? var context)])
         (car v)))

     (define (procedure-result-spec var context)
       (let ([v (procedure-result-variable? var context)])
         (cadr v)))

     (define (label-variable? var context)
       (let ([v (var-binding var context)])
         (eq? v 'label)))
         
     (define (switch-variable? var context)
       (let ([v (var-binding var context)])
         (eq? v 'switch)))
         
     (define (settable-variable? var context)
       (let ([v (var-binding var context)])
         (or (box? v)
             (and (syntax? v)
                  (memq (syntax-e v) '(integer real Boolean))
                  v))))
     
     (define (own-variable? var context)
       (let ([v (var-binding var context)])
         (box? v)))

     (define (array-element? var context)
       (let ([v (var-binding var context)])
         (and (pair? v)
              (eq? (car v) 'array)
              (cadr v))))
     
     (define (spec-coerce-target spec)
       (cond
         [(and (syntax? spec) (memq (syntax-e spec) '(string label switch real integer |Boolean| #f))) spec]
         [(eq? (syntax-e (car spec) 'array)) (cadr spec)]
         [(eq? (syntax-e (car spec)) 'procedure) 'procedure]))
     
     (define (stx-number? a) (and (syntax? a) (number? (syntax-e a)))))

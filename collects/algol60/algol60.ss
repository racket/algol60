#cs(module algol60 mzscheme
     (require-for-syntax "parse.ss"
                         "simplify.ss"
                         "compile.ss"
                         (lib "file.ss"))
     (require (lib "include.ss")
              "runtime.ss"
              "prims.ss")
              
     
     (provide include-algol)
     
     (define-syntax (include-algol stx)
       (syntax-case stx ()
         [(_ str)
          (string? (syntax-e (syntax str)))
          
          (let ([content (compile-simplified
                          (simplify 
                           (parse-a60-file 
                            (normalize-path (syntax-e (syntax str))
                                            (or
                                             (current-load-relative-directory)
                                             (current-directory))))))])
            ;; Compiled result includes syntax objects with no context.
            ;; Given them the context of this module
            (let loop ([content content])
              (cond
                [(pair? content)
                 (cons (loop (car content))
                       (loop (cdr content)))]
                [(null? content) null]
                [else
                 (let ([v (syntax-e content)])
                   (datum->syntax-object
                    (let ([has-context? (module-identifier=? #'#%app (datum->syntax-object content '#%app))])
                      (if has-context?
                          content
                          #'here))
                    (cond
                      [(pair? v) 
                       (loop v)]
                      [(vector? v)
                       (list->vector (loop (vector->list v)))]
                      [(box? v)
                       (box (loop (unbox v)))]
                      [else
                       v])
                    content
                    content))])))])))

(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "class.ss")
           "parse.ss"
           "simplify.ss"
           "compile.ss")

  (provide tool@)

  (define tool@
    (unit/sig ()
      (import drscheme:tool^)
      
      (define lang%
        (class* object% (drscheme:language:language<%>)
          (define/public (config-panel parent)
            (case-lambda
              [() null]
              [(x) (void)]))
          (define/public (default-settings) null)
          (define/public (default-settings? x) #t)
          (define/public (front-end input settings)
            (let-values ([(port name)
                          (if (string? input)
                              (values (open-input-file input) (path->complete-path input))
                              (let ([text (drscheme:language:text/pos-text input)])
                                (values
                                 (open-input-string
                                  (send text
                                        get-text
                                        (drscheme:language:text/pos-start input)
                                        (drscheme:language:text/pos-end input)))
                                 text)))])
              (lambda ()
                (if (eof-object? (peek-char port))
                    eof
                    (compile-simplified (simplify (parse-a60-port port name)))))))
          (define/public (get-style-delta) #f)
          (define/public (get-language-position) (list "Algol 60"))
          (define/public (get-language-numbers) (list 10))
          (define/public (get-teachpack-names) null)
          (define/public (marshall-settings x) x)
          (define/public (on-execute settings run-in-user-thread)
            (dynamic-require '(lib "base.ss" "algol60") #f)
            (dynamic-require '(lib "base.ss" "algol60") (void))
            (let ([path ((current-module-name-resolver) '(lib "base.ss" "algol60") #f #f)]
                  [n (current-namespace)])
              (run-in-user-thread
               (lambda ()
		 (error-display-handler 
		  (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
		 (current-eval 
		  (drscheme:debug:make-debug-eval-handler (current-eval)))
                 (with-handlers ([void (lambda (x)
                                         (printf "~a~n"
                                                 (exn-message x)))])
                   (namespace-attach-module n path)
                   (namespace-require path))))))
          (define/public (render-value value settings port port-write) (write value port))
          (define/public (render-value/format value settings port port-write) (write value port))
          (define/public (unmarshall-settings x) x)
	  (define/public (create-executable . args) (error "can't do that, yet"))
	  (define/public (get-one-line-summary) "Algol 60 (not Scheme at all!)")
          
          (super-instantiate ())))
      
      (drscheme:language-configuration:add-language
       (make-object lang%)))))
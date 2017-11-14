#lang racket

(require (for-syntax "tip-compiler.rkt" racket))

(provide (except-out (all-from-out racket)
                     #%module-begin)
         (rename-out [module-begin #%module-begin]))


;;-------------------------------------------------------------
; TIP Syntax Manipulation

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ ex ...)
     (with-syntax ([(expr ...) (replace-underscores-in-exprs (remove-annotations #'(ex ...)))])
       (with-syntax ([(const ...) (language-constants)]
                     [(((name par ...) (cs cts ...) ...) ...)
                      (consolidate-datatypes #'(expr ...))]
                     [(((c-par ...) ((c-id c-type) ...) assertion))
                      (consolidate-checks #'(expr ...))]
                     [((m-par m-name m-params m-rt m-body) ...)
                      (consolidate-functions #'(expr ...))])
         (with-syntax ([((n x ...) ...) (format-datatypes #'(((name par ...) (cs cts ...) ...) ...))]
                       [(ts ...) (make-types #'((name par ...) ...))])
           (syntax-subst #'((true #true) (false #false) (const const) ... (m-name m-name) ... (n n) ... (ts ts) ... (cs cs) ... ...)
                         #`(#%module-begin
                            #,(generate-language #'(ts ...) #'((n x ...) ...))
                            #,@(generate-metafunctions #'((m-par m-name m-params m-rt m-body) ...)
                                                       #'(((name par ...) (cs cts ...) ...) ...))
                            #,(generate-relation #'(((name par ...) (cs cts ...) ...) ...))
                            #,(generate-judgment-form #'((c-id c-type) ...))
                            #,(generate-redex-checks #'(((c-par ...) ((c-id c-type) ...) assertion)))
                            )))))]))


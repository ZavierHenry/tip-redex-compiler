#lang racket

(require (for-template redex/reduction-semantics racket))
(provide (all-defined-out))

;;-----------------------------------------------------------------
;Utility syntax manipulation

(define (syntax-preserve datum context)
  (datum->syntax context datum context context))


;; changes underscores in TIP benchmark to hyphens
(define (replace-underscores-in-exprs exprs)
  (replace-underscores exprs exprs))


(define (replace-underscores expr context)
  (match expr
    ['_ context]
    [(? symbol? (app symbol->string (and (regexp #rx"_" (list _)) identifier)))
     (syntax-preserve (string->symbol (regexp-replace* "_" identifier "-"))
                      context)]
    [(? syntax?) (replace-underscores (syntax-e expr) context)]
    [(list _ ...)
     (syntax-preserve (map (λ (x) (replace-underscores (syntax-e x) x)) expr) context)]
    [_ context]))


(define (remove-annotations exprs)
  (syntax-case exprs (:source :let)
    [() exprs]
    [(:source source rest ...) (remove-annotations #'(rest ...))]
    [(:let rest ...) (remove-annotations #'(rest ...))]
    [(first rest ...) (syntax-cons (remove-annotations #'first) (remove-annotations #'(rest ...)))]
    [_ exprs]))


;; makes redex pattern e.g. Nat_x or t_1
(define (make-pattern type suffix)
  (string->symbol (format "~a_~a" (syntax-e type) (syntax-e suffix))))


(define (pattern? v1)
  (and (identifier? v1)
       (regexp-match #rx"_[a-zA-Z0-9]+" (symbol->string (syntax-e v1)))))


(define (make-pattern-random-suffix type)
  (make-pattern type #`#,(gensym)))


;; makes compiled language type e.g. list-t or Nat-t
(define (make-type name)
  (string->symbol (format "~a-t" (syntax-e name))))


;; operates like cons but for syntax objects
(define (syntax-cons elm elms)
  (with-syntax ([(current ...) elms])
    (syntax-case elm ()
      [e #'(e current ...)])))

;; operates like append but for syntax object
(define (syntax-append lst1 lst2)
  (with-syntax ([(current ...) lst1])
    (syntax-case lst2 ()
      [() lst1]
      [(new ...) #'(current ... new ...)])))

;; substitutes table variables in expr
(define (syntax-subst table expr)
  (syntax-case expr ()
    [() expr]
    [(expr exprs ...) (syntax-cons (syntax-subst table #'expr)
                                   (syntax-subst table #'(exprs ...)))]
    [id (let ([id #'id])
          (let ([x (syntax-retrieve id table)])
            (if x x id)))]))


;; operates like map but with syntax objects
(define (syntax-map f losyn)
  #`(#,@(map f (syntax->list losyn))))


(define (syntax-equal? obj-1 obj-2)
  (and (syntax? obj-1)
       (syntax? obj-2)
       (equal? (syntax->datum obj-1)
               (syntax->datum obj-2))))


(define (maybe-term-wrap syn wrap-syn?)
  (if wrap-syn? #`(term #,syn) syn))


(define (maybe-unquote-wrap syn unquote-syn?)
  (if unquote-syn? #`,#,syn syn))


;;--------------------------------------------------------------
;Syntax searching functions

;; finds syntax object in list of syntax objects
(define (syntax-find syn-obj losyn)
  (syntax-case losyn ()
    [() #false]
    [(obj rest ...)
     (syntax-equal? #'obj syn-obj)
     #'obj]
    [(_ rest ...) (syntax-find syn-obj #'(rest ...))]))


;;retrieves the value from a map of syntax objects given its key
(define (syntax-retrieve syn-obj mosyn)
  (syntax-case mosyn ()
    [() #false]
    [((k v) rest ...) (syntax-equal? #'k syn-obj) #'v]
    [((_ _) rest ...) (syntax-retrieve syn-obj #'(rest ...))]))


;;replaces entry in table
(define (syntax-replace table key obj)
  (with-syntax ([value obj])
    (syntax-case table ()
      [() #`((#,key value))]
      [((k v) rest ...)
       (syntax-equal? #'k key)
       #'((k value) rest ...)]
      [((k v) rest ...)
       (syntax-cons #'(k v)
                    (syntax-replace #'(rest ...) key obj))])))



;;-------------------------------------------------------------------------------------------
;Redex define-language manipulation

(define (language-constants)
  #'(#%TIP #%dt #%t Int Bool Int-t Bool-t #%check #%isType))


;; collects datatypes with the form: ([[datatype name] [polymorphic variables]] [[constructor name] [constructor types]])
(define (consolidate-datatypes exprs)
  (syntax-case exprs (declare-datatypes)
    [() exprs]
    [((declare-datatypes (par ...) ((dt (id (_ t) ...) ...) ...)) exprs ...)
     (syntax-append #'(((dt par ...) (id t ...) ...) ...)
                  (consolidate-datatypes #'(exprs ...)))]
    [(_ exprs ...) (consolidate-datatypes #'(exprs ...))]))



(define (format-datatypes dts)
    (syntax-case dts ()
      [() dts]
      [(((name par ...) (id t ...) ...) rest ...)
       (syntax-cons
        (syntax-cons #'name
                     (format-constructors #'((par #%dt) ...) #'((id t ...) ...)))
        (format-datatypes #'(rest ...)))]))


(define (format-constructors table cs)
  (syntax-case cs ()
    [() cs]
    [((id) rest ...)
     (syntax-cons #'id (format-constructors table #'(rest ...)))]
    [(c rest ...)
     (syntax-cons (syntax-subst table (format-types #'c))
                  (format-constructors table #'(rest ...)))]))



(define (format-types ts)
  (syntax-case ts ()
    [() ts]
    [((id _ ...) rest ...) (syntax-cons #'id (format-types #'(rest ...)))]
    [(id rest ...) (syntax-cons #'id (format-types #'(rest ...)))]))



(define (make-types names)
  (syntax-case names ()
    [() names]
    [((id) rest ...)
     (syntax-cons (make-type #'id) (make-types #'(rest ...)))]
    [((id par ...) rest ...)
     (syntax-cons (syntax-cons (make-type #'id) 
                               (syntax-subst #'((par #%t) ...) #'(par ...)))
                  (make-types #'(rest ...)))]))
    

;; Generates redex define-language
(define (generate-language types datatypes)
  (with-syntax ([((name x ...) ...) datatypes]
                [(ts ...) types])
    #`(define-language #%TIP
        [#%t Int-t Bool-t ts ...]
        [#%dt Int Bool name ...]
        [Int integer]
        [Bool boolean]
        [name x ...] ...)))


;;------------------------------------------------------------------------------------
;Redex metafunction manipulation



;; collects function definitions with the form: ([polymorphic variables] [function name] [function parameters] [function return type] [function body)
(define (consolidate-functions exprs)
  (syntax-case exprs (define-fun-rec define-funs-rec define-fun par)
    [() exprs]
    [((define-fun-rec (par poly (name params rt body))) exprs ...)
     (syntax-cons #'(poly name params rt body)
                  (consolidate-functions #'(exprs ...)))]
    [((define-fun-rec name params rt body) exprs ...)
     (syntax-cons #'(() name params rt body)
                  (consolidate-functions #'(exprs ...)))]
    [((define-fun rest ...) exprs ...) (consolidate-functions #'((define-fun-rec rest ...) exprs ...))]
    [((define-funs-rec ((par poly (name params rt)) pars ...) (body bodies ...)) exprs ...)
     (syntax-cons #'(poly name params rt body)
                  (consolidate-functions #'((define-funs-rec (pars ...) (bodies ...)) exprs ...)))]
   
    [(_ exprs ...) (consolidate-functions #'(exprs ...))]))


(define (generate-metafunctions function-info dts)
  (syntax-map (λ (x) (with-syntax ([(par name params rt body) x])
                       (generate-metafunction #'par #'name #'params #'rt dts #'body)))
              function-info))


;; generates define-metafunction
(define (generate-metafunction poly name params rt dts body)
  (with-syntax ([(rt p ...) (format-metafunction-params (syntax-cons #`(r #,rt) params))]
                [((var type) ...) params])
    #`(define-metafunction #%TIP
        #,name : p ... -> rt
        #,@(generate-metafunction-body name params body dts))))


    
(define (format-metafunction-params params)
  (syntax-case params ()
    [() params]
    [((var (name t ...)) rest ...)
     (syntax-cons #'name
                  (format-metafunction-params #'(rest ...)))]
    [((var name) rest ...)
     (syntax-cons #'name
                  (format-metafunction-params #'(rest ...)))]))

;; generates a list of decompositions with the following form:
;; ([substitution table] [expressions] [conditionals])
(define (decompose-function-body body table dts within-term?)
  (with-syntax ([((k (v type)) ...) table])
    (syntax-case body (_ match lambda let ite @ as and or not => = distinct + - * div mod > >= < <=)
      [(let rest ...) (error "This compiler does not support let bindings")]
      [(lambda rest ...) (error "This compiler does not support first-class functions")]
      [(match var case ...)
       (syntax-find #'var #'(k ...))
       (handle-match-body table #'var (sort-match-clauses #'(case ...)) dts within-term?)]
      [(match expr case ...) (error "This compiler does not support non-single-variable match bindings")]
      [(ite cond true false)
       (with-syntax ([((t-t (c-e t-e) (t-c ...)) ...)
                      (combine-decomposition-lists
                       (decompose-function-body #'cond table dts #f)
                       (decompose-function-body #'true table dts within-term?))]
                     [((f-t (f-e) f-c) ...)
                      (decompose-function-body #'false table dts within-term?)])
         #'((t-t (t-e) (t-c ... (side-condition c-e))) ... (f-t (f-e) f-c) ...))]
      [(@ func expr ...) (error "This compiler does not support first-class functions")]
      [(_ id ty) #`((((k (v type)) ...) (#,(maybe-term-wrap #'id (not within-term?))) ()))]
      [(as id ty) #`((((k (v type)) ...) (#,(maybe-term-wrap #'id (not within-term?))) ()))]
      [(not arg)
       (with-syntax ([((t (ex) c) ...) (decompose-function-body #'arg table dts #f)])
         (if within-term? #'((t (,(not ex)) c) ...) #'((t ((not ex)) c) ...)))]
      [(and arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'and)]
      [(or arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'or)]
      [(=> premise implication) (handle-builtin-function-decomposition #'premise #'implication table dts within-term? #'implies)]
      [(= arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'equal?)]
      [(distinct arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'(negate equal?))]
      [(+ arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'+)]
      [(- arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'-)]
      [(* arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'*)]
      [(div arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'quotient)]
      [(mod arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'modulo)]
      [(> arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'>)]
      [(>= arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'>=)]
      [(< arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'<)]
      [(<= arg1 arg2) (handle-builtin-function-decomposition #'arg1 #'arg2 table dts within-term? #'<=)]
      [(id expr ...)
       (with-syntax ([((t (ex ...) c) ...) (apply combine-decomposition-lists
                                              (map (λ (x) (decompose-function-body x table dts #t))
                                                   (syntax->list #'(expr ...))))])
         (if within-term? #'((t ((id ex ...)) c) ...) #'((t ((term (id ex ...))) c) ...)))]
      [id #`((((k (v type)) ...) (#,(maybe-term-wrap (syntax-subst #'((k v) ...) body) (not within-term?))) ()))])))


(define (handle-builtin-function-decomposition arg1 arg2 table dts within-term? function)
  (with-syntax ([((t (ex1 ex2) c) ...) (combine-decomposition-lists
                                        (decompose-function-body arg1 table dts #f)
                                        (decompose-function-body arg2 table dts #f))])
    (if within-term? #`((t (,(#,function ex1 ex2)) c) ...)
        #`((t ((#,function ex1 ex2)) c) ...))))
      
    
(define (sort-match-clauses clauses)
  (syntax-case clauses (case default)
    [() clauses]
    [((case default expr) rest ...)
     #'(rest ... (case default expr))]
    [(expr rest ...) (syntax-cons #'expr (sort-match-clauses #'(rest ...)))]))


;;binds the variables of the match case pattern
(define (bind-pattern type binding dts)
  (with-syntax ([(((t p ...) (ct cts ...) ...) ...) dts])
    (syntax-case type (Int Bool)
      [Int (syntax-case binding ()
             [var (integer? (syntax->datum binding)) #'()]
             [var #`((var (#,(make-pattern #'Int #`#,(gensym)) Int)))])]
      [Bool (syntax-case binding (true false)
              [true #'()]
              [false #'()]
              [var #`((var (#,(make-pattern #'Bool #`#,(gensym)) Bool)))])]
      [(name par ...)
       (with-syntax ([((tp ...) (c cs ...) ...)
                      (syntax-retrieve #'name #'((t ((p ...) (ct cts ...) ...)) ...))])
         (syntax-case binding ()
           [(n rest ...)
            (with-syntax ([(ts ...) (syntax-subst
                                     #'((tp par) ...)
                                     (syntax-retrieve #'n #'((c (cs ...)) ...)))])
              (for/fold ([table #'()])
                        ([b (syntax->list #'(rest ...))]
                         [ty (syntax->list #'(ts ...))])
                (syntax-append table (bind-pattern ty b dts))))]
           [n (syntax-find #'n #'(c ...)) #'()]
           [n #`((n (#,(make-pattern #'name #`#,(gensym)) (name par ...))))]))]
      [name (syntax-find #'name #'(t ...))
            (with-syntax ([((c cs ...) ...) (syntax-retrieve #'name #'((t ((ct cts ...) ...)) ...))])
              (syntax-case binding ()
                [(n rest ...)
                 (with-syntax ([(ts ...) (syntax-retrieve #'n #'((c (cs ...)) ...))])
                   (for/fold ([table #'()])
                             ([b (syntax->list #'(rest ...))]
                              [ty (syntax->list #'(ts ...))])
                     (syntax-append table (bind-pattern ty b dts))))]
                [n (syntax-find #'n #'(c ...)) #'()]
                [n #`((n (#,(make-pattern #'name #`#,(gensym)) name)))]))]
      [name #`((#,binding (#,(make-pattern #'#%dt #`#,(gensym)) name)))])))




(define (handle-match-body table var cases dts within-term?)
  (with-syntax ([((v (e t)) ...) table])
    (with-syntax ([(ex ty) (syntax-retrieve var table)])
      (syntax-case cases (case default)
        [() cases]
        [((case default body) rest ...)
         (syntax-append
          (decompose-function-body #'body table dts #t)
          (handle-match-body table var #'(rest ...) dts within-term?))]
        [((case expr body) rest ...)
         (with-syntax ([((k (value tp)) ...) (bind-pattern #'ty #'expr dts)])
           (if (expr-conflict? (syntax-subst #'((k value) ...) #'expr) #'ex)
               (handle-match-body table var #'(rest ...) dts within-term?)
               (syntax-append
                (decompose-function-body
                 #'body
                 (with-syntax ([(e2 t2) #`(#,(syntax-subst #'((k value) ...) #'expr) ty)])
                   (with-syntax ([(e3 ...) (syntax-subst #`((ex e2)) #'(e ...))])
                     (syntax-replace #'((v (e3 t)) ... (k (value tp)) ...)
                                     var
                                     #'(e2 t2))))
                 dts
                 within-term?)
                (handle-match-body table var #'(rest ...) dts within-term?))))]))))


(define (combine-decomposition-lists . lodec)
  (for/fold ([decs #'()])
            ([i lodec])
    (combine-two-decomposition-lists decs i)))


(define (combine-two-decomposition-lists lodec-1 lodec-2)
  (cond
    [(syntax-equal? lodec-1 #'()) lodec-2]
    [(syntax-equal? lodec-2 #'()) lodec-1]
    [else
     (for/fold ([decs #'()])
               ([i-1 (syntax->list lodec-1)])
       (with-syntax ([(t-1 b-1 c-1) i-1])
         (syntax-append
          (syntax-map (λ (x) (combine-two-decompositions i-1 x))
                      #`#,(filter-not (λ (x) (with-syntax ([(t b c) x])
                                               (table-conflict? #'t #'t-1)))
                                      (syntax->list lodec-2)))
          decs)))]))
        

(define (combine-two-decompositions dec-1 dec-2)
  (with-syntax ([(t1 (e1 ...) (c1 ...)) dec-1]
                [(t2 (e2 ...) (c2 ...)) dec-2])
    #`(#,(combine-tables #'t1 #'t2)
       (e1 ... e2 ...)
       (c1 ... c2 ...))))


(define (reconcile-function-decomposition compositions vars)
  (syntax-case compositions ()
    [() compositions]
    [((table body extras) rest ...)
     (syntax-cons #'(table body extras)
                  (reconcile-function-decomposition
                   #`#,(filter-not (λ (x) (with-syntax ([(t b e) x])
                                            (duplicate-var-subst? #'table #'t vars)))
                                    (syntax->list #'(rest ...)))
                   vars))]))


;; merges two substitution tables
(define (combine-tables table-1 table-2)
  (with-syntax ([((k-2 (v-2 t-2)) ...) table-2])
    (syntax-case table-1 ()
      [() table-2]
      [((k-1 (v-1 t-1)) rest ...)
       (syntax-find #'k-1 #'(k-2 ...))
       (with-syntax ([(v t) (syntax-retrieve #'k-1 table-2)])
         (combine-tables
          #'(rest ...)
          (syntax-replace table-2 #'k-1 #`(#,(mix-bindings #'v-1 #'v) t))))]
      [((k-1 (v-1 t-1)) rest ...)
       (combine-tables #'(rest ...) #'((k-2 (v-2 t-2)) ... (k-1 (v-1 t-1))))])))


(define (mix-bindings expr1 expr2)
  (cond
    [(pattern? expr1) expr2]
    [(pattern? expr2) expr1]
    [else
     (syntax-case #`(#,expr1 #,expr2) ()
       [((e1 ...) (e2 ...))
        (syntax-map (λ (x) (with-syntax ([(b1 b2) x]) (mix-bindings #'b1 #'b2)))
                    #'((e1 e2) ...))]
       [(e1 e2) expr1])]))



(define (expr-conflict? expr1 expr2)
  (cond
    [(pattern? expr1) #false]
    [(pattern? expr2) #false]
    [else
     (syntax-case #`(#,expr1 #,expr2) ()
       [((e1 ...) (e2 ...))
        (= (length (syntax->list expr1))
           (length (syntax->list expr2)))
        (ormap expr-conflict?
               (syntax->list expr1)
               (syntax->list expr2))]
       [((e1 ...) (e2 ...)) #true]
       [(e1 e2) (not (syntax-equal? #'e1 #'e2))])]))
         

(define (table-conflict? table1 table2)
  (with-syntax ([((var2 (value2 type2)) ...) table2])
    (syntax-case table1 ()
      [() #false]
      [((var1 (value1 type1)) rest ...)
       (syntax-find #'var1 #'(var2 ...))
       (with-syntax ([(ex ty) (syntax-retrieve #'var1 table2)])
         (or (expr-conflict? #'value1 #'ex)
             (table-conflict? #'(rest ...) table2)))]
      [(_ rest ...) (table-conflict? #'(rest ...) table2)])))
      

    
(define (duplicate-var-subst? table1 table2 vars)
  (syntax-case vars ()
    [() #true]
    [(var rest ...)
     (with-syntax ([((k-1 (v-1 t-1)) ...) table1]
                   [((k-2 (v-2 t-2)) ...) table2])
       (not (syntax-equal? (syntax-retrieve #'var #'((k-1 v-1) ...))
                      (syntax-retrieve #'var #'((k-2 v-2) ...))))
       #false)]
    [(_ rest ...) (duplicate-var-subst? table1 table2 #'(rest ...))]))


(define (init-decomposition-table params)
  (syntax-case params ()
    [() params]
    [((var (name t ...)) rest ...)
     (syntax-cons #`(var (#,(make-pattern #'name #'var) (name t ...)))
                  (init-decomposition-table #'(rest ...)))]
    [((var name) rest ...)
     (syntax-cons #`(var (#,(make-pattern #'name #'var) name))
                  (init-decomposition-table #'(rest ...)))]))


(define (generate-metafunction-body name params body dts)
  (with-syntax ([((var type) ...) params])
    (with-syntax ([((table body extras) ...)
                   (reconcile-function-decomposition
                    (decompose-function-body body (init-decomposition-table #'((var type) ...)) dts #t)
                    #'(var ...))])
      (syntax-map
       (λ (x) (with-syntax ([(((k (v t)) ...) (b) (e ...)) x])
                (syntax-subst #'((k v) ...) #`([#,name var ...] b e ...))))
       #'((table body extras) ...)))))
  
  

;;-------------------------------------------------------------------------------------
;Redex judgment form manipulation


;; generates the judgment form used to satisfy redex check variables
(define (generate-judgment-form params)
  #`(define-judgment-form #%TIP
      #:mode (#%check #,@(generate-mode params))
      #:contract (#%check #,@(generate-contract params))
      #,(generate-judgment-body params)))


(define (generate-mode params)
  (syntax-case params ()
    [() params]
    [((_ _) rest ...)
     (syntax-append #'(I I) (generate-mode #'(rest ...)))]))


(define (generate-contract params)
  (syntax-case params ()
    [() params]
    [((_ _) rest ...)
     (syntax-append #'(#%dt #%t)
                    (generate-contract #'(rest ...)))]))


(define (generate-judgment-body params)
  (syntax-case params ()
    [((var _) ...)
     (with-syntax ([((dt t) ...)
                    (syntax-map
                     (λ (x) #`(#,(make-pattern #'#%dt x) #,(make-pattern #'#%t x)))
                     #'(var ...))])
       (syntax-append
        (generate-premises #'((dt t) ...))
        #`[-------------------------
          (#%check #,@(generate-conclusion #'((dt t) ...)))]))]))


(define (generate-premises patterns)
  (with-syntax ([((dt t) ...) patterns])
    #'((#%isType dt t) ...)))


(define (generate-conclusion patterns)
  (syntax-case patterns ()
    [() patterns]
    [((dt t) rest ...)
     (syntax-append #'(dt t) (generate-conclusion #'(rest ...)))]))
                                      


;; generates relation used to type-check a given expression
(define (generate-relation dts)
  (with-syntax ([(((dt par ...) (ct cts ...) ...) ...) dts])
    #`(define-relation #%TIP
        #%isType ⊆ #%dt × #%t
        [(#%isType Int Int-t)]
        [(#%isType Bool Bool-t)]
        #,@(generate-relation-body dts))))


(define (generate-relation-body dts)
  (syntax-case dts ()
    [() dts]
    [(((dt par ...) (ct cts ...) ...) rest ...)
     (syntax-case (generate-relation-clauses #'((dt par ...) (ct cts ...) ...)) (#%isType)
       [[(#%isType s ...)] (syntax-cons #'[(#%isType s ...)] (generate-relation-body #'(rest ...)))]
       [([(multiple ...) ...] ...)
         (syntax-append #'([(multiple ...) ...] ...)
                        (generate-relation-body #'(rest ...)))])]))


(define (generate-relation-clauses dt)
  (syntax-case dt ()
    [((dt) rest ...) #`[(#%isType dt #,(make-type #'dt))]]
    [((dt par ...)) #'()]
    [((dt par ...) (ct) rest ...)
     (with-syntax ([(t-pattern ...) (syntax-map (λ (x) #`#,(make-pattern #'#%t x)) #'(par ...))])
       (syntax-cons (syntax-subst #'((par t-pattern) ...)
                                  #`[(#%isType ct #,(format-relation-type #'(par ...) #'(dt par ...)))])
                    (generate-relation-clauses #'((dt par ...) rest ...))))]
    [((dt par ...) (ct ts ...) rest ...)
     (with-syntax ([(f-pats ...) (format-relation-constructor #'(par ...) #'(ts ...))]
                   [(f-ts ...) (format-relation-type #'(par ...) #'(ts ...))]
                   [(t-pattern ...)
                    (syntax-map (λ (x) #`#,(make-pattern #'#%t x)) #'(par ...))]) 
       (syntax-cons
        (syntax-subst #'((par t-pattern) ...)
                      #`[(#%isType (ct f-pats ...) #,(format-relation-type #'(par ...) #'(dt par ...)))
                         (#%isType f-pats f-ts) ...])
        (generate-relation-clauses #'((dt par ...) rest ...))))]))
                     
                     
(define (format-relation-type par type)
  (syntax-case type ()
    [(t ...) (syntax-map (λ (x) (format-relation-type par x)) type)]
    [t (syntax-find #'t par) type]
    [t #`#,(make-type type)]))


(define (format-relation-constructor par cs)
  (syntax-case cs ()
    [() cs]
    [((name _ ...) rest ...)
     (syntax-cons #`#,(make-pattern #'name #`#,(gensym))
                  (format-relation-constructor par #'(rest ...)))]
    [(t ts ...) (syntax-find #'t par) (syntax-cons #`#,(make-pattern #'#%dt #`#,(gensym))
                                                   (format-relation-constructor par #'(ts ...)))]
    [(t ts ...) (syntax-cons #`#,(make-pattern #'t #`#,(gensym))
                             (format-relation-constructor par #'(ts ...)))]))


;;---------------------------------------------------------------------------------------
;Redex check manipulation

;; collects the TIP benchmark goals with the following form:
;; ([polymorphic variables] [goal parameters] [goal body])
(define (consolidate-checks exprs)
  (syntax-case exprs (prove assert-not par forall)
    [() exprs]
    [((prove (par params (forall ((id type) ...) assertion))) exprs ...)
     (syntax-cons #'(params ((id type) ...) assertion)
                  (consolidate-checks #'(exprs ...)))]
    [((prove (forall ((id type) ...) assertion)) exprs ...)
     (syntax-cons #'(() ((id type) ...) assertion)
                  (consolidate-checks #'(exprs ...)))]
    [((assert-not rest ...) exprs ...) (consolidate-checks #'((prove rest ...) exprs ...))]
    [(expr exprs ...) (consolidate-checks #'(exprs ...))]))


(define (generate-redex-checks check-info)
  (with-syntax ([((par params assertion)) check-info])
    (generate-redex-check #'par #'params #'assertion)))


;; generates redex check 
(define (generate-redex-check par params body)
  (with-syntax ([((id type) ...) params])
    (with-syntax ([(var ...) (format-check-variables params)])
      (syntax-subst #'((id var) ...)
                    #`(redex-check #%TIP #,@(generate-judgment par params)
                                   #,(decompose-property body #f)
                                   #:attempts 2000)))))


(define (format-check-variables params)
  (syntax-case params ()
    [() params]
    [((id (t-name _ ...)) rest ...)
     (syntax-cons (make-pattern #'t-name #'id)
                  (format-check-variables #'(rest ...)))]
    [((id type) rest ...)
     (syntax-cons (make-pattern #'type #'id)
                  (format-check-variables #'(rest ...)))]))

(define (generate-judgment poly params)
  #`(#:satisfying (#%check #,@(format-judgment poly params))))

(define (format-judgment poly params)
  (syntax-case params ()
    [() params]
    [((var (t-id t ...)) rest ...)
     (syntax-append #`(var #,(format-judgment-poly poly #'(t-id t ...)))
                    (format-judgment poly #'(rest ...)))]
    [((var type) rest ...)
     (syntax-append #`(var #,(make-type #'type))
                    (format-judgment poly #'(rest ...)))]))


(define (format-judgment-poly poly types)
  (syntax-case types ()
    [() types]
    [((t-id t ...) ts ...)
     (syntax-cons (format-judgment-poly poly #'(t-id t ...))
                  (format-judgment-poly poly #'(ts ...)))]
    [(t ts ...) (syntax-cons (if (syntax-find #'t poly) (make-pattern #'#%t #'t) (make-type #'t))
                             (format-judgment-poly poly #'(ts ...)))]))


;; convers TIP benchmark goal into redex check property
(define (decompose-property body within-term?)
  (syntax-case body (let @ _ as exists match ite and or not => = distinct + - * div mod > >= < <=)
    [(_ expr type) (maybe-term-wrap #'expr (not within-term?))]
    [(as expr type) (maybe-term-wrap #'expr (not within-term?))]
    [(let rest ...) (error "This compiler does not support let bindings")]
    [(@ rest ...) (error "This compiler does not support first-class functions")]
    [(exists rest ...) (error "This compiler does not support existential quantifiers")]
    [(match rest ...) (error "This compiler does not support matching inside of asssertions")]
    [(or arg1 arg2)
     (with-syntax ([d-arg1 (decompose-property #'arg1 #f)]
                   [d-arg2 (decompose-property #'arg2 #f)])
      (maybe-unquote-wrap #'(or d-arg1 d-arg2) within-term?))]
    [(and arg1 arg2)
     (with-syntax ([d-arg1 (decompose-property #'arg1 #f)]
                   [d-arg2 (decompose-property #'arg2 #f)])
       (maybe-unquote-wrap #'(and d-arg1 d-arg2) within-term?))]
    [(not arg)
     (with-syntax ([d-arg (decompose-property #'arg #f)])
       (maybe-unquote-wrap #'(not d-arg) within-term?))]
    [(=> arg1 arg2)
     (with-syntax ([d-arg1 (decompose-property #'arg1 #f)]
                   [d-arg2 (decompose-property #'arg2 #f)])
       (maybe-unquote-wrap #'(implies d-arg1 d-arg2) within-term?))]
    [(ite cond true false)
     (with-syntax ([d-cond (decompose-property #'cond #f)]
                   [d-true (decompose-property #'true #f)]
                   [d-false (decompose-property #'false #f)])
       (maybe-unquote-wrap #'(if d-cond d-true d-false) within-term?))]
    [(= arg1 arg2)
     (with-syntax ([d-arg1 (decompose-property #'arg1 #f)]
                   [d-arg2 (decompose-property #'arg2 #f)])
       (maybe-unquote-wrap #'(equal? d-arg1 d-arg2) within-term?))]
    [(distinct arg1 arg2)
     (with-syntax ([d-arg1 (decompose-property #'arg1 #f)]
                   [d-arg2 (decompose-property #'arg2 #f)])
       (maybe-unquote-wrap #'(not (equal? d-arg1 d-arg2)) within-term?))]
    [(+ args ...)
     (with-syntax ([(d-args ...) (syntax-map (λ (x) (decompose-property x #f)) #'(args ...))])
       (maybe-unquote-wrap #'(+ d-args ...) within-term?))]
    [(- args ...)
     (with-syntax ([(d-args ...) (syntax-map (λ (x) (decompose-property x #f)) #'(args ...))])
       (maybe-unquote-wrap #'(- d-args ...) within-term?))]
    [(* args ...)
     (with-syntax ([(d-args ...) (syntax-map (λ (x) (decompose-property x #f)) #'(args ...))])
       (maybe-unquote-wrap #'(* d-args ...) within-term?))]
    [(div arg1 arg2)
     (with-syntax ([d-arg1 (decompose-property #'arg1 #f)]
                   [d-arg2 (decompose-property #'arg2 #f)])
       (maybe-unquote-wrap #'(quotient d-arg1 d-arg2) within-term?))]
    [(mod arg1 arg2)
     (with-syntax ([d-arg1 (decompose-property #'arg1 #f)]
                   [d-arg2 (decompose-property #'arg2 #f)])
       (maybe-unquote-wrap #'(modulo d-arg1 d-arg2) within-term?))]
    [(> arg1 arg2) (with-syntax ([d-arg1 (decompose-property #'arg1 #f)]
                                 [d-arg2 (decompose-property #'arg2 #f)])
                     (maybe-unquote-wrap #'(> d-arg1 d-arg2) within-term?))]
    [(>= arg1 arg2)
     (with-syntax ([d-arg1 (decompose-property #'arg1 #f)]
                   [d-arg2 (decompose-property #'arg2 #f)])
       (maybe-unquote-wrap #'(>= d-arg1 d-arg2) within-term?))]
    [(< arg1 arg2)
     (with-syntax ([d-arg1 (decompose-property #'arg1 #f)]
                   [d-arg2 (decompose-property #'arg2 #f)])
       (maybe-unquote-wrap #'(< d-arg1 d-arg2) within-term?))]
    [(<= arg1 arg2)
     (with-syntax ([d-arg1 (decompose-property #'arg1 #f)]
                   [d-arg2 (decompose-property #'arg2 #f)])
       (maybe-unquote-wrap #'(<= d-arg1 d-arg2) within-term?))]
    [(id args ...)
     (with-syntax ([(d-args ...) (syntax-map (λ (x) (decompose-property x #t)) #'(args ...))])
       (maybe-term-wrap #'(id d-args ...) (not within-term?)))]
    [id (maybe-term-wrap body (not within-term?))]))
    
       
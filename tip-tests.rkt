#lang racket

(require rackunit "tip-compiler.rkt" redex/reduction-semantics)

(provide syntax-check-equal?
         template-check
         meta-test-check-success
         meta-test-check-failure)

(define unimplemented-message "Tests have not been built")
(define unfinished-message "Tests have not been finished")

(define-simple-check (syntax-check-equal? x y)
  (equal? (syntax->datum x)
          (syntax->datum y)))

(define-check (template-check template expr)
  (with-check-info
   (['template template] ['expr (syntax->datum expr)])
   (letrec ([f (λ (template expr)
                 (match template
                   [(list _ ...)
                    (syntax-case expr ()
                      [(e ...) (= (length (syntax->list expr))
                                  (length template))
                               (for ([t (in-list template)]
                                     [e (in-list (syntax->list expr))])
                                 (f t e))]
                      [_ (fail-check "Incorrect template")])]
                   [(? symbol? (app symbol->string (regexp #rx"^\\|#(.*)\\|" (list _ type))))
                    (match (syntax-e expr)
                      [(? symbol? (app symbol->string (regexp #rx"(.*)_[a-zA-Z0-9]+" (list _ ex))))
                       (unless (equal? ex type) (fail-check "Incorrect template"))]
                      [_ (fail-check "Incorrect template")])]
                   [_
                    (syntax-case expr ()
                      [(e ...) (fail-check "Incorrect template")]
                      [id (unless (equal? template (syntax-e expr))
                            (fail-check "Incorrect template"))])]))])
     (f template expr))))


(define-check (meta-test-check-success t)
  (match (run-test (test-suite "" (t)))
    [(list (test-success _ _)) (void)]
    [(list (test-failure _ result))
     (fail-check "Exception running test that expected non-exception")
     ((current-check-handler) result)]
    [(list (test-error _ result))
     (fail-check "Error running test")
     ((current-check-handler) result)]
    [(list (test-result _)) (fail-check "Unexpected test-result type")]))


(define-check (meta-test-check-failure pred t)
  (match (run-test (test-suite "" (t)))
    [(list (test-success _ _)) (fail-check "No exception running test that expected an exception")]
    [(list (test-failure _ result))
     (unless (pred result)
       (fail-check "Predicate failed for meta test")
       ((current-check-handler) result))]
    [(list (test-error _ result))
     (fail-check "Error running test")
     ((current-check-handler) result)]
    [(list (test-result _)) (fail-check "Unexpected test-result type")]))
                     
      
;;----------------------------------------------------------------------------------
;Utility syntax name manipulation tests


(test-case
 "Remove underscores tests"
 (syntax-check-equal? #'(_ nil Int) #'(_ nil Int))
 (syntax-check-equal? (replace-underscores-in-exprs #'acc_plus)
                      #'acc-plus)
 (syntax-check-equal? (replace-underscores-in-exprs #'(= (acc_plus x y) (acc_plus y x)))
                      #'(= (acc-plus x y) (acc-plus y x)))
 (syntax-check-equal?
  (replace-underscores-in-exprs
   #'(define-language TIP
       [t Int-t Bool-t super_e-t]
       [dt Int Bool super_e]
       [Int integer]
       [Bool boolean]
       [super_e Man Boy (great super_e)]))
  #'(define-language TIP
      [t Int-t Bool-t super-e-t]
      [dt Int Bool super-e]
      [Int integer]
      [Bool boolean]
      [super-e Man Boy (great super-e)]))                                               
  )


(test-case
 "Remove annotations tests"
 (syntax-check-equal?
  (remove-annotations #'(define-fun-rec == :source Definitions.==
                          ((x Nat) (y Nat)) Bool
                          (match x
                            (case Z
                              (match y
                                (case Z true)
                                (case (S z) false)))
                            (case (S x2)
                              (match y
                                (case Z false)
                                (case (S y2) (== x2 y2)))))))
  #'(define-fun-rec == ((x Nat) (y Nat)) Bool
      (match x
        (case Z
          (match y
            (case Z true)
            (case (S z) false)))
        (case (S x2)
          (match y
            (case Z false)
            (case (S y2) (== x2 y2)))))))
 (syntax-check-equal?
  (remove-annotations #'(declare-datatypes (a) ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
                                                      (cons :source |Prelude.:| (head a) (tail (list a)))))))
  #'(declare-datatypes (a) ((list (nil) (cons (head a) (tail (list a)))))))
 
  (syntax-check-equal?
   (remove-annotations #'(define-fun-rec
                           (par (a) (elem :let :source Prelude.elem
                                          ((x a)(y (list a))) Bool
                                          (match y
                                            (case nil false)
                                            (case (cons z xs) (or (= z x) (elem x xs))))))))
   #'(define-fun-rec
       (par (a) (elem ((x a) (y (list a))) Bool
                      (match y
                        (case nil false)
                        (case (cons z xs) (or (= z x) (elem x xs))))))))
  (syntax-check-equal? (remove-annotations #'(declare-datatypes () ((Bin (One) (ZeroAnd (b Bin)) (OneAnd (c Bin))))))
                       #'(declare-datatypes () ((Bin (One) (ZeroAnd (b Bin)) (OneAnd (c Bin)))))))
                       
                                              
 
(test-case
 "Make type tests"
 (check-equal? (make-type #'Nat) 'Nat-t)
 (check-equal? (make-type #'list) 'list-t))

(test-case
 "Syntax cons tests"
 (syntax-check-equal? (syntax-cons #'r #'(e a l)) #'(r e a l))
 (syntax-check-equal? (syntax-cons #'[(le Nat_x Nat_y) (plus Nat_x Nat_y)]
                             #'([(le Z Nat_y) Z] [(le Nat_x Z) (S Z)]))
                #'([(le Nat_x Nat_y) (plus Nat_x Nat_y)]
                   [(le Z Nat_y) Z]
                   [(le Nat_x Z) (S Z)]))
 (syntax-check-equal? (syntax-cons #'(y (S Nat_z)) #'())
                #'((y (S Nat_z)))))
                

(test-case
 "Syntax append tests"
 (syntax-check-equal? (syntax-append #'((r a)) #'((z e)))
                #'((r a) (z e)))
 (syntax-check-equal? (syntax-append #'() #'(z e a l))
                #'(z e a l))
 )


(test-case
 "Syntax substitution tests"
 (check-exn #rx"bad syntax" (thunk (syntax-subst #'(x y) #'x)))
 (syntax-check-equal? (syntax-subst #'((x Nat_x) (y Z)) #'(le x y))
                      #'(le Nat_x Z))
 (syntax-check-equal? (syntax-subst #'((x (S Z)) (y ESC)) #'(if (= y ESC) (S z) (plus y y)))
                      #'(if (= ESC ESC) (S z) (plus ESC ESC)))
 (syntax-check-equal? (syntax-subst #'((z 11)) #'(if (< z z) z (+ z 11)))
                      #'(if (< 11 11) 11 (+ 11 11))))


(test-case
 "Syntax map tests"
 (syntax-check-equal? (syntax-map identity #'(a b c d))
                      #'(a b c d)))


(test-case
 "Term wrap tests"
 (syntax-check-equal? (maybe-term-wrap #'(le Bin_x Bin_y) #t)
                #'(term (le Bin_x Bin_y)))
 (syntax-check-equal? (maybe-term-wrap #'(< 77 44) #f)
                #'(< 77 44)))


(test-case
 "Unquote wrap tests"
 (syntax-check-equal? (maybe-unquote-wrap #'(le Nat_x Nat_y) #f)
                #'(le Nat_x Nat_y))
 (syntax-check-equal? (maybe-unquote-wrap #'(+ (term 77) (term (toNat (S Z)))) #t)
                #',(+ (term 77) (term (toNat (S Z))))))


;;----------------------------------------------------------------------------------
;Syntax searching functions tests


(test-case
 "Syntax find tests"
 (check-false (syntax-find #'x #'()))
 (check-false (syntax-find #'y #'(z Nat x)))
 (check-not-false (syntax-find #'x #'(x y z)))
 (check-not-false (syntax-find #'cons #'(nil cons)))
 (syntax-check-equal? #'x (syntax-find #'x #'(x y z)))
 (with-syntax ([x #'Nat])
   (check-eq? #'x (syntax-find #'Nat #'(x Bool Int Token)))))


(test-case
 "Syntax retrieve tests"
 (check-false (syntax-retrieve #'x #'()))
 (check-false (syntax-retrieve #'y #'((x y) (z Token))))
 (syntax-check-equal? (syntax-retrieve #'x #'((x ESC)))
                      #'ESC)
 (syntax-check-equal? (syntax-retrieve #'y #'((x Z) (y (cons (S (S Z)) (cons Z nil)))))
                      #'(cons (S (S Z)) (cons Z nil)))
 (syntax-check-equal? (syntax-retrieve #'x #'((x (cons ESC nil)) (x P)))
                      #'(cons ESC nil))
 (check-exn #rx"bad syntax" (thunk (syntax-retrieve #'x #'(x y z w))))
 )




(test-case
 "Syntax replace key tests"
 (syntax-check-equal? (syntax-replace #'() #'x #'(S Z))
                      #'((x (S Z))))
 (syntax-check-equal? (syntax-replace #'((x Nat_x) (x Z)) #'x #'(S (S Z)))
                      #'((x (S (S Z))) (x Z)))
 (syntax-check-equal? (syntax-replace #'((y Token_y) (z ESC)) #'w #'(ZeroAnd (OneAnd One)))
                      #'((y Token_y) (z ESC) (w (ZeroAnd (OneAnd One)))))
 )




;;---------------------------------------------------------------------------------
;Redex define-language manipulation tests

(test-case
 "Consolidate datatypes tests"
 (syntax-check-equal? #'(((Nat) (Z) (S Nat))
                   ((Bin) (One) (ZeroAnd Bin) (OneAnd Bin)))
                (consolidate-datatypes
                 #'((declare-datatypes () ((Nat (Z) (S (p Nat)))))
                   (declare-datatypes () ((Bin (One) (ZeroAnd (proj1-ZeroAnd Bin)) (OneAnd (proj1-OneAnd Bin)))))
                   (define-fun-rec
                     s ((x Bin)) Bin
                     (match x
                       (case One (ZeroAnd One))
                       (case (ZeroAnd xs) (OneAnd xs))
                       (case (OneAnd ys) (ZeroAnd (s ys)))))
                   (define-fun-rec
                     plus ((x Nat)(y Nat)) Nat
                     (match x
                       (case Z y)
                       (case (S z) (S (plus z y)))))
                   (define-fun-rec
                     toNat ((x Bin)) Nat
                     (match x
                       (case One (S Z))
                       (case (ZeroAnd xs) (plus (toNat xs) (toNat xs)))
                       (case (OneAnd ys) (plus (plus (S Z) (toNat ys)) (toNat ys)))))
                   (prove
                    (forall ((n Bin)) (= (toNat (s n)) (plus (S Z) (toNat n))))))))
 (syntax-check-equal? #`[((pair a b) (pair2 a b))
                         ((list a) (nil) (cons a (list a)))]
                      (consolidate-datatypes
                       #'((declare-datatypes
                           (a b) ((pair (pair2 (proj1-pair a) (proj2-pair b)))))
                          (declare-datatypes
                           (a) ((list (nil) (cons (head a) (tail (list a))))))
                          (define-fun-rec
                            (par (a) (select ((x a)(y (list (pair a (list a))))) (list (pair a (list a)))
                                             (match y
                                               (case nil (_ nil (pair a (list a))))
                                               (case (cons z x2)
                                                 (match x
                                                   (case (pair2 y2 ys)
                                                     (cons (pair2 y2 (cons x ys)) (select x x2)))))))))
                          
                                                                      
                       (define-fun-rec
                         (par (a) (select2 ((x (list a))) (list (pair a (list a)))
                                           (match x
                                             (case nil (_ nil (pair a (list a))))
                                             (case (cons y xs) (cons (pair2 y xs) (select y (select2 xs))))))))
                       (define-fun-rec
                         (par (a b) (map ((f (=> a b)) (x (list a))) (list b)
                                         (match x
                                           (case nil (_ nil b))
                                           (case (cons y xs) (cons (@ f y) (map f xs)))))))
                       (prove
                        (par (b) (forall ((xs (list b))) (= (map (lambda ((x (pair b (list b)))) (match x (case (pair2 y z) y)))
                                                                 (select2 xs))
                                                            xs))))))))

(test-case
 "Generate language tests"
 (syntax-check-equal? (generate-language #'(Nat-t) #'((Nat Z (S Nat))))
                      #'(define-language #%TIP
                          [#%t Int-t Bool-t Nat-t]
                          [#%dt Int Bool Nat]
                          [Int integer]
                          [Bool boolean]
                          [Nat Z (S Nat)]))
 (syntax-check-equal? (generate-language #'() #'())
                      #'(define-language #%TIP
                          [#%t Int-t Bool-t]
                          [#%dt Int Bool]
                          [Int integer]
                          [Bool boolean]))
 (syntax-check-equal? (generate-language #'(Bin-t (list-t #%t))
                                         #'((Bin One (ZeroAnd Bin) (OneAnd Bin))
                                            (list nil (cons #%dt list))))
                      #'(define-language #%TIP
                          [#%t Int-t Bool-t Bin-t (list-t #%t)]
                          [#%dt Int Bool Bin list]
                          [Int integer]
                          [Bool boolean]
                          [Bin One (ZeroAnd Bin) (OneAnd Bin)]
                          [list nil (cons #%dt list)])))



;;---------------------------------------------------------------------------------
;Redex metafunction manipulation tests

(test-case
 "Consolidate functions tests"
 (syntax-check-equal? #'[((a) interleave ((x (list a))(y (list a))) (list a)
                              (match x (case nil y) (case (cons z xs) (cons z (interleave y xs)))))
                         ((a) evens ((x (list a))) (list a) (match x (case nil (_ nil a)) (case (cons y xs) (cons y (odds xs)))))
                         ((a) odds ((x (list a))) (list a) (match x (case nil (_ nil a)) (case (cons y xs) (evens xs))))]
                      (consolidate-functions
                       #'((declare-datatypes (a) ((list (nil) (cons (head a) (tail (list a))))))
                          (define-fun-rec
                            (par (a) (interleave ((x (list a)) (y (list a))) (list a)
                                                 (match x
                                                   (case nil y)
                                                   (case (cons z xs) (cons z (interleave y xs)))))))
                          (define-funs-rec
                            ((par (a) (evens ((x (list a))) (list a)))
                             (par (a) (odds ((x (list a))) (list a))))
                            ((match x
                               (case nil (_ nil a))
                               (case (cons y xs) (cons y (odds xs))))
                             (match x
                               (case nil (_ nil a))
                               (case (cons y xs) (evens xs)))))
                          (prove (par (a) (forall ((xs (list a))) (= (interleave (evens xs) (odds xs)) xs)))))))

 (syntax-check-equal? #'[((a b) zip ((x (list a)) (y (list b))) (list (pair a b))
                                (match x
                                  (case nil (_ nil (pair a b)))
                                  (case (cons z x2)
                                    (match y
                                      (case nil (_ nil (pair a b)))
                                      (case (cons x3 x4) (cons (pair2 z x3) (zip x2 x4)))))))
                         ((a b) zipConcat ((x a) (y (list a)) (z (list b))) (list (pair a b))
                                (match z
                                  (case nil (_ nil (pair a b)))
                                  (case (cons y2 ys) (cons (pair2 x y2) (zip y ys)))))]
                      (consolidate-functions
                       #'((declare-datatypes (a b) ((pair (pair2 (proj1-pair a) (proj2-pair b)))))
                          (declare-datatypes (a) ((list (nil) (cons (head a) (tail (list a))))))
                          (define-fun-rec
                            (par (a b) (zip ((x (list a)) (y (list b))) (list (pair a b))
                                            (match x
                                              (case nil (_ nil (pair a b)))
                                              (case (cons z x2)
                                                (match y
                                                  (case nil (_ nil (pair a b)))
                                                  (case (cons x3 x4) (cons (pair2 z x3) (zip x2 x4)))))))))
                          (define-fun
                            (par (a b) (zipConcat ((x a) (y (list a)) (z (list b))) (list (pair a b))
                                                  (match z
                                                    (case nil (_ nil (pair a b)))
                                                    (case (cons y2 ys) (cons (pair2 x y2) (zip y ys)))))))
                          (prove
                           (par (a b) (forall ((x a) (xs (list a)) (ys (list b)))
                                              (= (zip (cons x xs) ys) (zipConcat x xs ys)))))))))

(test-case
 "Generate-metafunction tests"
 (template-check '(define-metafunction #%TIP
                    acc-plus : Nat Nat -> Nat
                    [(acc-plus Z Nat_y) Nat_y]
                    [(acc-plus (S \|#Nat\|) Nat_y) (acc-plus \|#Nat\| (S Nat_y))])
                 (generate-metafunction #'() #'acc-plus #'((x Nat)(y Nat)) #'Nat
                                        #'(((Nat) (Z) (S Nat)))
                                        #'(match x
                                            (case Z y)
                                            (case (S z) (acc-plus z (S y)))))))

(test-case
 "Decompose function body tests"
 (check-exn #rx"let bindings"
           (thunk (decompose-function-body #'(let ((y2 (ite (le y z) z y))) (ite (le x y2) x y2))
                                           #'((x (Nat_x Nat)) (y (Nat_y Nat)) (z (Nat_z Nat)))
                                           #'(((Nat) (Z) (S Nat)))
                                           #true)))
 (check-exn #rx"first-class functions"
            (thunk (decompose-function-body #'(lambda ((y2 a)) (not (@ (@ x z) y2)))
                                            #'((y (list_y (list a))))
                                            #'(((list a) (nil) (cons a (list a))))
                                            #true)))
 )



(test-case
 "Mix bindings tests"
 (syntax-check-equal? (mix-bindings #'Nat_x #'(S Z))
                      #'(S Z))
 (syntax-check-equal? (mix-bindings #'(S (S (S Nat_y))) #'(S (S Nat_x)))
                      #'(S (S (S Nat_y))))
 (syntax-check-equal? (mix-bindings #'(cons Token_y list_xs) #'(cons ESC list_xs))
                      #'(cons ESC list_xs)))


(test-case
 "Expression conflict tests"
 (check-false (expr-conflict? #'Token_x #'ESC))
 (check-false (expr-conflict? #'(ZeroAnd One) #'Bin_z))
 (check-true (expr-conflict? #'Z #'(S (S Z))))
 (check-false (expr-conflict? #'(cons 44 (cons 22 nil))
                             #'(cons Int_x (cons 22 nil))))
 (check-true (expr-conflict? #'(cons false nil) #'(cons true nil))))

;;---------------------------------------------------------------------------------
;Redex judgment form manipulation tests


(test-case
 "Generate judgment form tests"
 (syntax-check-equal? (generate-judgment-form #'((x Nat)))
                #'(define-judgment-form #%TIP
                    #:mode (#%check I I)
                    #:contract (#%check #%dt #%t)
                    [(#%isType #%dt_x #%t_x)
                     -------------------------
                     (#%check #%dt_x #%t_x)]))
 (syntax-check-equal? (generate-judgment-form #'((x (list a)) (y Nat)))
                #'(define-judgment-form #%TIP
                    #:mode (#%check I I I I)
                    #:contract (#%check #%dt #%t #%dt #%t)
                    [(#%isType #%dt_x #%t_x)
                     (#%isType #%dt_y #%t_y)
                     -------------------------
                     (#%check #%dt_x #%t_x #%dt_y #%t_y)])))
                     
                                        

(test-case
 "Generate mode tests"
 (syntax-check-equal? (generate-mode #'((x Nat)))
                      #'(I I))
 (syntax-check-equal? (generate-mode #'((x Token) (y Token)))
                      #'(I I I I))
 (syntax-check-equal? (generate-mode #'((z (list a))))
                      #'(I I))
 (check-exn #rx"bad syntax" (thunk (generate-mode #'(x Token)))))


(test-case
 "Generate contract tests"
 (syntax-check-equal? (generate-contract #'((x Nat)))
                                         #'(#%dt #%t))
 (syntax-check-equal? (generate-contract #'((x Token) (y Token) (z (list a))))
                      #'(#%dt #%t #%dt #%t #%dt #%t))
 (check-exn #rx"bad syntax" (thunk (generate-contract #'(n Int)))))
 

(test-case
 "Generate judgment body tests"
 (syntax-check-equal? (generate-judgment-form #'((x Nat)))
                      #'(define-judgment-form #%TIP
                          #:mode (#%check I I)
                          #:contract (#%check #%dt #%t)
                          [(#%isType #%dt_x #%t_x)
                           -------------------------
                           (#%check #%dt_x #%t_x)]))
 (syntax-check-equal? (generate-judgment-form #'((x Token)(y (list a))))
                      #'(define-judgment-form #%TIP
                          #:mode (#%check I I I I)
                          #:contract (#%check #%dt #%t #%dt #%t)
                          [(#%isType #%dt_x #%t_x)
                           (#%isType #%dt_y #%t_y)
                           -------------------------
                           (#%check #%dt_x #%t_x #%dt_y #%t_y)])))




(test-case
 "Generate relation tests"
 (syntax-check-equal? (generate-relation #'())
                      #'(define-relation #%TIP
                          #%isType ⊆ #%dt × #%t
                          [(#%isType Int Int-t)]
                          [(#%isType Bool Bool-t)]))
 (template-check '(define-relation #%TIP
                    #%isType ⊆ #%dt × #%t
                    [(#%isType Int Int-t)]
                    [(#%isType Bool Bool-t)]
                    [(#%isType (pair2 \|##%dt\| \|##%dt\|) (pair-t #%t_a #%t_b))
                     (#%isType \|##%dt\| #%t_a)
                     (#%isType \|##%dt\| #%t_b)]
                    [(#%isType nil (list-t #%t_a))]
                    [(#%isType (cons \|##%dt\| \|#list\|) (list-t #%t_a))
                     (#%isType \|##%dt\| #%t_a)
                     (#%isType \|#list\| (list-t #%t_a))])
                 (generate-relation #'[((pair a b) (pair2 a b))
                                       ((list a) (nil) (cons a (list a)))])))
                                         



;;--------------------------------------------------------------------------------
;Redex check manipulation tests

(test-case
 "Consolidate checks tests"
 (syntax-check-equal? #'((() ((xs (list Token)) (ys (list Token))) (=> (= (escape xs) (escape ys)) (= xs ys))))
                (consolidate-checks
                 #'((declare-datatypes (a) ((list (nil) (cons (head a) (tail (list a))))))
                    (declare-datatypes () ((Token (A) (B) (C) (D) (ESC) (P) (Q) (R))))
                    (define-fun
                      isSpecial ((x Token)) Bool
                      (match x
                        (case default false)
                        (case ESC true)
                        (case P true)
                        (case Q true)
                        (case R true)))
                    (define-fun
                      code ((x Token)) Token
                      (match x
                        (case default x)
                        (case ESC ESC)
                        (case P A)
                        (case Q B)
                        (case R C)))
                    (define-fun-rec
                      escape ((x (list Token))) (list Token)
                      (match x
                        (case nil (_ nil Token))
                        (case (cons y xs)
                          (ite
                           (isSpecial y) (cons ESC (cons (code y) (escape xs)))
                           (cons y (escape xs))))))
                    (prove
                     (forall ((xs (list Token)) (ys (list Token))) (=> (= (escape xs) (escape ys)) (= xs ys)))))))
 (syntax-check-equal? #'[((a) ((x (list a))) (= (rev (qrev x (_ nil a))) x))]
                      (consolidate-checks
                       #'((declare-datatypes (a) ((list (nil) (cons (head a) (tail (list a))))))
                          (define-fun-rec
                            (par (a)
                                 (qrev ((x (list a)) (y (list a))) (list a)
                                       (match x
                                         (case nil y)
                                         (case (cons z xs) (qrev xs (cons z y)))))))
                          (define-fun-rec
                            (par (a)
                                 (++ ((x (list a)) (y (list a))) (list a)
                                     (match x
                                       (case nil y)
                                       (case (cons z xs) (cons z (++ xs y)))))))
                          (define-fun-rec
                            (par (a)
                                 (rev ((x (list a))) (list a)
                                      (match x
                                        (case nil (_ nil a))
                                        (case (cons y xs) (++ (rev xs) (cons y (_ nil a))))))))
                          (prove
                           (par (a) (forall ((x (list a))) (= (rev (qrev x (_ nil a))) x))))))))

(test-case
 "Generate redex check tests"
 (syntax-check-equal?
  (generate-redex-check
   #'() #'((x Nat)(y Nat)(z Nat))
   #'(= (formula-pow x (plus y z))
        (times (formula-pow2 x y) (formula-pow3 x z))))
  #'(redex-check #%TIP #:satisfying (#%check Nat_x Nat-t Nat_y Nat-t Nat_z Nat-t)
                 (equal? (term (formula-pow Nat_x (plus Nat_y Nat_z)))
                         (term (times (formula-pow2 Nat_x Nat_y) (formula-pow3 Nat_x Nat_z))))
                 #:attempts 2000))
 (check-exn
  #rx"does not support"
  (thunk
   (generate-redex-check
    #'() #'((x Nat)(y Nat)(z Nat))
    #'(= (let ((y2 (ite (le y z) z y))) (ite (le x y2) x y2))
         (ite (le x z)
              (@ (let ((x3 (ite (le x y) x y)))
                   (lambda ((y4 Nat)) (ite (le x3 y4) y4 x3)))
                 x)
              (@ (let ((x2 (ite (le x y) x y)))
                   (lambda ((y3 Nat)) (ite (le x2 y3) y3 x2)))
                 z)))))))



(test-case
 "Decompose property tests"
 (syntax-check-equal? (decompose-property #'(_ nil Int) #f)
                      #'(term nil))
 (syntax-check-equal? (decompose-property #'(_ nil Token) #t)
                      #'nil)
 (syntax-check-equal? (decompose-property #'(as nil (list a)) #f)
                      #'(term nil))
 (syntax-check-equal? (decompose-property #'(as nil (list Bool)) #t)
                      #'nil)
 (check-exn #rx"does not support"
            (thunk (decompose-property #'(let ((k (nmsorttd-half1 (length x))))
                                    (lmerge (nmsorttd (take k x)) (nmsorttd (drop k x))))
                                #t)))
 (check-exn #rx"let bindings"
            (thunk (decompose-property #'(let ((k (nmsorttd-half1 (length x))))
                                    (lmerge (nmsorttd (take k x) (nmsorttd (drop k x)))))
                                #f)))
 (check-exn #rx"does not support"
            (thunk (decompose-property #'(@ (lambda ((y4 Nat)) (ite (le x3 y4) x3 y4)) z) #f)))
 (check-exn #rx"first-class functions"
            (thunk (decompose-property #'(@ (lambda ((y4 Nat)) (ite (le x3 y4) x3 y4)) z) #t)))
 (check-exn #rx"does not support"
            (thunk (decompose-property #'(=> (elem x xs) (exists ((y Nat)) (= x (!! xs y)))) #f)))
 (check-exn #rx"existential quantifiers"
            (thunk (decompose-property #'(=> (elem x xs) (exists ((y Nat)) (= x (!! xs y)))) #t)))
 (check-exn #rx"does not support"
            (thunk (decompose-property #'(=> (not (match xs (case nil true) (case (cons y z) false)))
                                      (= (last (cons x xs)) (last xs))) #f)))
 (syntax-check-equal? (decompose-property #'(or (le (+ x y) y) (=> (= x y) true)) #f)
                      #'(or (term (le ,(+ (term x) (term y)) y)) (implies (equal? (term x) (term y)) (term true))))
 (syntax-check-equal? (decompose-property #'(or true (= z (= (or x z) false))) #t)
                      #',(or (term true) (equal? (term z) (equal? (or (term x) (term z)) (term false)))))
 (syntax-check-equal? (decompose-property #'(and (<= y y2) (ordered z)) #f)
                      #'(and (<= (term y) (term y2)) (term (ordered z))))
 (syntax-check-equal? (decompose-property #'(and (eps r) (eps q2)) #t)
                      #',(and (term (eps r)) (term (eps q2))))
 (syntax-check-equal? (decompose-property #'(=> (gt x y) (not (gt y x))) #f)
                      #'(implies (term (gt x y)) (not (term (gt y x)))))
 (syntax-check-equal? (decompose-property #'(=> (lt x y) (not (lt y x))) #t)
                      #',(implies (term (lt x y)) (not (term (lt y x)))))
 (syntax-check-equal? (decompose-property #'(=> (= (escape xs) (escape ys)) (= xs ys)) #f)
                      #'(implies (equal? (term (escape xs)) (term (escape ys))) (equal? (term xs) (term ys))))
 (syntax-check-equal? (decompose-property #'(=> (= (len xs) (len ys))
                                                (= (zip (rev xs) (rev ys)) (rev (zip xs ys))))
                                          #t)
                      #',(implies (equal? (term (len xs)) (term (len ys)))
                                 (equal? (term (zip (rev xs) (rev ys))) (term (rev (zip xs ys))))))
 

 (syntax-check-equal? (decompose-property #'(= (ite (le x y) x y) (ite (le y x) y x)) #f)
                      #'(equal? (if (term (le x y)) (term x) (term y))
                                (if (term (le y x)) (term y) (term x))))

 (syntax-check-equal? (decompose-property #'(ite (= x z) (plus (S Z) (count x ys)) (count x ys)) #t)
                      #',(if (equal? (term x) (term z)) (term (plus (S Z) (count x ys))) (term (count x ys))))

 (syntax-check-equal? (decompose-property #'(= (S (count n xs)) (count n (cons n xs))) #f)
                      #'(equal? (term (S (count n xs))) (term (count n (cons n xs)))))
 (syntax-check-equal? (decompose-property #'(= (drop (S n) (cons x xs)) (drop n xs)) #t)
                      #',(equal? (term (drop (S n) (cons x xs))) (term (drop n xs))))

 (syntax-check-equal? (decompose-property #'(=> (lt y x) (distinct x y)) #f)
                      #'(implies (term (lt y x)) (not (equal? (term x) (term y)))))

 (syntax-check-equal? (decompose-property #'(=> (distinct x y) (lt x y)) #t)
                      #',(implies (not (equal? (term x) (term y))) (term (lt x y))))
 (syntax-check-equal? (decompose-property #'(+ 1 (nmsorttd-half1 (- x 2))) #f)
                      #'(+ (term 1) (term (nmsorttd-half1 ,(- (term x) (term 2))))))
 (syntax-check-equal? (decompose-property #'(+ (third (- x 3)) 1) #t)
                      #',(+ (term (third ,(- (term x) (term 3)))) (term 1)))
 (syntax-check-equal? (decompose-property #'(- x 1) #f)
                      #'(- (term x) (term 1)))
 (syntax-check-equal? (decompose-property #'(- (- x 2) (minus 43 y)) #t)
                      #',(- (- (term x) (term 2)) (term (minus 43 y))))
 (syntax-check-equal? (decompose-property #'(* x x) #f)
                      #'(* (term x) (term x)))
 (syntax-check-equal? (decompose-property #'(* x 11) #t)
                      #',(* (term x) (term 11)))
 (syntax-check-equal? (decompose-property #'(div (length x) 3) #f)
                      #'(quotient (term (length x)) (term 3)))
 (syntax-check-equal? (decompose-property #'(div (+ (* 2 (length x)) 1) 3) #t)
                      #',(quotient (+ (* (term 2) (term (length x))) (term 1)) (term 3)))
 (syntax-check-equal? (decompose-property #'(mod x 2) #f)
                      #'(modulo (term x) (term 2))))
 
 
 
;;----------------------------------------------------------------------------------------------
;Meta-testing (a.k.a testing test utilties)


(test-case
 "Syntax-check-equal? tests"
 (meta-test-check-success (thunk (syntax-check-equal? #'() #'())))
 (meta-test-check-success (thunk (syntax-check-equal? #'56 #'56)))
 (meta-test-check-failure exn? (thunk (syntax-check-equal? #'(cons Z nil) #'(cons (S Z) nil))))
 )


(test-case
 "Template check tests"
 (meta-test-check-success (thunk (template-check '55 #'55)))
 (meta-test-check-success (thunk (template-check '\|#Token\| #'Token_x)))
 (meta-test-check-success (thunk (template-check '|-2| #'|-2|)))
 (meta-test-check-success (thunk (template-check '(plus Z \|#Nat\|) #'(plus Z Nat_g342367659))))
 (meta-test-check-failure (λ (x) (regexp-match? #rx"Incorrect template" (exn-message x)))
                          (thunk (template-check '\|#list\| #'Nat_zz)))
 (meta-test-check-failure (λ (x) (regexp-match? #rx"Incorrect template" (exn-message x)))
                          (thunk (template-check 'real #'thick))))

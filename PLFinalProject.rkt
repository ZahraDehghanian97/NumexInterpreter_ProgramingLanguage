;; PL Project - Fall 2018
;; NUMEX interpreter
#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct closure     (env f) #:transparent);; a closure is not in "source" programs; it is what functions evaluate to

(struct var      (string)               #:transparent)  ;; a variable, e.g., (var "foo")
(struct num      (int)                  #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool     (boolean)              #:transparent)  ;; a boolean
(struct plus     (e1 e2)                #:transparent)  ;; add two expressions
(struct minus    (e1 e2)                #:transparent)  ;; subtract two expressions
(struct mult     (e1 e2)                #:transparent)  ;; multiply two expressions
(struct div      (e1 e2)                #:transparent)  ;; divide two expressions
(struct neg      (e1)                   #:transparent)  ;; negative of an expression
(struct andalso  (e1 e2)                #:transparent)  ;; conjunct two expressions
(struct orelse   (e1 e2)                #:transparent)  ;; disjunct two expressions
(struct cnd      (e1 e2 e3)             #:transparent)  ;; if e1 then e2 else e3
(struct iseq     (e1 e2)                #:transparent)  ;; if e1 = e2 then true else false
(struct ifnzero  (e1 e2 e3)             #:transparent)  ;; if e1 != 0 then e2 else e3
(struct ifleq    (e1 e2 e3 e4)          #:transparent)  ;; if e1 > e2 then e4 else e3
(struct lam      (nameopt formal body)  #:transparent)  ;; a recursive(?) 1-argument function
(struct apply    (funexp actual)        #:transparent)  ;; function application
(struct with     (var e body)           #:transparent)  ;; a local binding (let var = e in body) 
(struct apair    (e1 e2)                #:transparent)  ;; make a new pair
(struct 1st      (e1)                   #:transparent)  ;; get first part of a pair
(struct 2nd      (e1)                   #:transparent)  ;; get second part of a pair
(struct munit    ()                     #:transparent)  ;; unit value -- good for ending a list
(struct ismunit  (e1)                   #:transparent)  ;; if e1 is unit then true else false
;; Problem 1


;;part a
(define (racketlist->numexlist xs) (if (null? xs) (munit) (apair (car xs)(racketlist->numexlist (cdr xs)))))

;;part b
(define (numexlist->racketlist xs) (if (munit? xs) '() (cons (apair-e1 xs)(numexlist->racketlist (apair-e2 xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))


(define (eval-under-env e env)
  
  (cond [(var? e) 
         (envlookup env (var-string e))]
        
        [(num? e)
         (if (integer? (num-int e)) e
             (error "not number " e))]

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;arithmatic part

        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "+ apply to non-number")))]

        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "- apply to non-number")))]


        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "* apply to non-number")))]

        [(div? e) 
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (if (equal? (num-int v2) 0)
                   (error "Division by zero .....")
                   (num (quotient (num-int v1)(num-int v2))))

               (error "/ apply to non-number")))]

        ;;;;;;;;;;;;;;;;;;boolean part
        [(bool? e)
         (if (boolean? (bool-boolean e)) e
             (error "not boolean" e))]

        [(andalso? e)
            (let ([v1 (eval-under-env (andalso-e1 e) env )])
              (if (bool? v1) (if (equal? (bool-boolean v1) #f)(bool #f)
                                  (let ([v2 (eval-under-env (andalso-e2 e) env)])
                                          (if (bool? v2)(bool (bool-boolean v2))(error "e2 is not bool"))))
                  (error "e1 is not bool")))]
                
         [(orelse? e)
            (let ([v1 (eval-under-env (orelse-e1 e) env )])
              (if (bool? v1) (if (equal? (bool-boolean v1) #t)(bool #t)
                                 (let ([v2 (eval-under-env (orelse-e2 e) env)])
                                   (if (bool? v2)(bool (bool-boolean v2)) (error "e2 is not bool"))))
                  (error "e1 is not bool")))]
         ;;;;;;;;;;;;;;;;others

        [(cnd? e)
          (let ([v1 (eval-under-env (cnd-e1 e) env)])
            (if (bool? v1)
                (if(equal? (bool-boolean v1) #t)
                   (eval-under-env (cnd-e2 e) env)
                   (eval-under-env (cnd-e3 e) env))
                (error "e1 is not a boolean"))
            )]

        
         [(neg? e) 
         (let ([v1 (eval-under-env (neg-e1 e) env)])
           (cond
             [(num? v1) (num (- (num-int v1)))]
             [(bool? v1) (if(false? (bool-boolean v1)) (bool #t) (bool #f))]
             [#t (error "e neither is bool or num")]))]

         
         [(iseq? e) 
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
           (cond
             [(and (num? v1)(num? v2))
                  (if (equal? (num-int v1)  (num-int v2)) (bool #t) (bool #f)) ]
             [(and (bool? v1)(bool? v2))
                  (if(eq? (bool-boolean v1)  (bool-boolean v2)) (bool #t) (bool #f)) ]
               [#t (bool #f)])) ]


          [(ifnzero? e)
           (let ([v1 (eval-under-env (ifnzero-e1 e) env)])
             (if (num? v1)
                 (if (equal? (num-int v1)  0)
                     (eval-under-env (ifnzero-e3 e) env)
                     (eval-under-env (ifnzero-e2 e) env))
                 (error "e1 is not num ")))]
          

          [(ifleq? e)
           (let ([v1 (eval-under-env (ifleq-e1 e) env)] [v2 (eval-under-env (ifleq-e2 e) env)])
              (if (and (num? v1) (num? v2)) (if (> (num-int v1) (num-int v2))
                                                (eval-under-env (ifleq-e4 e) env)
                                                (eval-under-env (ifleq-e3 e) env))
                  (error "e1/e2 is not num")))]

           [(with? e)
            (let ([v (eval-under-env (with-e e) env)])
                   (let ([ext-env (cons (cons (with-var e) v) env)]) (eval-under-env (with-body e) ext-env)))]

           
            [(apply? e)
             (let ([v1 (eval-under-env (apply-funexp e) env)]
                   [v2 (eval-under-env (apply-actual e) env)])
             (if (closure? v1)
                  (let* ([c-fun (closure-f v1)]
                         [c-env (closure-env v1)]
                         [ext-env-temp (cons (cons (lam-formal c-fun) v2) c-env)]
                         [ext-env (if (lam-nameopt c-fun)
                               (cons (cons (lam-nameopt c-fun) v1) ext-env-temp)
                                 ext-env-temp)])
                    (eval-under-env (lam-body c-fun) ext-env))
              (error "NUMEX call first-subexpression not closure")))]

             [(closure? e) e]

             [(lam? e)
              (closure env e)]


            ;;;;;;;;;;;;;;;;;;;;;;;;cons & cells
            
          [(apair? e)
            (let ([v1 (eval-under-env (apair-e1 e) env)]
              [v2 (eval-under-env (apair-e2 e) env)])
               (apair v1 v2))]

 
         [(1st? e)
           (let ([v (eval-under-env (1st-e1 e) env)])
             (if (apair? v)
                 (apair-e1 v)
                 (error "Numex first applied to non-pair")))]
         

         [(2nd? e)
          (let ([v (eval-under-env (2nd-e1 e) env)])
            (if (apair? v)
                (apair-e2 v)
                (error "Numex snd applied to non-pair")))]


         [(ismunit? e)
          (let ([v (eval-under-env (ismunit-e1 e) env)])
            (if (munit? v) (bool #t) (bool #f)))]
         
         [(munit? e)
         (munit)]

        ;;;;;;;;;;;;;;;;;;;;;;default case
         
        [#t (error (format "bad NUMEX expression: ~v" e))]))



;;  Problem 3

;;  part a

(define (ifmunit e1 e2 e3)  (cond [(ismunit e1) e2][#t e3]))

;(define (ifmunit e1 e2 e3)  (cnd (ismunit e1) e2 e3))


;;  part b

(define (with* bs e2)
  (if (null? bs)e2
      (with (caar bs) (cdar bs) (with* (cdr bs) e2))))



;; part c

(define (ifneq e1 e2 e3 e4) 
  (with* (list (cons "_x" e1) (cons "_y" e2))
         (cond [(iseq (var "_x") (var "_y")) e4][#t e3])))



;(define (ifneq e1 e2 e3 e4) 
 ; (with* (list (cons "_x" e1) (cons "_y" e2))
  ;       (cnd (iseq (var "_x") (var "_y")) e4 e3)))





;; Problem 4

;;  Part(a)

(define numex-filter (lam "numex-filter1" "x" ;func
                          (lam  "numex-filter2"  "y" ;list
                                            [cnd (ismunit (var "y")) (munit)
                                                 (ifnzero (apply (var "x") (1st (var "y")))
                                                                                (apair (apply (var "x") (1st (var "y"))) (apply (var "numex-filter2") (2nd (var "y"))))
                                                                                (apply (var "numex-filter2") (2nd (var "y"))))])))
                                                                        
;;  second-part

              
(define numex-all-gt
  (with "filter"  numex-filter
       (lam "numex-all-gt-1" "i"
       (apply numex-filter  (lam "lessthan" "x" (ifleq (var "x") (var "i") (num 0)(var "x")))) )))
              
    





;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

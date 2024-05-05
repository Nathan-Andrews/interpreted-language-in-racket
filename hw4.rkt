;; CSCE 314, Programming Languages, Homework 4

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)

(define (racketlist->mupllist l)
  (letrec ([f (lambda (l)
                   (if (null? l)
                       (munit)
                       (apair (car l) (f (cdr l)))))])
    (f l)))

(define (mupllist->racketlist l)
  (letrec ([f (lambda (l)
                (if (munit? l)
                    '()
                    (cons (apair-e1 l)
                          (f (apair-e2 l)))))])
    (f l)))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(int? e) e]
        [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(isgreater? e)
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
           (if (and (int? v1) (int? v2) (> (int-num v1) (int-num v2)))
               (int 1)
               (int 0)))]
        [(ifnz? e)
         (let ([v1 (eval-under-env (ifnz-e1 e) env)])
           (if (and (int? v1) (not (= (int-num v1) 0)))
               (eval-under-env (ifnz-e2 e) env)
               (eval-under-env (ifnz-e3 e) env)))]
        [(fun? e) (closure env e)]
        [(call? e) ;;finish this
         (let ([f1 (eval-under-env (call-funexp e) env)]
               [v1 (eval-under-env (call-actual e) env)])
           (if (not (closure? f1))
               (error "no closure found in function call")
               (eval-under-env (fun-body (closure-fun f1))
                               (if (equal? (fun-nameopt (closure-fun f1)) null)
                                   (cons
                                    (cons (fun-formal (closure-fun f1))
                                          v1)
                                    env)
                                   (cons (cons (fun-nameopt (closure-fun f1))
                                           f1)
                                     (cons
                                      (cons (fun-formal (closure-fun f1))
                                            v1)
                                      env))))))]
        [(mlet? e)
         (let ([v1 (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v1) env)))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(first? e)
         (let ([v1 (eval-under-env (first-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL first applied to non-pair")))]
        [(second? e)
         (let ([v1 (eval-under-env (second-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL second applied to non-pair")))]
        [(munit? e) e]
        [(ismunit? e) (if (munit? (eval-under-env (ismunit-e e) env))
                          (int 1)
                          (int 0))]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3)
  (ifnz (ismunit e1) e2 e3))

(define (mlet* bs e2)
  (letrec ([f (lambda (xs)
               (if (null? xs)
                   e2
                   (mlet (caar xs) (cdr (car xs)) (f (cdr xs)))))])
    (f bs)))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1 (mlet "_y" e2 (ifnz
                               (add
                                (isgreater (var "_x") (var "_y"))
                                (isgreater (var "_y") (var "_x")))
                               e4
                               e3))))

;; Problem 4

(define (mupl-filter f)
  (fun "rec" "e1" (ifnz (ismunit (var "e1"))
                           (munit)
                           (ifnz (call f (first (var "e1")))
                                 (apair (first (var "e1")) (call (var "rec") (second (var "e1"))))
                                 (call (var "rec") (second (var "e1")))))))

(define (mupl-all-gt i)
  (mupl-filter (fun null "x" (isgreater (var "x") i))))

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

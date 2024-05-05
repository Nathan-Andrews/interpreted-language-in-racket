#lang racket

(require "hw4.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 4 Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")
   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")
   (check-equal? (eval-exp (add (int 42) (int 10)))
                           (int 52)
                           "tests addition of two ints")
   (check-equal? (eval-exp (isgreater (int 20) (int 19)))
                 (int 1)
                 "tests is-greater")
   (check-equal? (eval-exp (ifnz (int 34) (int 3) (int -3)))
                 (int 3)
                 "tests ifnz with non-zero argument")
   (check-equal? (eval-exp (ifnz (int 0) (int 3) (int -3)))
                 (int -3)
                 "tests ifnz with zero argument")
   (check-equal? (eval-exp (ifnz (munit) (int 3) (int -3)))
                 (int -3)
                 "tests ifnz with non-int argument")
   (check-equal? (eval-exp (call (fun null "x" (add (var "x") (var "x"))) (int 3)))
                 (int 6)
                 "tests call on anonymous function")
   (check-equal? (eval-exp (call (fun "sum" "x" (add (var "x") (ifnz (var "x") (call (var "sum") (add (var "x") (int -1))) (int 0)))) (int 5)))
                 (int 15)
                 "tests call on recursive function")
   (check-equal? (eval-exp (mlet "x" (int 32) (mlet "y" (int 42) (add (var "x") (var "y")))))
                 (int 74)
                 "tests mlet variables")
   (check-equal? (eval-exp (first (apair (int 1) (int 2))))
                 (int 1)
                 "tests first")
   (check-equal? (eval-exp (second (apair (int 1) (int 2))))
                 (int 2)
                 "tests second")
   (check-equal? (eval-exp (ifmunit (munit) (int 5) (int 10)))
                 (int 5)
                 "tests ifmunit")
   (check-equal? (eval-exp (mlet "x" (int 5) (ifeq (int 5) (var "x") (int 42) (int 17))))
                 (int 42)
                 "tests ifeq")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 1)) (cons "y" (int 5)) (cons "z" (int 17))) (add (var "x") (add (var "y") (var "z")))))
                 (int 23)
                 "tests mlet*")
   (check-equal? (eval-exp (call (mupl-filter (fun null "x" (ifeq (var "x") (int 5) (int 1) (int 0)))) (apair (int 5) (apair (int 17) (apair (int 5) (munit))))))
                 (apair (int 5) (apair (int 5) (munit)))
                 "tests mupl-filter")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)

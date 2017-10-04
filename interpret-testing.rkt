#lang racket
(require compatibility/defmacro
         macro-debugger/stepper
         macro-debugger/expand)

(require racket/trace)
;; This code is translated from PCL
(require "interpret.scm")
(require "simpleParser.scm")
(require "lex.scm")
;; test-name make-parameter makes things dynamic
(define test-name (make-parameter null))

;; parameterize

(define-macro (check . forms)
  `(combine-results ,@(map (lambda (x) `(report-result ,x ',x test-name)) forms)))

(define-macro (deftest name-args . body)
  "Define a test function. Within a test function we can call other test functions
   or use 'check' to run individual test cases"
  `(define ,name-args
     (parameterize ((test-name (append (test-name) (list (car ',name-args)))))
       ,@body)))

(define (combine-results . forms)
  "Works like the and operator, but does not short circuit"
  (let ((result '#t)) ; If one of the tests turns out to be null, then result becomes null
    (map (lambda (x) (unless x (set! result '#f))) forms) result))

(define (report-result result form test-name)
  "Takes a result and the form to evaluate to check if it passes the test"
  (pretty-print (format "~a... ~a: ~a" (if result
                                           "pass"
                                           "FAIL") (test-name) form))
  result)

(define p1-a1  (parser "./tests/p1/test1.javaish"))
(define p1-a2  (parser "./tests/p1/test2.javaish"))
(define p1-a3  (parser "./tests/p1/test3.javaish"))
(define p1-a4  (parser "./tests/p1/test4.javaish"))
(define p1-a5  (parser "./tests/p1/test5.javaish"))
(define p1-a6  (parser "./tests/p1/test6.javaish"))
(define p1-a7  (parser "./tests/p1/test7.javaish"))
(define p1-a8  (parser "./tests/p1/test8.javaish"))
(define p1-a9  (parser "./tests/p1/test9.javaish"))
(define p1-a10 (parser "./tests/p1/test10.javaish"))
(define p1-a11 (parser "./tests/p1/test11.javaish"))
(define p1-a12 (parser "./tests/p1/test12.javaish"))
(define p1-a13 (parser "./tests/p1/test13.javaish"))
(define p1-a14 (parser "./tests/p1/test14.javaish"))
(define p1-a15 (parser "./tests/p1/test15.javaish"))
(define p1-a16 (parser "./tests/p1/test16.javaish"))
(define p1-a17 (parser "./tests/p1/test17.javaish"))
(define p1-a18 (parser "./tests/p1/test18.javaish"))
(define p1-a19 (parser "./tests/p1/test19.javaish"))
(define p1-a20 (parser "./tests/p1/test20.javaish"))
(define p1-a21 (parser "./tests/p1/test21.javaish"))
(define p1-a22 (parser "./tests/p1/test22.javaish"))
(define p1-a23 (parser "./tests/p1/test23.javaish"))
(define p1-a24 (parser "./tests/p1/test24.javaish"))
(define p1-a25 (parser "./tests/p1/test25.javaish"))
(define p1-a26 (parser "./tests/p1/test26.javaish"))
(define p1-a27 (parser "./tests/p1/test27.javaish"))
(define p1-a28 (parser "./tests/p1/test28.javaish"))

(define p2-a1  (parser "./tests/p2/test1.javaish"))
(define p2-a2  (parser "./tests/p2/test2.javaish"))
(define p2-a3  (parser "./tests/p2/test3.javaish"))
(define p2-a4  (parser "./tests/p2/test4.javaish"))
(define p2-a5  (parser "./tests/p2/test5.javaish"))
(define p2-a6  (parser "./tests/p2/test6.javaish"))
(define p2-a7  (parser "./tests/p2/test7.javaish"))
(define p2-a8  (parser "./tests/p2/test8.javaish"))
(define p2-a9  (parser "./tests/p2/test9.javaish"))
(define p2-a10 (parser "./tests/p2/test10.javaish"))
(define p2-a11 (parser "./tests/p2/test11.javaish"))
(define p2-a12 (parser "./tests/p2/test12.javaish"))
(define p2-a13 (parser "./tests/p2/test13.javaish"))
(define p2-a14 (parser "./tests/p2/test14.javaish"))
(define p2-a15 (parser "./tests/p2/test15.javaish"))
(define p2-a16 (parser "./tests/p2/test16.javaish"))
(define p2-a17 (parser "./tests/p2/test17.javaish"))
(define p2-a18 (parser "./tests/p2/test18.javaish"))
(define p2-a19 (parser "./tests/p2/test19.javaish"))
(define p2-a20 (parser "./tests/p2/test20.javaish"))

;; (define p3-a1  (parser "./tests/p3/test1.javaish"))

(deftest (test-normal-p1)
  (check (equal? (m-expr p1-a1) 150)
         (equal? (m-expr p1-a2) -39/11) ; changed to the correct value!
         (equal? (m-expr p1-a3) 10)
         (equal? (m-expr p1-a4) 16)
         (equal? (m-expr p1-a5) 220)
         (equal? (m-expr p1-a6) 5)
         (equal? (m-expr p1-a7) 6)
         (equal? (m-expr p1-a8) 10)
         (equal? (m-expr p1-a9) 5)
         (equal? (m-expr p1-a10) -39)
         ;; (equal? (m-expr p1-a11) 150) error before declareb
         ;; (equal? (m-expr p1-a12) 150) error before declare
         ;; (equal? (m-expr p1-a13) 150) error before assign
         ;; (equal? (m-expr p1-a14) 150) error redefining
         (equal? (m-expr p1-a15) 'true)
         (equal? (m-expr p1-a16) 100)
         (equal? (m-expr p1-a17) 'false)
         (equal? (m-expr p1-a18) 'true)
         (equal? (m-expr p1-a19) 128)
         (equal? (m-expr p1-a20) 12)))

(deftest (test-extra-p1)
  (check (equal? (m-expr p1-a21) 30)
         (equal? (m-expr p1-a22) 11)
         (equal? (m-expr p1-a23) 1106)
         (equal? (m-expr p1-a24) 12)
         (equal? (m-expr p1-a25) 16)
         (equal? (m-expr p1-a26) 72)
         (equal? (m-expr p1-a27) 21)
         (equal? (m-expr p1-a28) 164)))

(deftest (test-all-p1)
  (combine-results
   (test-normal-p1)
   (test-extra-p1)))

(deftest (test-all-p2)
  (check (equal? (m-expr p2-a1) 20)
         (equal? (m-expr p2-a2) 164)
         (equal? (m-expr p2-a3) 32)
         ;; (equal? (m-expr p2-a5) 2) undefined
         (equal? (m-expr p2-a6) 25)
         (equal? (m-expr p2-a7) 21)
         (equal? (m-expr p2-a8) 6)
         (equal? (m-expr p2-a9) -1)
         (equal? (m-expr p2-a10) 789)
         ;; (equal? (m-exp2-pr a11) 789) can't use an undefined variable
         ;; (equal? (m-exp2-pr a12) 789) variable is already defined
         ;; (equal? (m-exp2-pr a13) 789) Break called not inside anything
         (equal? (m-expr p2-a14) 12)
         (equal? (m-expr p2-a15) 125)
         (equal? (m-expr p2-a16) 110)
         (equal? (m-expr p2-a17) 2000400)
         (equal? (m-expr p2-a18) 101)
         ;; (equal? (m-exp2-pr a19) 19) try not inside a catch
         (equal? (m-expr p2-a20) 21)
         ))



(deftest (test-all)
  (combine-results (test-all-p2)
                   (test-all-p1)))

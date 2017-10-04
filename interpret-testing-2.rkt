#lang racket
(require compatibility/defmacro
         macro-debugger/stepper
         macro-debugger/expand)

(require racket/trace)
;; This code is translated from PCL
(require "interpret.scm")
(require "functionParser.scm")
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

(define p3-a1  (parser "./tests/p3/test1.javaish"))
(define p3-a2  (parser "./tests/p3/test2.javaish"))
(define p3-a3  (parser "./tests/p3/test3.javaish"))
(define p3-a4  (parser "./tests/p3/test4.javaish"))
(define p3-a5  (parser "./tests/p3/test5.javaish"))
(define p3-a6  (parser "./tests/p3/test6.javaish"))
(define p3-a7  (parser "./tests/p3/test7.javaish"))
(define p3-a8  (parser "./tests/p3/test8.javaish"))
(define p3-a9  (parser "./tests/p3/test9.javaish"))
(define p3-a10 (parser "./tests/p3/test10.javaish"))
(define p3-a11 (parser "./tests/p3/test11.javaish"))
(define p3-a12 (parser "./tests/p3/test12.javaish"))
(define p3-a13 (parser "./tests/p3/test13.javaish"))
(define p3-a14 (parser "./tests/p3/test14.javaish"))
(define p3-a15 (parser "./tests/p3/test15.javaish"))
(define p3-a16 (parser "./tests/p3/test16.javaish"))
(define p3-a17 (parser "./tests/p3/test17.javaish"))
(define p3-a18 (parser "./tests/p3/test18.javaish"))
(define p3-a19 (parser "./tests/p3/test19.javaish"))
(define p3-a20 (parser "./tests/p3/test20.javaish"))



(deftest (test-normal-p3)
  (check (equal? (m-expr-global p3-a1) 10)
         (equal? (m-expr-global p3-a2) 14)
         (equal? (m-expr-global p3-a3) 45)
         (equal? (m-expr-global p3-a4) 55)
         (equal? (m-expr-global p3-a5) 1)
         (equal? (m-expr-global p3-a6) 115) ; 210 not 115
         (equal? (m-expr-global p3-a7) 'true)
         (equal? (m-expr-global p3-a8) 20)
         (equal? (m-expr-global p3-a9) 24)
         (equal? (m-expr-global p3-a10) 2)
         (equal? (m-expr-global p3-a11) 35)
         ;; (equal? (m-expr-global p3-a12) 789) mismatched arguments
         (equal? (m-expr-global p3-a13) 90)
         (equal? (m-expr-global p3-a14) 69)
         (equal? (m-expr-global p3-a15) 87) ; 88 not 87
         (equal? (m-expr-global p3-a16) 64)
         ;; (equal? (m-expr-global p3-a17) 2000400) b out of scope
         (equal? (m-expr-global p3-a18) 125)
         (equal? (m-expr-global p3-a19) 100)
         (equal? (m-expr-global p3-a20) 2000400) ;; 2000000 not 2000400
         ))

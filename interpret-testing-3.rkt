#lang racket
(require compatibility/defmacro
         macro-debugger/stepper
         macro-debugger/expand)

(require racket/trace)
;; This code is translated from PCL
(require "interpret.scm")
(require "classParser.scm")
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

(define a1  (parser "./tests/p4/test1.javaish"))
(define a2  (parser "./tests/p4/test2.javaish"))

(define a7  (parser "./tests/p4/test7.javaish")) ; broke
(define a8  (parser "./tests/p4/test8.javaish")) ; broke
(define a6  (parser "./tests/p4/test6.javaish"))
(define a31  (parser "./tests/p4/test31.javaish"))
(define a32  (parser "./tests/p4/test32.javaish"))


(deftest (test-normal-p3)
  (check (equal? (m-expr-class a32 "B") 530)
         (equal? (m-expr-class a31 "A") 20)
         (equal? (m-expr-class a1 "A")  15)
         (equal? (m-expr-class a2 "A")  12)))

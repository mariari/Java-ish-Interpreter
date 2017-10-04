#lang racket

;; This is a hygenic version of the S/ macro a friend made for me
(require (for-syntax syntax/parse)
         racket/stxparam)

(begin-for-syntax
  ;; a symbol beginning with s/
  ;;  .function    the part of the symbol after s/
  (define-syntax-class s/
    #:attributes (function)
    (pattern x:id
             #:with (_ f) (regexp-match #rx"^[sS]/(.+)"
                                        (symbol->string (syntax-e #'x)))
             #:attr function (datum->syntax #'x
                                            (string->symbol (syntax-e #'f)))))

  ;; expand forms including s/function calls
  ;;  .out    expanded result
  (define-syntax-class (expand-s/ state-var)
    #:attributes (out)
    ; call to s/function
    (pattern (x:s/ (~var args (expand-s/ state-var)) ...)
             #:with state state-var
             #:attr out #'(x.function state args.out ...))
    ; normal call
    (pattern ((~var e (expand-s/ state-var)) ...)
             #:attr out #'(e.out ...))
    ; datum
    (pattern d #:attr out #'d))
  )

(define-syntax define/s
  (syntax-parser
    [(_ (name state args ...) body ...)
     #:with ((~var body/e (expand-s/ #'state)) ...) #'(body ...)
     #'(define (name state args ...)
         body/e.out ...)]))


(define (foo state x)
  (printf "[~a] x = ~a\n" state x))

(define/s (bar state)
  (s/foo 5))

(bar 'the-state)

;;  ONCΕ AGAIN


(require racket/match
         (for-syntax syntax/parse))

;; syntax: (define-s (fn arg ...) body ...)
;; defines a function fn that takes an additional state argument first
(define-syntax define-s
  (syntax-parser
    [(_ (fn arg ...) body ...)
     #:with state (datum->syntax this-syntax 'state) ; inject symbol s
     #'(define (fn state arg ...)
         (+s state body) ...)]))


;; syntax: (+s <state-var> <expr>)
;; insert state variable into calls beginning with s.
(define-syntax +s
  (syntax-parser
    ;; parse function calls beginning with s. using regexp
    [(_ state-var (fn arg ...))
     #:with (_ fn-w/o-s-str)
     (regexp-match #px"^s\\.(.+)" (symbol->string (syntax->datum #'fn)))

     ;; turn s.xyz => xyz
     ;; has to convert the string fn-w/o-s-str back into a symbol
     ;; and inject it into the scope of the call
     #:with fn-w/o-s (datum->syntax this-syntax
                                    (string->symbol (syntax->datum #'fn-w/o-s-str))
                                    this-syntax)

     ;; call with arg added first
     #'(fn-w/o-s state-var (+s state-var arg) ...)]

    ;; a non-"s." call
    [(_ state-var (e ...))
     #'((+s state-var e) ...)]

    ;; a datum (number, etc.)
    [(_ stat-var e) #'e]))




(define-s (thing)
  (s.displayln)
  (displayln (add1 state)))

(thing 3)

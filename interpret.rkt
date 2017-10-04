#lang racket

(require compatibility/defmacro
         macro-debugger/stepper
         macro-debugger/expand)
(require racket/trace)
(require srfi/1)

(require "classParser.scm")
(require "lex.scm")
(provide (all-defined-out))

(require threading) ; needs raco
(require (for-syntax threading))

;;; HOW TO IMPROVE---------------------------------------------------------------------------------------------
;; 1. break up each section into it's own file
;; 2. Make the returning of values be consistent, that way we don't have to do branching logic for m-values-val
;;    vs having to do a case lookup, or have a function that handles what you want!
;; 3. Create a better state type that passes in all the different CPS's and what not and abstracts away from the
;;    large argument lists, each function will end up taking more than what's needed, but that's fine
;; 4. instead of my S/ macro, use a proper state monad to pass around the state
;; PARAMETERS--------------------------------------------------------------------------------------------------

;; The default undefined symbol for a variable
(define undefined-sym 'undefined)

(define default-state (list (hash) (hash)))
(define default-state-global (list (hash)))

(define class-key-word 'class88)

;; THere is another function pattern-to-fn that must be at the bottom for drracket
;; THIS DISPATCH HASH SHOULD BE LOOKED AT FIRST AND IS AT THE BOTTOM!!!

;; Just a struct to hold our values in order to return multiple things
;; (m-values-var (m-values (hash) (fn-get 'a) 3))
;; Maybe make the struct into a hash later so if I add stuff I don't have to change everything

(struct m-values (state var val))

;;  clas :: (or clas #f) -> hash -> clas
(struct clas (parent vars/fns))
;; Helper macros----------------------------------------------------------------------------------------------

;; we need curr because macros aren't first clas so we can't (curry fn or)
;; but we can curry this way! (curr fn or)
(defmacro curr (fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((arg (gensym)))
    `(λ (,arg) (,fn ,@args ,arg))))

;; to curry 2 arguments, we can't apply so we have to do this
;; there is a hack to get around this, but it's computationally expensive
(defmacro curr-2 (fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((arg1 (gensym))
        (arg2 (gensym)))
    `(λ (,arg1 ,arg2) (,fn ,@args ,arg1 ,arg2))))


;; Not used
(define-macro (dλ . ds)
  "A macro that changes the function that is called based on the arguments given to it"
  (let ((args (gensym)))
    `(λ ,args
       (case (car ,args)
         ,@(map (λ (d)
                  `(,(if (eq? 'else (car d))
                         'else
                         (list (car d)))
                    (apply (λ ,@(cdr d))
                           ,(if (eq? 'else (car d))
                                args
                                `(cdr ,args)))))
                ds)))))


;; ((dλ (inc (x) (+ 1 x)) (dec (x) (- 1 x)) (else (x) 3)) 2)
;; ((dλ (inc (x) (+ 1 x)) (dec (x) (- 1 x)) (else (x) 3)) 'inc 2)

;;  begin-for-sytnax allows us to s/-symbol? in our defmacro
(begin-for-syntax
  (define (s/-symbol? s)
    "Check whether or not if a symbol is a string symbol or not"
    (and (symbol? s)
       (~> s symbol->string string-length (> 2))
       (~> s symbol->string (substring 0 2) string-upcase (string=? "S/")))))





(begin-for-syntax
  (define (s/-symbol-to-function s)
    (~> s symbol->string (substring 2) string->symbol)))


;; A pure version of aλ I did not write
(module a racket
        (define-syntax (aλ exp)
          (syntax-case exp ()
                       [(aλ args body ...)
                        (with-syntax ([self (datum->syntax exp 'self)])
                                     #'(letrec ([self (λ args body ...)])
                                         self))]))
        (provide aλ))

(define-macro (alambda parms . body)
  "a self referential λ... recursively call with self"
  `(letrec ((self (λ ,parms ,@body)))
     self))


(require (for-syntax racket/match
                     compatibility/defmacro
                     'a))

;; 
(define-macro (define/s name-args . body)
  "automatically inserts state right after the S/functaion call"
  `(define ,name-args
     ,@(map (aλ (x)
                (match x
                  [(list* (? s/-symbol? s-symb) bs) `(,(s/-symbol-to-function s-symb) state ,@(map self bs))]
                  [(list* a b)                       (map self x)]
                  [(? s/-symbol? s-symb)            `(curry ,(s/-symbol-to-function s-symb) state)]
                  [a                                a]))
            body)))


(define-macro (define-m name-args expr function-name arg1 arg2 arg3 . body)
  "Defines a m-expression and inserts the following variables into the scope
    EXPR is the name of the expression variable we wish to parse
    FUNCTION-NAME ARG1, ARG2 and ARG3
    can be seen like (FUNCTION-NAME ARG1 ARG2 ARG3) in expression"
  `(define/s ,name-args
     (let* [(sexp ,expr)
            (,function-name (car sexp))
            (,arg1          (cadr sexp))                                      ; always be at least 2 things in the sexp
            (,arg2          (unless (empty? (cddr sexp))
                                '()
                                (caddr sexp)))                                 ; there might not be a 3rd '(var z)
            (,arg3          (if (or (empty? (cddr sexp)) (empty? (cdddr sexp))) ; only appears in if statements
                                '()                                            ; give a default back of '() because we do a
                                (cadddr sexp)))]                               ; a check on it, sadly
       ,@body)))
;; Here is the non-macro version of m-var it's the same thing as m-var
;; (define (m-var expr state)
;;   (let* ((sexp (cadr (list m-var expr state)))
;;          (_ (car sexp))
;;          (name (cadr sexp))
;;          (bind (if (empty? (cddr sexp)) #f (caddr sexp)))
;;          (_
;;           (if (or (empty? (cddr sexp) (empty? (cdddr sexp))))
;;             #f
;;             (cdddar sexp))))
;;     ........
;; HELPER FUNCTIONS-------------------------------------------------------------------------------------------

(define ∘ compose)

;; state -> var -> val -> m-values
(define (m-set-val state name val)
  "Helps create the m-state that we pass along by setting
   the val in a sate"
  (m-values (set-var state name val) name val))

;; Variable assigner helper functions-------------------------------------------------------------------------

;; Updated to handle a state-list from a state
(define (get-var-gen state-list sym undefd)
  "Looks up the var from the state and returns
   the undefined symbol passed if none is found"
  (call/cc
   (λ (k)
     (map (λ (state)
            (when (hash-has-key? state sym)
              (k (hash-ref state sym))))
          state-list)
     undefd)))

(define (get-var-gen-local state-list sym undefd)
  "Looks up the var from the state and returns
   the undefined symbol passed if none is found"
  (get-var-gen (drop-right state-list 1) sym undefd))

;; only called once in the code outside of the und version... that's in assign to check if the var is called
(define (get-var state sym)
  "Gets the SYMbol from a STATE
   or #f is no such SYMbol exists"
  (get-var-gen state sym #f))

(define (get-var-local state sym)
  "Gets the SYMbol from a STATE like
   get-var but only from the local state"
  (get-var-gen-local state sym #f))

;; Need this for patterns like '((var y) (= y x) (return y))
;; where x would be #f by the normal method
(define (get-var-assign state sym)
  "Gets the SYMbol from a STATE
   or undefined if the variable doesn't exist"
  (get-var-gen state sym undefined-sym))


;;; Undefined variant

;; assign will give 'undefined variable error non und version will give 'undefined
;; (get-var-und-assign (m-values-state (m-var '(var z) (hash))) 'c)

;; will just give #f just like get-var
;; (get-var-und (m-values-state (m-var '(var z) (hash))) 'c)

(define (get-var-und-gen state sym f)
  "A general generator that either gets passed get-var or get-var-assign
   based on used case"
  (cond [(or (number? sym)
            (eq? #f sym)
            (eq? #t sym)
            (eq? 'true sym)) sym]
        [(equal? undefined-sym state) (error "Can't use an undefined variable")]
        [else                         (f state sym)]))

;; use when actually evaluating the symbol
(define (get-var-und state sym)
  "gets a variable from a state and errors if the variable is undefined
   best used when using the variable"
  (get-var-und-gen state sym get-var))


(define (get-var-und-local state sym)
  "works like get-var-und except only checks the local state"
  (get-var-und-gen state sym get-var-local))

;; 
(define (get-var-und-assign state sym)
  "gets a variable from a state and errors if the variable is undefined
   Different from the non-assign version as this one returns undefined
   if a variables isn't bound"
  (get-var-und-gen state sym get-var-assign))


;; outdated, can just grab the state directly from here
(define (get-var-state-location state-list sym)
  "Looks up and retrieves the position of where the sate with the var is"
  (call/cc (λ (k)
             (map (λ (state num)
                    (when (hash-has-key? state sym)
                      (k num)))
                  state-list (range (length state-list)))
             0)))

(define (get-var-state state-list sym)
  (member sym state-list (λ (x y) (hash-has-key? y x))))

;; (member 'x (create-new-scope (m-expr '((var x 0) (var y 10)))) (λ (x y) (hash-has-key? y x)))

;; Updated to account for it being a list only used in m-var directly when not in m-set-val
(define (set-var state sym val)
  (update-nth-scope (λ (x) (hash-set x sym val))
                    state
                    (get-var-state-location state sym)))


(define (set-var-first state sym val)
  (update-first-scope (λ (x) (hash-set x sym val)) state))

;; (#hash(((#hash((class88 . A))) . (#hash((class88 . A))))
;;        (a . (#hash((class88 . A)))))
;;  #hash((add . ((g h) (return (+ g h))))
;;        (class88 . A)
;;        (main . (() (var a (new A)) (return (funcall (dot a add) 10 2)))))
;;  #hash((A . #<clas>)))

;; Getting the functions-------------------------------------------------------------------------------------

(define (fn-get sym [fn-list pattern-to-fn])
  "Gets a function from pattern-to-fn and returns the function to call
   or #f if no such SYMbol exists"
  (if (legal-fn? sym fn-list)
      (hash-ref fn-list sym)
      #f))

(define (get-fn-error sym [fn (fn-get sym)])
  "Gets a function from a SYMbol or returns an error"
  (if fn
      fn
      (error "Function is not defined")))

(define (legal-fn? sym [fn-list pattern-to-fn])
  "Checks if the function is illegal or not"
  (hash-has-key? fn-list sym))

;; State Helper functions-------------------------------------------------------------------------------------
;;  pointless functions just to be "abstract enough"... will be useful if the way I handle state changes, though
(define (first-scope state)
  "gets the narrowest-scope of the state"
  (car state))

(define (rest-scope state)
  "gets the rest of the state"
  (cdr state))

;; updater fns are nice
(define (update-first-scope fn state)
  "updates the most narrow scope of the state"
  (cons (fn (first-scope state))
        (rest-scope state)))

;; These functions don't use first-scope nor rest-scope because they are implemented efficiently for a list
;;  Other data structures would get increase performance by re-implementing these functions accordingly

;; a bit slow, can fix later, ie stop when we find what we want
(define (update-nth-scope fn state nth)
  "updates the nth most narrow scope of the state"
  (map (λ (x y)
         (if (= nth y)
             (fn x)
             x))
       state
       (range (length state))))

;; This is a lot faster when nth is small, but slower than the other when nth is large
(define (update-nth-scope% fn state nth [cps identity])
  "updates the nth most narrow scope of the state"
  (if (zero? nth)
      (~> state first-scope fn list cps (append (rest-scope state)))
      (update-nth-scope% fn
                         (cdr state)
                         (- nth 1)
                         (∘ cps (curry cons (car state))))))

;; "updates the nth most narrow scope of the state"
(define/match (update-nth-scope%% fn state nth [cps identity])
  [(fn (list* x xs) 0   cps) (~> x fn list cps (append xs))]
  [(fn (list* x xs) nth cps) (update-nth-scope% fn xs
                                                (- nth 1)
                                                (∘ cps (curry cons x)))])
 
(define (create-new-scope state)
  "adds a new level of scoping to the state"
  (cons (hash) state))

;; CPS helper functions---------------------------------------------------------------------------------------
(define default-cps-value    identity)
(define default-cps-continue (λ (x) (error "Continue called not inside anything")))
(define default-cps-break    (λ (x) (error "Break called not inside anything")))
(define default-cps-try      (λ (x) (error "try called not inside a catch")))

(define default-cps
  (hash 'return default-cps-value
        'begin  default-cps-continue
        'loop   default-cps-break
        'try    default-cps-try))

(define (return-cps cps-seq)
  (hash-ref cps-seq 'return))

(define (begin-cps cps-seq)
  (hash-ref cps-seq 'begin))

(define (loop-cps cps-seq)
  (hash-ref cps-seq 'loop))

(define (try-cps cps-seq)
  (hash-ref cps-seq 'try))

(define (set-return-cps val cps-seq)
  (hash-set cps-seq 'return val))

(define (set-begin-cps val cps-seq)
  (hash-set cps-seq 'begin val))

(define (set-loop-cps val cps-seq)
  (hash-set cps-seq 'loop val))

(define (set-try-cps val cps-seq)
  (hash-set cps-seq 'try val))
;; MEXPS------------------------------------------------------------------------------------------------------
;; m value passes up, state passes down

;; (var z (= x (= y 10)))

;; (m-var '(var z (= x (= y 10))) (hash 'z 2))

;; pattern match of bind
;; '(= x (= y 10))


;; expr = '(var z (= c (= c 10))) -> _ = 'var | name = z | bind = '(= c ...) | _ = #f
;; Look above for the non-macro version

;; At some point turn m-var into a cps-seq passing style as what I'm doing will eventually blow the stack......
;;  cps-seq style on m-create??????
;; (compose cps-seq (λ (x) m-set-val state name (m-values-val x)))


;; (m-values-state (m-var '(var z (= c (= d 9))) (hash 'd 2 'c 3)))
(define-m (m-var-gen expr state reassign [cps-seq default-cps]) expr _ name bind _
  "Takes an EXPR and a STATE, updates the state and returns a m-value"
  (when (and reassign (get-var-und-local (list (first-scope state)) name))
    (error "Variable is already defined")) ; variable is already defined
  (match bind                        ; bind = '(= c ...) -> sym-fn = =
    [(list* sym-fn _)
     (let [(m-value ((get-fn-error sym-fn) bind (S/set-var name undefined-sym) cps-seq))] ; we are making
       (m-set-val (m-values-state m-value) ; a new state so if we have a pattern (var x (= y x)) it fails
                  name                     ; instead of assinging y #f and assinging x #f
                  (m-values-val m-value)))]
    [`()
     (S/m-set-val name undefined-sym)] ; no value is passed so our var is undefined
    [(? (curry get-var-und-assign state) val) ; covers the (var z x) case
     (S/m-set-val name (S/get-var-und-assign val))]
    [val
     (S/m-set-val name val)]))

(define (m-var expr state [cps-seq default-cps]) 
  "Takes an EXPR and a STATE, updates the state and returns a m-value
   Does a check for a reassignment"
  (m-var-gen expr state #t cps-seq))

(define (m-var-fn expr state [cps-seq default-cps]) 
  "This version allows a reassignment as this is allowed when
   a function creates it's arguments"
  (m-var-gen expr state #f cps-seq))


(define-m (m-function expr state reassign [cps-seq default-cps]) expr _ name arg code
  (s/m-set-val name (cons arg code)))


(define/match (rem-extends expr)
  [('())      '()]
  [((list _ a)) a])


;; m-class :: expr -> state -> cps -> m-val
(define-m (m-class expr state [cps-seq default-cps]) expr _ name parent meths
  "Used to store the classes"                                       ; note for the graders.. the let just makes things
  (let* ((parent-class (s/get-var-und (rem-extends parent)))        ; easier to read for the future... that's all
         (fields/state (set-var (m-inner meths
                                         default-state-global
                                         cps-seq
                                         class-fns)
                                class-key-word
                                name)) 
         (parent-field (cons fields/state
                             (if parent-class ; use the reserve word to save what class we are in
                                 (clas-vars/fns parent-class)
                                 null)))) 
    (s/m-set-val name (clas parent-class (flatten parent-field))))) ; need to flatten because the lists like to get nested

(define-m (m-assign expr state [cps-seq default-cps]) expr _ name bind _
  "Takes an EXPR and a STATE, updates the state and returns a m-value"
  (unless (S/get-var name)                      ; the one use of get-var
    (error "The variable is not instantiated")) ; variable isn't in the state
  (match bind
         [(list* sym-fn _)
          (let ([m-value ((get-fn-error sym-fn) bind state cps-seq)]) 
            (m-set-val (m-values-state m-value)
                       name
                       (m-values-val m-value)))]
         
         [`() (error "can't assign a value to nothing!")]
         [val (m-set-val state name (get-var-und-assign state val))]))


;; (m-values-val (m-fn + '(+ (* 9 2) (+ 2 3)) (hash 'd 2 'c 3)))
;; (m-values-val (m-fn > '(> (= x (+ x 1)) y) (hash 'x 10 'y 10)))
;; '#hash((x . 11) (y . 10) (nil . #t))
;; (m-values-val ((curry m-fn (curr-2 and)) '(&& (< 2 3) (== 0 0)) default-state))
;; (m-values-val ((curry m-fn (λ (x y) (not x))) '(! #f) default-state))

(define (get-fns expr) (cdr expr))
(define (padlist lis) (cons null lis))

(define/s (m-fn fn expr state [cps-seq default-cps] [vals null])
  "used for predicates and function applications"
  (match (get-fns expr)
         [(list* (list* sym-fn1 args2) args) ; first is a list '(+ (* 1 2) _)
          (let ([m-value
                 ((get-fn-error sym-fn1) (cons sym-fn1 args2) state cps-seq)])
            (m-fn fn
                  (padlist args)
                  (m-values-state m-value)
                  cps-seq
                  (cons (m-values-val m-value) vals)))] ; states gets carried over
         [(list* arg1 args)                             ; first is a atom '(+ b _)
          (m-fn fn
                (padlist args)
                state
                cps-seq
                (cons (S/get-var-und arg1) vals))]
         [_
          (S/m-set-val 'nil (apply fn (reverse vals)))]))

(define/s (m-fn fn expr state [cps-seq default-cps] [vals null])
  "used for predicates and function applications"
  (match (get-fns expr)
         [(list* (list* sym-fn1 args2) args) ; first is a list '(+ (* 1 2) _)
          (let ([m-value
                 ((get-fn-error sym-fn1) (cons sym-fn1 args2) state cps-seq)])
            (m-fn fn
                  (padlist args)
                  (m-values-state m-value)
                  cps-seq
                  (cons (m-values-val m-value) vals)))] ; states gets carried over
         [(list* arg1 args)             ; first is a atom '(+ b _)
          (m-fn fn
                (padlist args)
                state
                cps-seq
                (cons (S/get-var-und arg1) vals))]
         [_
          (S/m-set-val 'nil (apply fn (reverse vals)))]))


;; (m-if '(if (&& (< 2 5) (== (% 2 2) 0)) (+ 100 0) (+ 200 0)) default-state)

(define-m (m-if expr state [cps-seq default-cps]) expr _ pred then els
  "does an if statement in our c like language"
  (let* ([comp-mval (λ (x expr state)
                       ((get-fn-error x) expr state cps-seq))] ; grab the value and compute the m-expr
         [else-check (λ (state)                                ; checks for an optional else
                        (if (empty? els)
                            (S/m-set-val 'nil 'nil)
                            (comp-mval (car els) els state)))])
    (match pred
      [(list* sym-pred _)               ; we have a predicate test!
       (let [(m-value (comp-mval sym-pred pred state))]
         (if (m-values-val m-value)
             (comp-mval (car then) then (m-values-state m-value))
             (else-check (m-values-state m-value))))]
      [val
       (if val
           (comp-mval (car then) then state)
           (S/else-check))])))


;; (m-values-val (m-while '(while (> (* x x) 128) (= x (- x 1))) (hash 'x 20))) -> x = 11
(define-m (m-while expr state [cps-seq default-cps]) expr _ pred body _
  "Our while like m-expression in our C like function"
  (define (m-while-inner expr state cps-seq)
    (let [(body-disp
           (λ (state)
              (match body
                     [(list* sym-fn _)
                      (m-while-inner expr
                                     (m-values-state ((get-fn-error sym-fn) body state cps-seq))
                                     cps-seq)]
                     [_
                      (m-while expr state cps-seq)])))]
      (match pred
             [(list* sym-fn _)
              (let [(m-value ((get-fn-error sym-fn) pred state cps-seq))]
                (if (m-values-val m-value)
                    (body-disp (m-values-state m-value))
                    m-value))]
             [val
              (if (S/get-var-und val)
                  (S/body-disp)
                  (S/m-set-val '() (S/get-var-und val)))]))) ; replace with #f since get-var-und can only be #f here?
  (let/ec k                             ; used for break;
          (m-while-inner expr state (set-loop-cps k cps-seq))))


;; We are going to use the cps-seq to break out of the computation and exit!
(define-m (m-return expr state [cps-seq default-cps]) expr _ return _ _
  "Breaks out of the program and returns"
  (define (return-check state val)
    (let ([ret-cps (λ (x) ((return-cps cps-seq) (S/m-set-val 'nil x)))])
      (case val
            [(#f) (ret-cps 'false)] ; quit out of the entire program with return-cps
            [(#t) (ret-cps 'true)]
            [else (ret-cps  val)])))
  
  (match return
         [(list* sym-fn _) (let [(m-value ((get-fn-error sym-fn) return state cps-seq))]
                             (return-check (m-values-state m-value)
                                           (m-values-val   m-value))
                             m-value)]                                 ; we are just doing this so it plays nicely without the let/ec
         [val              (s/return-check (S/get-var-und-assign val)) ; use get-var-und-assign instead as it should error
                           (S/m-set-val '() val)]))                    ; needless as we shall quit out by cps... same as above

;; m-expr at the moment doesn't bind the rest of the arguments and thus I need to map through them
(define (statements-of-begin expr)
  (cdr expr))

(define/s (m-begin expr state [cps-seq default-cps])
  "Handles the {} cases"
  (let/cc k
    (let* ((process-begin (m-inner (statements-of-begin expr)
                                   (S/create-new-scope)
                                   (set-begin-cps k cps-seq)))
           (rest-state    (rest-scope process-begin)))
      (m-set-val rest-state 'nil 'nil))))

(define/s (m-continue expr state [cps-seq default-cps])
  "Handles Continue statements"
  ((begin-cps cps-seq) (S/m-set-val 'nil 'nil)))

(define/s (m-break expr state [cps-seq default-cps])
  "Handles Break statements"
  ((loop-cps cps-seq)
   (m-set-val (S/rest-scope) 'nil 'nil)))

;; not going to make a new state for m-try... doesn't break any tests, but idk
(define-m (m-try expr state [cps-seq default-cps]) expr _ body catch finally
  (let* ((computed   (let/ec k (m-inner body state (set-try-cps k cps-seq)))) ; type = state or (vector e state)
         (m-final    (λ (finally new-state cps-seq)
                       (if (empty? finally) ; finally is optional
                           new-state
                           (m-finally finally new-state cps-seq))))
         (finally-fn (λ (new-state)
                       (m-set-val (m-final finally new-state cps-seq)
                                  'nil
                                  'nil))))
    (if (vector? computed) ; only a vector if we have a throw (e state)
        (finally-fn (m-catch catch
                             (vector-ref computed 1)
                             (vector-ref computed 0)
                             cps-seq))
        (finally-fn computed))))

;; Not going to implement case matching on whether you can do (throw (= x 20) since it's not in the tests
;; This behavior can easily be added though!!!
;; -> (vector val state)
(define-m (m-throw expr state [cps-seq default-cps]) expr _ val _ _
  ((try-cps cps-seq) (vector (S/get-var-und val) state)))

;; -> state
(define-m (m-catch expr state val [cps-seq default-cps]) expr _ var body _
  (let* ((catch-state  (set-var state (car var) val))
         (eval-state   (m-inner body
                                catch-state
                                cps-seq))
         (proper-state (set-var eval-state
                                (car var)
                                (S/get-var-und (car var)))))
    ;; proper-state resets the (e) val in the catch as it shouldn't persist
    proper-state))

;; -> state
(define-m (m-finally expr state [cps-seq default-cps]) expr _ body _ _
  (m-inner body state cps-seq))
*
;; state -> (list syms) -> state
(define/s (assign-vars state vars vals name)
  (unless (= (length vars) (length vals))
    (error (format "~a was given ~a arguments when it only takes ~a" name (length vals) (length vars))))
  (match (list vars vals)
    [(list (list* a b) (list* c d)) (assign-vars (S/set-var-first a c) b d name)]
    [(list (list* a)   (list* c))   state]))

(define (get-args expr)
  (cddr expr))

;; (map (λ (x)
;;          (match x
;;            [(list* (list* (? legal-fn? fn) _)) (m-values-val ((get-fn-error fn) x default-state default-cps))]
;;            [(list* val)                        val]))
;;                     '((- 2 3) (+ 2 3) 9))
;;  -> (-1 5 9)

(define/s (comprehend-name state name [cps-seq default-cps])
  "grabs the proper name for the m-funcall function"
  (match name
    [(list* function _) (m-values-var ((get-fn-error function) name state cps-seq))]
    [name               (s/get-var name)]))

(define-m (m-funcall expr state [cps-seq default-cps]) expr _ name _ _ _
  (let* ((computed (let/ec ret-cps
                     (let* ((hash     (s/comprehend-name name cps-seq))
                            (vars     (car hash))
                            (new-expr (cdr hash))
                            (vals     (map (λ (x)
                                             (match x
                                               [(list* (list* (? legal-fn? fn) _)) (m-values-val ((get-fn-error fn) x state cps-seq))]
                                               [(list* val)                        (S/get-var-und val)]))
                                           (get-args expr)))
                            (state    (assign-vars (S/create-new-scope) vars vals name)))
                       (m-inner new-expr state (set-return-cps ret-cps cps-seq)))))
         (value    (when (m-values? computed)
                     (m-values-val computed)))
         (state    (if (m-values? computed)
                       (rest-scope (m-values-state computed))
                       (rest-scope computed))))
    (m-set-val state 'nil value)))

(define (m-identity expr state [cps-seq default-cps])
  "returns just the state without doing any computation"
  (m-values state null null))

;; foldr
;; (set-var 2nd (car 1st) (cadr 1st))

(define (zip xs ys) (map list xs ys))

(define-m (m-new expr state [cps-seq default-cps]) expr _ class _ _ _
  (let* ((hash         (car (clas-vars/fns (s/get-var-und class))))
         (key-val-comb (zip (hash-keys hash) (hash-values hash)))
         (new-state    default-state-global)
         (var/val      (foldr (λ (x y) (match x
                                         ;; [(list _ (list* fn-lis _)) y] ; exclude functions from being included in the new var
                                         [(list sym val)            (set-var y sym val)]))
                              new-state                                ; gets passed again and again
                              key-val-comb)))
    (m-set-val state var/val var/val)))


;; (grab-state 'this (append (cadr (m-expr-class a7 "C")) (car (m-expr-class a7 "C"))))

(define/s (grab-state state var)
  "grabs the state of the class or variable sent"
  (let ((val-resolve (λ (var)
                       (let ((val (s/get-var-und var)))
                         (if (clas? val)      ; ie we pass it A and the class is A
                             (clas-vars/fns val)
                             val)))))
    (match var
      ['this  ((∘ clas-vars/fns             s/get-var-und s/get-var-und) class-key-word)] ; should grab the nearest class var
      ['super ((∘ clas-vars/fns clas-parent s/get-var-und s/get-var-und) class-key-word)]
      [(list _ var) (val-resolve var)]
      [var          (val-resolve var)])))



(define-m (m-dot expr state [cps-seq default-cps]) expr _ var field _ _
  (let ((var/val (get-var-und (s/grab-state var) field)))
    (m-set-val state var/val var/val))) 
;; CALLER-----------------------------------------------------------------------------------------------------


;; (semantics "test21.javaish")
(define (semantics file)
  (let [(expression (parser file))]
    (m-expr expression)))

;; (~>> fn-list (fn-get fn) (get-fn-error fn))

;; used in M-expr for the parsing and begin to delimt
(define (m-inner expr state cps-seq [fn-list pattern-to-fn])
  "fn-list allows for class-vars to be used instead of pattern-to-fn for legal functions"
  (match expr
    [(list* (list* (? (λ (x) (legal-fn? x fn-list)) fn)
                   args)
            rest) ; code change to account for pattern-to-fn vs class-fns
          
     (let [(state (m-values-state ((get-fn-error fn (fn-get fn fn-list))
                                   (car expr)
                                    state
                                    cps-seq)))]
       
       (m-inner rest state cps-seq fn-list))]
    
    ['() state]     ; empty list, happens when there is no-return
    [_   (error "invalid expression")]))

;; (m-expr c)
(define (m-expr expr [state default-state] [cps-seq default-cps])
  "Parses the M-expressions of the program"
  (m-values-val
   (let/ec ret-cps              ; this is our quick way to quit for return
     (m-inner expr state (set-return-cps ret-cps cps-seq)))))

;;  fix this main calling mechanism later... main can't take arguments with this restriction, can be easily fixed
(define call-main
  `(funcall main))

(define (m-expr-global expr [state default-state-global] [cps-seq default-cps])
  "Parses the M-expressions of the program with the new function syntax"
  (let* ((globals (create-new-scope (m-inner expr state cps-seq)))
         (main    call-main))
    (m-values-val
     (let/ec ret-cps
       (m-funcall main globals (set-return-cps ret-cps cps-seq))))))


(define (m-expr-class expr name [state default-state-global] [cps-seq default-cps])
  "Parses the M-expressions of the program with the new class syntax
   requires a name to tell what main program to run"
  (let* ((classes     (m-inner expr state cps-seq class-fns))
         (class-m     (~>> name string->symbol (get-var-und classes) clas-vars/fns))
         (class-state (append class-m classes))) ;; appends the classes to the back of the class state
    (m-values-val
     (let/ec ret-cps
       (m-funcall call-main class-state (set-return-cps ret-cps cps-seq))))
    ;; class-state
    ))
;; Has to be at the bottom for drRacket
(define pattern-to-fn
  (hash 'if        m-if
        'var       m-var
        'while     m-while
        '=         m-assign
        'return    m-return
        'begin     m-begin
        'continue  m-continue
        'break     m-break
        'try       m-try
        'throw     m-throw
        'catch     m-catch
        'finally   m-finally
        'function  m-function
        'class     m-class
        'funcall   m-funcall
        'static-function m-function
        'new       m-new
        'dot       m-dot
        '|| (curry m-fn (curr-2 or))
        '&& (curry m-fn (curr-2 and))
        '!  (curry m-fn not)              ; ignore the second argument
        '+  (curry m-fn +)
        '-  (curry m-fn -)
        '*  (curry m-fn *)
        '/  (curry m-fn /)
        '%  (curry m-fn modulo)
        '== (curry m-fn =)
        '!= (curry m-fn (λ x (not (apply = x))))
        '>=  (curry m-fn >=)
        '<=  (curry m-fn <=)
        '>  (curry m-fn >)
        '<  (curry m-fn <)))

(define class-fns
  (hash 'function        m-function
        'static-function m-function
        'static-var      m-var
        'var             m-var ; can't give it m-identity, as we can call x directly
        'class           m-class))

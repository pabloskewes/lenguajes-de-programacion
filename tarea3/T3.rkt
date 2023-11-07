#lang play
(print-only-errors #t)

#|
  Expr  ::= <num>
          | (+ <Expr> <Expr>)
          | (- <Expr> <Expr>)
          | (* <Expr> <Expr>)
          | <id>
          | (fun (<id> : <Type>) <Expr>)
          | (<Expr> <Expr>);
          | (tt)
          | (ff)
          | (leq <Expr> <Expr>)
          | (ifc <Expr> <Expr> <Expr>)
|#
(deftype Expr
  ;; core
  (num n)
  (binop op l r)
  ;; unary first-class functions
  (id x)
  (fun binder binderType body)
  (app callee arg)
  ;; p1.c
  (tt)
  (ff)
  (leq l r)
  (ifc condition then else)
)

#| BEGIN P1 |#

;; Type ::= numT
;;        | (arrowT <Type> <Type>)
(deftype Type
  (numT)
  (arrowT argType resType)
  ;; p1.c
  (boolT)
)

(test (Type? (numT)) #t)
(test (Type? (arrowT (numT) (numT))) #t)
(test (Type? (arrowT (numT) (arrowT (numT) (numT)))) #t)
(test (Type? 10) #f)
(test (Type? 'foo) #f)
(test (Type? (boolT)) #t)
(test (Type? (arrowT (boolT) (boolT))) #t)

;; s-expr-type ::= 'Number
;;              |  '(-> <s-expr> <s-expr>)

;; parse-type : s-expr-type -> Type 
;; parsea un s-expr-type en un Type, o falla con un error
(define (parse-type t) 
  (match t
    ['Number (numT)]
    [(list '-> arg res) (arrowT (parse-type arg) (parse-type res))]
    ; p1.c
    ['Boolean (boolT)]
    [_ (error 'parse-type "invalid type: ~a" t)]))

(test (parse-type 'Number) (numT))
(test (parse-type '(-> Number Number)) (arrowT (numT) (numT)))
(test (parse-type '(-> (-> Number Number) Number)) (arrowT (arrowT (numT) (numT)) (numT)))
(test/exn (parse-type 'foo) "invalid type: foo")
(test (parse-type 'Boolean) (boolT))
(test (parse-type '(-> Boolean Boolean)) (arrowT (boolT) (boolT)))
(test (parse-type '(-> (-> Boolean Number) Boolean)) (arrowT (arrowT (boolT) (numT)) (boolT)))

;; parse : s-expr -> Expr
(define (parse s)
  (match s
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    [(list '+ l r) (binop '+ (parse l) (parse r))]
    [(list '- l r) (binop '- (parse l) (parse r))]
    [(list '* l r) (binop '* (parse l) (parse r))]
    [(list 'fun (list binder ': type) body) (fun binder (parse-type type) (parse body))]
    [(list callee arg) (app (parse callee) (parse arg))]
    [_ (error 'parse "invalid syntax: ~a" s)]))

;; Implementación de ambientes de tipos
;; (análoga a la de ambientes de valores)

;; TypeEnv ::= ⋅ | <TypeEnv>, <id> : <Type>
(deftype TypeEnv (mtTenv) (aTenv id type env))
(define empty-tenv (mtTenv))
(define extend-tenv aTenv)

(define (tenv-lookup x env)
  (match env
    [(mtTenv) (error 'tenv-lookup "free identifier: ~a" id)]
    [(aTenv id type rest) (if (symbol=? id x) type (tenv-lookup x rest))]
    ))

;; infer-type : Expr TypeEnv -> Type
;; infiere el tipo de una expresión, o falla con un error
(define (infer-type expr tenv) 
  (match expr
    [(num n) (numT)]
    ; [x #:when (symbol? x) (tenv-lookup x tenv)]
    [(binop op l r) (match op
        ['+ (match (cons (infer-type l tenv) (infer-type r tenv))
            [(cons (numT) (numT)) (numT)]
            [_ (error 'infer-type "invalid operand for +")])]
        ['- (match (cons (infer-type l tenv) (infer-type r tenv))
            [(cons (numT) (numT)) (numT)]
            [_ (error 'infer-type "invalid operand for -")])]
        ['* (match (cons (infer-type l tenv) (infer-type r tenv))
            [(cons (numT) (numT)) (numT)]
            [_ (error 'infer-type "invalid operand for *")])]
        [_ (error 'infer-type "invalid operator")])]
    [(id x) (tenv-lookup x tenv)]
    [(fun binder binderType body) (arrowT binderType (infer-type body (extend-tenv binder binderType tenv)))]
    [(app callee arg) 
        ; (match (infer-type callee tenv)
        ; [(arrowT argType resType) (match (infer-type arg tenv)
        ;     [argType resType]
        ;     [_ (error 'infer-type "function argument type mismatch")])]
        ; [_ (error 'infer-type "function application to a non-function")])
        (define callee-type (infer-type callee tenv))
        (define arg-type (infer-type arg tenv))
        (match callee-type
          [(arrowT T1 T2) (if (equal? T1 arg-type) 
                              T2 
                              (error 'infer-type "function argument type mismatch"))]
          [_ (error 'infer-type "function application to a non-function")])]
))

(test (infer-type (num 1) empty-tenv) (numT))
(test (infer-type (fun 'x (numT) (id 'x)) empty-tenv) (arrowT (numT) (numT)))
(test (infer-type (fun 'x (arrowT (numT) (numT)) (id 'x)) empty-tenv)
(arrowT (arrowT (numT) (numT)) (arrowT (numT) (numT))))
(test/exn (infer-type (binop '+ (num 1) (fun 'x (numT) (id 'x))) empty-tenv) "infer-type: invalid operand for +")
(test/exn (infer-type (app (num 1) (num 2)) empty-tenv) "infer-type: function application to a non-function")
(test/exn (infer-type (app (fun 'x (numT) (id 'x)) (fun 'x (numT) (id 'x))) empty-tenv) "infer-type: function argument type mismatch")

(test (infer-type (parse 10) empty-tenv) (numT))
(test (infer-type (parse '(+ 10 20)) empty-tenv) (numT))
(test (infer-type (parse '(- 10 20)) empty-tenv) (numT))
(test (infer-type (parse '(* 10 20)) empty-tenv) (numT))
(test (infer-type (parse 'foo) (extend-tenv 'foo (numT) empty-tenv)) (numT))
(test (infer-type (parse '(fun (x : Number) x)) empty-tenv) (arrowT (numT) (numT)))
(test (infer-type (parse '(fun (x : Number) (+ x 1))) empty-tenv) (arrowT (numT) (numT)))
(test (infer-type (parse '(fun (x : Number) (+ x x))) empty-tenv) (arrowT (numT) (numT)))
(test/exn (infer-type (parse '(fun (x : Number) (+ x (fun (y : Number) y)))) empty-tenv) "infer-type: invalid operand for +")
(test/exn (infer-type (parse '(fun (x : Number) (+ x y))) empty-tenv) "tenv-lookup")



#| END P1 |#

#| BEGIN P2 PREAMBLE |#

;; ambiente de sustitución diferida
(deftype Env
  (mtEnv)
  (aEnv id val env))

;; interface ADT (abstract data type) del ambiente
(define empty-env (mtEnv))

;; "Simplemente" asigna un nuevo identificador para aEnv
;(define extend-env aEnv)
;;
;; es lo mismo que definir extend-env así:
;; (concepto técnico 'eta expansion')
(define (extend-env id val env) (aEnv id val env))

(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x) val (env-lookup x rest))]))

;; num2num-op : (Number Number -> Number) -> Val Val -> Val
(define (num2num-op op)
  (λ (l r)
    (match (cons l r)
      [(cons (num n) (num m)) (num (op n m))]
      [_ (error 'num-op "invalid operands")])))


(define num+ (num2num-op +))
(define num- (num2num-op -))
(define num* (num2num-op *))

#| END P2 PREAMBLE |#

#| BEGIN P2 |#

;; final? : ...
(define (final? e) '???)

(deftype Kont
  (mt-k) ; empty kont
  ;; ...
  )

(define empty-kont (mt-k))

;; State ::= (<Expr>, <Env>, <Kont>)
(deftype State
  (st expr env kont)
  )

;; inject : ...
(define (inject expr) '???)

;; step : ...
(define (step c) '???)

;; eval : Expr -> Expr
(define (eval expr)
  (define (eval-until-final state)
    (def (st expr _ kont) state)
    (if (and (final? expr) (mt-k? kont))
        expr
        (eval-until-final (step state))))
  (eval-until-final (inject expr)))

;; run : ...
(define (run s-expr) '???)

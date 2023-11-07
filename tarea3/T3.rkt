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
          | (<= <Expr> <Expr>)
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
  (ifc condition then else_)
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
;;             |  '(-> <s-expr> <s-expr>)
;;             |  'Boolean

;; parse-type : s-expr-type -> Type 
;; parsea un s-expr-type en un Type, o falla con un error
(define (parse-type t) 
  (match t
    ; p1.a
    ['Number (numT)]
    [(list '-> arg res) (arrowT (parse-type arg) (parse-type res))]
    ; p1.c
    ['Boolean (boolT)]
    [_ (error 'parse-type "invalid type: ~a" t)]))

(test (parse-type 'Number) (numT))
(test (parse-type '(-> Number Number)) (arrowT (numT) (numT)))
(test (parse-type '(-> (-> Number Number) Number)) (arrowT (arrowT (numT) (numT)) (numT)))
(test/exn (parse-type 'foo) "invalid type: foo")
; tests p1.c
(test (parse-type 'Boolean) (boolT))
(test (parse-type '(-> Boolean Boolean)) (arrowT (boolT) (boolT)))
(test (parse-type '(-> (-> Boolean Number) Boolean)) (arrowT (arrowT (boolT) (numT)) (boolT)))

;; parse : s-expr -> Expr
(define (parse s)
  (match s
    ; core
    [n #:when (number? n) (num n)]
    [(list '+ l r) (binop '+ (parse l) (parse r))]
    [(list '- l r) (binop '- (parse l) (parse r))]
    [(list '* l r) (binop '* (parse l) (parse r))]
    [(list 'fun (list binder ': type) body) (fun binder (parse-type type) (parse body))]
    [(list callee arg) (app (parse callee) (parse arg))]
    ; p1.c
    ['true (tt)]
    ['false (ff)]
    [(list '<= l r) (binop '<= (parse l) (parse r))]
    [(list 'if condition then else_) (ifc (parse condition) (parse then) (parse else_))]
    ; core
    [x #:when (symbol? x) (id x)]
    [_ (error 'parse "invalid syntax: ~a" s)]))

(test (parse 10) (num 10))
(test (parse 'foo) (id 'foo))
(test (parse '(+ 10 20)) (binop '+ (num 10) (num 20)))
(test (parse '(- 10 20)) (binop '- (num 10) (num 20)))
(test (parse '(* 10 20)) (binop '* (num 10) (num 20)))
(test (parse '(fun (x : Number) x)) (fun 'x (numT) (id 'x)))
(test (parse '(fun (x : Number) (+ x 1))) (fun 'x (numT) (binop '+ (id 'x) (num 1))))
(test (parse '(fun (x : Number) (+ x x))) (fun 'x (numT) (binop '+ (id 'x) (id 'x))))
(test (parse '(if (<= 5 6) true false)) (ifc (binop '<= (num 5) (num 6)) (tt) (ff)))

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
        ; (<=: p1.c)
        ['<= (match (cons (infer-type l tenv) (infer-type r tenv))
            [(cons (numT) (numT)) (boolT)]
            [_ (error 'infer-type "invalid operand for <=")])]
        [_ (error 'infer-type "invalid operator")])]
    [(id x) (tenv-lookup x tenv)]
    [(fun binder binderType body) (arrowT binderType (infer-type body (extend-tenv binder binderType tenv)))]
    [(app callee arg) 
        (define callee-type (infer-type callee tenv))
        (define arg-type (infer-type arg tenv))
        (match callee-type
          [(arrowT T1 T2) (if (equal? T1 arg-type) 
                              T2 
                              (error 'infer-type "function argument type mismatch"))]
          [_ (error 'infer-type "function application to a non-function")])]
    ; p1.c
    [(tt) (boolT)]
    [(ff) (boolT)]
    [(ifc condition then else_) (match (list (infer-type condition tenv) (infer-type then tenv) (infer-type else_ tenv))
        [(list (boolT) then_type else_type) (if (equal? then_type else_type)
                                                then_type
                                                (error 'infer-type "if branches type mismatch"))]
        [_ (error 'infer-type "if condition must be a boolean")])]
    [_ (error 'infer-type "invalid expression")]
))

; tests p1.a
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

; tests p1.c
(test (infer-type ( ifc (binop '<= (num 5) (num 6)) (num 2) (num 3))
empty-tenv) (numT))
(test/exn (infer-type ( ifc (num 5) (num 2) (num 3)) empty-tenv) "infer-type: if condition must be a boolean")
(test/exn (infer-type ( ifc (binop '<= (num 5) (num 6)) (num 2) (tt)) empty-tenv) "infer-type: if branches type mismatch")

(test (infer-type (parse 'true) empty-tenv) (boolT))
(test (infer-type (parse 'false) empty-tenv) (boolT))
(test (infer-type (parse '(<= 5 6)) empty-tenv) (boolT))
(test (infer-type (parse '(if (<= 5 6) true false)) empty-tenv) (boolT))
(test/exn (infer-type (parse '(if (<= 5 6) 5 false)) empty-tenv) "infer-type: if branches type mismatch")
(test/exn (infer-type (parse '(if (<= 5 6) true 5)) empty-tenv) "infer-type: if branches type mismatch")
(test/exn (infer-type (parse '(if (+ 5 6) true false)) empty-tenv) "infer-type: if condition must be a boolean")


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

;; final? : Expr -> Boolean
;; Dada una expresión (de tipo Expr), retorna un booleano indicando
;; si la expresión es un valor o no.
(define (final? e) 
  (match e
    [(num _) #t]
    [(id _) #t]
    [(fun _ _ _) #t]
    [(tt) #t]
    [(ff) #t]
    [_ #f]))

(test (final? (num 1)) #t)
(test (final? (id 'x)) #t)
(test (final? (fun 'x (numT) (id 'x))) #t)
(test (final? (tt)) #t)
(test (final? (ff)) #t)
(test (final? (binop '+ (num 1) (num 2))) #f)
(test (final? (app (fun 'x (numT) (id 'x)) (num 1))) #f)

#|
  Kont  ::= mt-k
          | (binop-r-k <binop> <Env> <Kont>)
          | (binop-l-k <binop> <Env> <Kont>)
          | (arg-k <Val> <Env> <Kont>)
          | (fun-k <Val> <Env> <Kont>)
|#
(deftype Kont
  (mt-k) ; empty kont
  (binop-r-k op right-comp env kont)
  (binop-l-k op left-comp env kont)
  (arg-k f-arg env kont)
  (fun-k evaled-fun env kont)
)

(define empty-kont (mt-k))

(test (Kont? (mt-k)) #t)
(test (Kont? (binop-r-k '+ (num 2) empty-env empty-kont)) #t)
(test (Kont? (binop-l-k '+ (num 1) empty-env empty-kont)) #t)
(test (Kont? (arg-k (num 1) empty-env empty-kont)) #t)
(test (Kont? (fun-k (fun 'x (numT) (id 'x)) empty-env empty-kont)) #t)


;; State ::= (<Expr>, <Env>, <Kont>)
(deftype State
  (st expr env kont)
  )

(test (State? (st (num 1) empty-env empty-kont)) #t)
(test (State? (st (num 1) empty-env (binop-r-k '+ (num 2) empty-env empty-kont))) #t)

;; inject : Expr -> State
;; Recibe una expresión y crea un estado inicial,
;; con un ambiente vacío y la continuación vacía.
(define (inject expr) (st expr empty-env empty-kont))

(test (inject (num 1)) (st (num 1) empty-env empty-kont))
(test (inject (id 'x)) (st (id 'x) empty-env empty-kont))
(test (inject (fun 'x (numT) (id 'x))) (st (fun 'x (numT) (id 'x)) empty-env empty-kont))
(test (inject (binop '+ (num 1) (num 2))) (st (binop '+ (num 1) (num 2)) empty-env empty-kont))

;; step : State -> State
;; Recibe un estado y retorna el siguiente estado, sin usar recursión ni eval.
;; Funciona de acuerdo a las siguientes reglas de transición:
;; (Rleft)  ((op e1 e2 ), γ, k) -> (e1 , γ, binop-r-k(op, e2 , γ, k))
;; (Rvar)   (x, γ, k) -> (v, γ', k) donde γ(x) = (v, γ')
;; (Rfun)   ((e1 e2 ), γ, k) -> (e1 , γ, arg-k(e2 , γ, k))
;; (Rright) (v1, γ, binop-r-k(op, e2, γ', k)) -> (e2, γ', binop-l-k(op, v1, γ, k))
;; (Rbinop) (v2, γ, binop-l-k(op, v1 , γ', k)) -> (v1[op]v2, γ, k)
;; (Rarg)   (v1, γ, arg-k(e2, γ', k)) -> (e2, γ', fun-k(v1, γ, k))
;; (Rapp)   (v2, γ, fun-k((fun (x : _) e), γ', k)) -> (e, γ'[x -> (v2, γ)], k)
(define (step c) '???)


; (test (step (st (binop '+ (num 1) (num 2)) (mtEnv) (mt-k)))
; (st (num 1) (mtEnv) (binop-r-k '+ (num 2) (mtEnv) (mt-k)))) ; (Rleft)
; (test (step (st (num 1) (mtEnv) (binop-r-k '+ (num 2) (mtEnv) (mt-k))))
; (st (num 2) (mtEnv) (binop-l-k '+ (num 1) (mtEnv) (mt-k)))) ; (Rright)
; (test (step (st (num 2) (mtEnv) (binop-l-k '+ (num 1) (mtEnv) (mt-k))))
; (st (num 3) (mtEnv) (mt-k))) ; (Rbinop)



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

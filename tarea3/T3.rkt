#lang play


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
;; Expr: Representa una expresión del lenguaje
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
;;        | boolT
;; Representa un tipo del lenguaje
(deftype Type
  (numT)
  (arrowT argType resType)
  ;; p1.c
  (boolT)
)

;; s-expr-type ::= 'Number
;;              |  '(-> <s-expr> <s-expr>)
;;              |  'Boolean

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


;; parse : s-expr -> Expr
;; parsea un s-expr en un Expr, o falla con un error
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


;; Implementación de ambientes de tipos
;; (análoga a la de ambientes de valores)

;; TypeEnv ::= ⋅ | <TypeEnv>, <id> : <Type>
(deftype TypeEnv (mtTenv) (aTenv id type env))
(define empty-tenv (mtTenv))
(define extend-tenv aTenv)

;; tenv-lookup : Symbol TypeEnv -> Type
;; busca el tipo de un identificador en un ambiente de tipos,
;; o falla con un error
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
    [_ #f]))


#|
  Kont  ::= mt-k
          | (binop-r-k <binop> <Env> <Kont>)
          | (binop-l-k <binop> <Env> <Kont>)
          | (arg-k <Val> <Env> <Kont>)
          | (fun-k <Val> <Env> <Kont>)
|#
; Kontinuation: stack de acciones por realizar
(deftype Kont
  (mt-k) ; empty kont
  (binop-r-k op right-comp env kont)
  (binop-l-k op left-comp env kont)
  (arg-k f-arg env kont)
  (fun-k evaled-fun env kont)
)

(define empty-kont (mt-k))

;; State ::= (<Expr>, <Env>, <Kont>)
;; Representa el estado de la máquina de evaluación
(deftype State
  (st expr env kont)
  )


;; inject : Expr -> State
;; Recibe una expresión y crea un estado inicial,
;; con un ambiente vacío y la continuación vacía.
(define (inject expr) (st expr empty-env empty-kont))


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
(define (step c)
  (match c
    ; (Rleft)
    [(st (binop op e1 e2) env kont) (st e1 env (binop-r-k op e2 env kont))]
    ; (Rvar)
    [(st (id x) env kont) (st (car (env-lookup x env)) (cdr (env-lookup x env)) kont)]
    ; (Rfun)
    [(st (app e1 e2) env kont) (st e1 env (arg-k e2 env kont))]
    ; (Rright)
    [(st v1 env (binop-r-k op e2 env2 kont)) (st e2 env2 (binop-l-k op v1 env kont))]
    ; (Rbinop)
    [(st v2 env (binop-l-k op v1 env2 kont)) 
      (match op
        ['+ (st (num+ v1 v2) env kont)]
        ['- (st (num- v1 v2) env kont)]
        ['* (st (num* v1 v2) env kont)]
        [_ (error 'step "invalid operator")])]
    ; (Rarg)
    [(st v1 env (arg-k e2 env2 kont)) (st e2 env2 (fun-k v1 env kont))]
    ; (Rapp)
    [(st v2 env (fun-k (fun binder binderType body) env2 kont)) (st body (extend-env binder (cons v2 env) env2) kont)]
    [_ (error 'step "invalid state")])
)


;; eval : Expr -> Expr
(define (eval expr)
  (define (eval-until-final state)
    (def (st expr _ kont) state)
    (if (and (final? expr) (mt-k? kont))
        expr
        (eval-until-final (step state))))
  (eval-until-final (inject expr)))

;; run : s-expr -> (Val, Type)
;; recibe una expresión s-expr, la parsea
;; y retorna un par con la expresión evaluada y su tipo.
(define (run s-expr) 
  (define parsed (parse s-expr))
  (define type (infer-type parsed empty-tenv))
  (cons (eval parsed) type))

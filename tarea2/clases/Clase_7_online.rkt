#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)         
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <id> <expr> <expr>)  
         | (id <sym>)
         | (app <sym> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with id named-expr body)
  (id x)
  (app f e))

;; parse :: s-expr -> Expr
;; converts s-exprs into Exprs
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]    
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [x #:when (symbol? x) (id x)]
    [(list 'with (list x e) b) #:when (symbol? x) (with x (parse e) (parse b))]

    ;; error en el parser:
    ;; [(list x e) #:when (symbol? x) (app x e)]
    [(list x e) #:when (symbol? x) (app x (parse e))]))

(deftype FunDef
  (fundef name arg body))

;; look-up :: symbol listof(FunDef) -> FunDef (o error)
(define (look-up f f-list)
  (match f-list
    [(list) (error "function not found")]
    [(cons g r)
     (if (symbol=? f (fundef-name g))
         g
         (look-up f r))]))

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

;; eval :: Expr listof(FunDef) Env -> number
;; Evaluates an arithmetical expression
(define (eval expr f-list env)
  (match expr
    [(num n) n]
    [(add l r) (+ (eval l f-list env) (eval r f-list env))]
    [(sub l r) (- (eval l f-list env) (eval r f-list env))]
    [(if0 c t f) (if (zero? (eval c f-list env)) (eval t f-list env) (eval f f-list env))]
    ;;
    ;; con substitución explícita, encontrar un id significa que éste está libre
    ;; [(id x) (error 'calc "Open expression (free occurrence of ~a)" x)]
    [(id x) (env-lookup x env)]

    ;;
    ;; con substitución explícita, debo "reconstruir" el nodo num
    ;; (calc (subst b x (num (calc e))))
    [(with x e b)
     (def new-env (extend-env x (eval e f-list env) env))
     (eval b f-list new-env)]

    [(app f e)
     ;; acceso "imperativo"
     ;(define the-fundef (look-up f f-list))
     ;(define the-arg (fundef-arg the-fundef))
     ;(define the-body (fundef-body the-fundef))
     (define the-fundef (look-up f f-list))

     ;; def es un "alias" de "define-match"
     (def (fundef _ the-arg the-body) the-fundef)
     
     (def new-env (extend-env the-arg (eval e f-list env) empty-env))
     (eval the-body f-list new-env)]

    ))

(define (run prog f-list)
  (eval (parse prog) f-list empty-env))

(define f-list1 (list (fundef 'add1 'n (parse '(+ 1 n)))))

(define prog1 '(add1 1)) ;; debiera dar como resultado: 2
(define prog1* (app 'add1 (num 1))) ;; debiera dar como resultado: 2

(define prog2 (add (num 1) (id 'n)))



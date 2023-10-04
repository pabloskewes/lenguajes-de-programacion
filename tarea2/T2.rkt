#lang play
(print-only-errors)

;; PARTE 1A, 1B, 1F

#|
<expr> ::= (num <num>)
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (mul <expr> <expr>)
        | (tt)
        | (ff)
        | (leq <expr> <expr>)
        | (ifc <expr> <expr> <expr>)
        | (id <sym>)
        | (fun ListOf[<sym>] <expr>)
        | (app <expr> ListOf[<expr>])
|#
(deftype Expr
  ;; core
  (num n)
  (add l r)
  (sub l r)
  (mul l r)
  ;; booleanos
  (tt)
  (ff)
  (leq l r)
  ;; condicion
  (ifc c t e)
  ;; identificador
  (id x)
  ;; funcion con múltiples parámetros
  (fun params* body)
  ;; aplicación con múltiples argumentos
  (app f-name args))
  

(test (Expr? (num 1)) #t)
(test (Expr? (add (num 1) (num 2))) #t)
(test (Expr? (sub (num 1) (num 2))) #t)
(test (Expr? (mul (num 1) (num 2))) #t)
(test (Expr? (tt)) #t)
(test (Expr? (ff)) #t)
(test (Expr? (leq (num 1) (num 2))) #t)
(test (Expr? (ifc (num 1) (num 2) (num 3))) #t)
(test (Expr? (id 'x)) #t)
(test (Expr? (fun (list 'x 'y) (add (id 'x) (id 'y)))) #t)
(test (Expr? (app (id 'my-function) (list (num 1) (num 2) (num 3)))) #t)
(test (Expr? (fun (list 'x 'y 'z) (mul (sub (id 'x) (id 'y)) (id 'z)))) #t)


;; concrete syntax
#|
<s-expr> ::= <num>
            | (list '+ <s-expr> <s-expr>)
            | (list '- <s-expr> <s-expr>)
            | (list '* <s-expr> <s-expr>)
            | 'true
            | 'false
            | (list '<= <s-expr> <s-expr>)
            | (list 'if <s-expr> <s-expr> <s-expr>)

|#

;; parse :: s-expr -> Expr
;; converts s-exprs into Exprs
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mul (parse l) (parse r))]
    ['true (tt)]
    ['false (ff)]
    [(list '<= l r) (leq (parse l) (parse r))]
    [(list 'if c t e) (ifc (parse c) (parse t) (parse e))]
    ; se deja el error por mientras por si acaso
    [x #:when (symbol? x) (id x)]
    [(list 'fun (list params* ...) body) (fun params* (parse body))]
    [(list f-name args ...) (app (parse f-name) (map parse args))]

    [else (error 'parse "invalid s-expr: ~a" s-expr)]))
  
; tests p1.a
(test (parse 1) (num 1))
(test (parse (list '+ 1 2)) (add (num 1) (num 2)))
(test (parse '(+ 7 9)) (add (num 7) (num 9)))
(test (parse (list '- 3 4)) (sub (num 3) (num 4)))
(test (parse (list '+ 3 (list '- 2 1))) (add (num 3) (sub (num 2) (num 1))))
(test (parse (list '* 10 (list '+ 1 2))) (mul (num 10) (add (num 1) (num 2))))
(test (parse (list '* (list '+ 5 6) 7)) (mul (add (num 5) (num 6)) (num 7)))
(test (parse (list 'if 0 1 2)) (ifc (num 0) (num 1) (num 2)))
(test (parse (list 'if (list '- 1 2) 1 2)) (ifc (sub (num 1) (num 2)) (num 1) (num 2)))
(test (parse 'true) (tt))
(test (parse 'false) (ff))
(test (parse (list '<= 1 2)) (leq (num 1) (num 2)))
(test (parse (list '<= 1 (list '+ 1 2))) (leq (num 1) (add (num 1) (num 2))))
(test (parse (list '<= (list '+ 1 2) 1)) (leq (add (num 1) (num 2)) (num 1)))
(test (parse '(<= 3 5)) (leq (num 3) (num 5)))
(test (parse '(if (<= 3 5) 2 4)) (ifc (leq (num 3) (num 5)) (num 2) (num 4)))
(test (parse '(if true 2 4)) (ifc (tt) (num 2) (num 4)))
; tests p1.b
(test (parse 'x) (id 'x))
(test (parse '(fun (x y) (+ x y))) (fun (list 'x 'y) (add (id 'x) (id 'y))))
(test (parse '(my-function 2 3 4)) (app (id 'my-function) (list (num 2) (num 3) (num 4)))) 


;; PARTE 1C, 1G

;; values of expressions
;; <value> ::= (numV <num>)
;;          | (boolV <bool>) 
;;          | (closureV <sym> <expr> <env>)
(deftype Val
  (numV n)
  (boolV b)
  (closureV params* body env))


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

;; extend-env* :: (listof (cons <sym> <val>)) Env -> Env
;; Recibe una lista de pares (identificador, valor) y un ambiente, y retorna un
;; nuevo ambiente que extiende al ambiente dado con los pares de la lista.
(define extend-env*
  (lambda (ids-vals env)
    (match ids-vals
      [(list) env]
      [(cons (cons id val) rest) (extend-env* rest (extend-env id val env))])))

(test (extend-env* (list) empty-env) empty-env)
(test (extend-env* (list (cons 'x (numV 1)) (cons 'y (numV 2))) empty-env)
      (aEnv 'y (numV 2) (aEnv 'x (numV 1) empty-env)))
(define anEnv (extend-env* (list (cons 'x (numV 1)) (cons 'y (numV 2))) empty-env))
(test (extend-env* (list (cons 'z (numV 3))) anEnv)
      (aEnv 'z (numV 3) (aEnv 'y (numV 2) (aEnv 'x (numV 1) empty-env))))


(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x) val (env-lookup x rest))]))

;; PARTE 1D

;; num2num-op :: (Number Number -> Number)-> (Val Val -> Val)
; Recibe una función que realiza una operación binaria sobre dos números (retornan-
; do un número) y retorna una función que recibe dos valores, hace pattern
; matching sobre los constructores para acceder a los números, y aplica la ope-
; ración, retornando un nuevo valor. Si los dos valores que recibe no son de
; carácter numérico, la función debe fallar.
(define (num2num-op op)
  (lambda (v1 v2)
    (match (list v1 v2)
      [(list (numV n1) (numV n2)) (numV (op n1 n2))]
      [else (error 'num2num-op "invalid operands, expected numbers, got: ~a" (list v1 v2))])))


;; num2bool-op :: (Number Number -> Boolean)-> (Val Val -> Val)
; Análoga a num2num-op, pero retorna un valor de carácter booleano.
(define (num2bool-op op)
  (lambda (v1 v2)
    (match (list v1 v2)
      [(list (numV n1) (numV n2)) (boolV (op n1 n2))]
      [else (error 'num2bool-op "invalid operands, expected numbers, got: ~a" (list v1 v2))])))


(define num+ (num2num-op +))
(define num- (num2num-op -))
(define num* (num2num-op *))
(define num<= (num2bool-op <=))

(test (num+ (numV 1) (numV 2)) (numV 3))
(test (num- (numV 1) (numV 2)) (numV -1))
(test (num* (numV 1) (numV 2)) (numV 2))
(test (num<= (numV 1) (numV 2)) (boolV #t))
(test (num<= (numV 2) (numV 1)) (boolV #f))
(test/exn (num+ (numV 1) (boolV #t)) "invalid operands")

;; PARTE 1E, 1G

;; eval :: Expr Env -> Val
;; Evalúa una expresión en un ambiente dado.
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(add l r) (num+ (eval l env) (eval r env)) ]
    [(sub l r) (num- (eval l env) (eval r env)) ]
    [(mul l r) (num* (eval l env) (eval r env)) ]
    [(tt) (boolV #t)]
    [(ff) (boolV #f)]
    [(leq l r) (num<= (eval l env) (eval r env))]
    [(ifc c t e) 
     (define boolv (eval c env))
     (def (boolV b) boolv)
     (if b (eval t env) (eval e env))]
    [(id x) (env-lookup x env)]
    [(fun params* body) (closureV params* body env)]
    [(app f-name args) 
     (def (closureV the-args the-body the-claus-env) (eval f-name env))
     (def the-ext-env (extend-env* (map cons the-args (map (lambda (arg) (eval arg env)) args)) the-claus-env))
     (eval the-body the-ext-env)]))    

(test (eval (num 1) empty-env) (numV 1))
(test (eval (add (num 1) (num 2)) empty-env) (numV 3))
(test (eval (sub (num 1) (num 2)) empty-env) (numV -1))
(test (eval (mul (num 1) (num 2)) empty-env) (numV 2))
(test (eval (tt) empty-env) (boolV #t))
(test (eval (ff) empty-env) (boolV #f))
(test (eval (leq (num 1) (num 2)) empty-env) (boolV #t))
(test (eval (leq (num 2) (num 1)) empty-env) (boolV #f))
(test (eval (ifc (tt) (num 1) (num 2)) empty-env) (numV 1))
(test (eval (ifc (ff) (num 1) (num 2)) empty-env) (numV 2))
(test (eval (id 'x) (extend-env* (list (cons 'x (numV 1))) empty-env)) (numV 1))
(test (eval (fun (list 'x 'y) (add (id 'x) (id 'y))) empty-env)
      (closureV (list 'x 'y) (add (id 'x) (id 'y)) empty-env))
(test (eval (app (fun (list 'x 'y) (add (id 'x) (id 'y))) (list (num 1) (num 2))) empty-env)
      (numV 3))
(test/exn (eval (add (num 1) (tt)) empty-env) "invalid operands")
(test/exn (eval (add (num 1) (id 'x)) empty-env) "free identifier")


;; PARTE 2A

(define swap* '???)
(define curry* '???)
(define uncurry* '???)
(define partial* '???)

;; PARTE 2B

;; run :: ...
(define (run) '???)

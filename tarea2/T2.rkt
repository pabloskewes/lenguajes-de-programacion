#lang play
(print-only-errors #t)

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
        | (tupl ListOf[<expr>])
        | (proj <expr> <num>)
|#
(deftype Expr
  ;; core
  (num n)
  (add l r)
  (sub l r)
  (mul l r)
  ;; p1.a (booleanos y condicionales)
  (tt)
  (ff)
  (leq l r)
  (ifc c t e)
  ;; p1.f (tuplas)
  (tupl exprs*)
  (proj e n)
  ;; p1.b (funciones con múltiples parámetros)
  (id x)
  (fun params* body)
  (app f-name args)
)

;: Comentario: se dieron vuelta las posiciones de las implementaciones de los
;: constructores de tuplas/proyecciones y funciones/aplicaciones, porque, si bien
;: en el enunciado se va pidiendo su implementación en ese orden, en el código
;: tiene más sentido poner las tuplas primero ya que el matcheo de las funciones
;: aplica para cualquier símbolo, y el de las tuplas sólo para 'tuple y 'proj, por
;: lo que al cambiarlo de lugar, es más fácil mantener el mismo orden para los matcheos

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
            | <sym>
            | (list 'fun ListOf[<sym>] <s-expr>)
            | (list <sym> ListOf[<s-expr>])
            | (list 'tuple ListOf[<num>])
            | (list 'proj <s-expr> <num>)
|#

;; parse :: s-expr -> Expr
;; converts s-exprs into Exprs
(define (parse s-expr)
  (match s-expr
    ; core 
    [n #:when (number? n) (num n)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mul (parse l) (parse r))]
    ; p1.a
    ['true (tt)]
    ['false (ff)]
    [(list '<= l r) (leq (parse l) (parse r))]
    [(list 'if c t e) (ifc (parse c) (parse t) (parse e))]
    ; p1.f
    [(list 'tuple nums ...) (tupl (map parse nums))]
    [(list 'proj e n) (proj (parse e) (parse n))]
    ; p1.b
    [x #:when (symbol? x) (id x)]
    [(list 'fun (list params* ...) body) (fun params* (parse body))]
    [(list f-name args ...) (app (parse f-name) (map parse args))]
    ; error para cualquier otro caso
    [else (error 'parse "invalid s-expr: ~a" s-expr)]))
  

;; PARTE 1C, 1G

;; values of expressions
;; <value> ::= (numV <num>)
;;          | (boolV <bool>) 
;;          | (closureV <sym> <expr> <env>)
;;          | (tuplV ListOf[<value>])
(deftype Val
  ; p1.c  
  (numV n)
  (boolV b)
  (closureV params* body env)
  ; p1.g
  (tuplV vals*))


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


;; PARTE 1E, 1G

;; eval :: Expr Env -> Val
;; Evalúa una expresión en un ambiente dado.
(define (eval expr env)
  (match expr
    ; core
    [(num n) #:when (number? n) (numV n)]
    [(add l r) (num+ (eval l env) (eval r env))]
    [(sub l r) (num- (eval l env) (eval r env))]
    [(mul l r) (num* (eval l env) (eval r env))]
    ; p1.a
    [(tt) (boolV #t)]
    [(ff) (boolV #f)]
    [(leq l r) (num<= (eval l env) (eval r env))]
    [(ifc c t e) 
     (define boolv (eval c env))
     (def (boolV b) boolv)
     (if b (eval t env) (eval e env))]
    ; p1.f
    [(tupl exprs*) (tuplV (map (lambda (e) (eval e env)) exprs*))]
    [(proj (tupl exprs*) (num n)) 
     (def (tuplV vals*) (eval (tupl exprs*) env))
     (list-ref vals* (- n 1))]
    ; p1.b
    [(id x) #:when (symbol? x) (env-lookup x env)]
    [(fun params* body) (closureV params* body env)]
    [(app f-name args) 
     (def (closureV the-args the-body the-claus-env) (eval f-name env))
     (def the-ext-env (extend-env* (map cons the-args (map (lambda (arg) (eval arg env)) args)) the-claus-env))
     (eval the-body the-ext-env)]))    


;; PARTE 2A
;; Funciones globables dentro del lenguaje reconocido por el intérprete

;; swap* :: (A B -> C) -> (B A -> C)
;; Recibe una función (dentro del lenguaje) que recibe dos argumentos y retorna un valor,
;  y retorna una función que recibe dos argumentos en el orden inverso y 
; retorna el mismo valor, donde esta última función está dentro del lenguaje también.
(define swap* 
  (closureV '(f) (fun '(x y) (parse '(f y x))) empty-env))

;; curry* :: (A B -> C) -> (A -> B -> C) 
;; Recibe una función que recibe dos argumentos y retorna un valor, y retorna una
;; función que recibe un argumento y retorna una función que recibe un argumento
;; y retorna el mismo valor dentro del lenguaje.
(define curry* 
  (closureV '(f) (fun '(x) (fun '(y) (parse '(f x y)))) empty-env))

;; uncurry* :: ((A -> B -> C) -> (A B -> C))
;; Recibe una función que recibe un argumento y retorna una función que recibe un
;; argumento y retorna un valor, y retorna una función que recibe dos argumentos
;; y retorna el mismo valor dentro del lenguaje.
(define uncurry* 
  ; uncurry funciona así: uncurry* (lambda (f) (lambda (a b) ((f a) b)))
  (closureV '(f) (fun '(a b) (parse '((f a) b))) empty-env))

;; partial* :: ((A B -> C) A -> (B -> C))
;; Recibe una función que recibe dos argumentos A y B y retorna un valor C, y lo
;; transforma en una función que recibe un argumento B y retorna un valor C, que
;; es el resultado de aplicar la función original con el argumento A fijo.
(define partial*
  (closureV '(f a) (fun '(b) (parse '(f a b))) empty-env))


;; PARTE 2B

(define globals (list
  (cons 'swap swap*)
  (cons 'curry curry*)
  (cons 'uncurry uncurry*)
  (cons 'partial partial*)
))

;; run :: s-expr (Listof(Pair Symbol Val)) -> Val.
(define (run s-expr globals)
  (eval (parse s-expr) (extend-env* globals empty-env)))

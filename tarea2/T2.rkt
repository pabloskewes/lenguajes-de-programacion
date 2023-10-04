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
  ; ;; identificador
  ; (id x)
  ; ;; funcion con múltiples parámetros
  ; (fun (id ...) e)
  ; ;; aplicación con múltiples argumentos
  ; (app f (id ...)))
  

(test Expr? (num 1) #t)

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
    [else (error 'parse "invalid s-expr: ~a" s-expr)]))
  
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


;; PARTE 1C, 1G

(deftype Val
  ;...
  )

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

;; PARTE 1D

;; num2num-op :: ...
(define (num2num-op) '???)

;; num2bool-op :: ...
(define (num2bool-op) '???)

;; PARTE 1E, 1G

;; eval :: ...
(define (eval) '???)

;; PARTE 2A

(define swap* '???)
(define curry* '???)
(define uncurry* '???)
(define partial* '???)

;; PARTE 2B

;; run :: ...
(define (run) '???)

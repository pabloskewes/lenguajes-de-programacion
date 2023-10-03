#lang play

(let ((y 3)) (lambda (x) (+ x y)))
;(+ 5 ((lambda (x) x) 10))

;; Este tipo de ejemplos permite
;; "detectar" que tipo de alcance se usa

;; alcance estático: la asociación ocurre
;; al definir la función.

;; alcance dinámico: la asociación ocurre
;; al aplicar la función.

(let ((y 3))
  (let ((f (lambda (x) (+ x y))))
    (let ((y 5))
      (f 1))))

(deftype Value
  (numV n)
  (closureV id body env))

#|
int i = 10;
print(i+1);
... hay una secuencia de operaciones ...
|#

(define n1 (numV 12))
(define n2 (numV 13))
    
(define m1 (numV-n n1))
(define m2 (numV-n n2))

(let ((x 1))
  ;; secuencia implícita
  (def (numV m1) n1)
  (def (numV m2) n2)
  (+ m1 m2))

(let ((x 1))
  ;; secuencia implícita
  (define m1 (numV-n n1))
  (define m2 (numV-n n2))
  (+ m1 m2))

;; secuencia explícita
(begin
  (let ((x 1)) x)
  10)

(define c1 (closureV 'x '(+ x 1) (list)))
(define c2 (closureV 'x '(+ x y) (list (cons 'y 3))))

(let ((x 1))
  ;; secuencia implícita
  (define arg1 (closureV-id c1))
  (define body1 (closureV-body c1))
  (define env-clos1 (closureV-env c1))
  (define arg2 (closureV-id c2))
  (define body2 (closureV-body c2))
  (define env-clos2 (closureV-env c2))
  (+ 1 1))

;; def es un 'alias' de 'match-define' en Racket
;; permite combinar pattern matching con define.
(let ((x 1))
  ;; secuencia implícita
  ;(define arg1 (closureV-id c1))
  ;(define body1 (closureV-body c1))
  ;(define env-clos1 (closureV-env c1))
  ;; (def <las nuevas definiciones segun patron> <valor a matchear>)
  (def (closureV the-id the-body the-env) c1)
  
  (define arg2 (closureV-id c2))
  (define body2 (closureV-body c2))
  (define env-clos2 (closureV-env c2))
  (+ 1 1))

;(match c1
;  [(numV n) n]
;  [(closureV id b env) ...id...b...env])
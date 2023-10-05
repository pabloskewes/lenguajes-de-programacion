#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)
         | (id <sym>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <id> <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with id named-expr body)
  (id x))

;; calc :: Expr -> number
;; Evaluates an arithmetical expression
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(if0 c t f) (if (zero? (calc c)) (calc t) (calc f))]))

;; (print-only-errors #f)

(test (calc (num 1)) 1)
(test (calc (add (num 1) (num 1))) 2)
(test (calc (sub (num 1) (num 1))) 0)
(test (calc (if0 (num 0) (num 1) (num 2))) 1)
(test (calc (if0 (num 1) (num 1) (num 2))) 2)
(test (calc (if0 (add (num 1) (num 0)) (num 1) (num 2))) 2)
(test (calc (if0 (sub (num 1) (num 1)) (num 1) (num 2))) 1)
;; ...
;;(print-only-errors #t)
;; concrete syntax used
;; writing arithmetical expressions
#|
<s-expr> ::= <num>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
|#

;; parse :: s-expr -> Expr
;; converts s-exprs into Exprs
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'with (list x e) b) #:when (symbol? x) (with x (parse e) (parse b))]))

;; el operador ' se llama "quote"
(test (parse 1) (num 1))
(test (parse (list '+ 1 2)) (add (num 1) (num 2)))
(test (parse '(+ 7 9)) (add (num 7) (num 9)))
(test (parse (list '- 3 4)) (sub (num 3) (num 4)))
(test (parse (list '+ 3 (list '- 2 1))) (add (num 3) (sub (num 2) (num 1))))
(test (parse (list 'if0 0 1 2)) (if0 (num 0) (num 1) (num 2)))
(test (parse (list 'if0 (list '- 1 2) 1 2)) (if0 (sub (num 1) (num 2)) (num 1) (num 2)))
(test (parse '(with (x 1) (+ x 1))) (with 'x (num 1) (add (id 'x) (num 1))))
(test (parse '(with (x (+ 5 5)) (+ x x))) (with 'x (add (num 5) (num 5)) (add (id 'x) (id 'x))))

(test (calc (parse 1)) 1)
(test (calc (parse '(+ 1 1))) 2)
(test (calc (parse '(- 1 1))) 0)


;; Currification
;; max* :: number number -> number
;; has 2 arguments
(define (max* a b)
  (if (> a b) a b))

;; has 1 arguments
;; max-curry* :: number -> (number -> number)
(define (max-curry* a)
  (λ (b) (if (> a b) a b)))

;; max* :: number number -> number
;; max-curry* :: number -> (number -> number)

;; max** :: number number number -> number
(define (max** a b c)
  (max a b c))

;; max-curry** :: number -> (number number -> number)
(define (max-curry** a)
  (λ (b c) (max a b c)))

(define (max-curry*** a)
  (λ (b) (λ  (c) (max a b c))))

;; curry :: (A B -> C) -> (A -> (B -> C))
;; A, B y C son 'variables de tipo'... similar a los "Generics" en Java/Scala.
(define (curry f)
  (λ (a)
    (λ (b)
      (f a b))))


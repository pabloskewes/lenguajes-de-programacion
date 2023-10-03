#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)         
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <sym>)
         | (with <id> <expr> <expr>)         
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (with id named-expr body))

;; parse :: s-expr -> Expr
;; converts s-exprs into Exprs
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]    
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [x #:when (symbol? x) (id x)]
    [(list 'with (list x e) b) #:when (symbol? x) (with x (parse e) (parse b))]))


;; subst :: Expr Symbol Expr -> Expr
;; (subst in what for) substituye todas las ocurrencias
;; libres del identificador 'what' en la expresión 'in' por
;; la expresión 'for'.
(define (subst in what for)
  (match in
    [(num n) (num n)]
    [(add l r) (add (subst l what for) (subst r what for))]
    [(sub l r) (sub (subst l what for) (subst r what for))]
    [(if0 c t f) (if0 (subst c what for) (subst t what for) (subst f what for))]
    [(id x) (if (symbol=? x what) for (id x))]
    [(with x e b)
     (with x
           (subst e what for)
           (if (symbol=? x what)
               b ; x no está libre en el cuerpo b
               (subst b what for)))]))

;; calc :: Expr -> number
;; Evaluates an arithmetical expression
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(if0 c t f) (if (zero? (calc c)) (calc t) (calc f))]
    [(id x) (error 'calc "Open expression (free occurrence of ~a)" x)]
    [(with x e b)
     (calc (subst b x (num (calc e))))]))

;; tests sustitución
(test (calc (parse '(with (x 5) (+ x x)))) 10)
(test/exn (calc (parse '(with (x 5) (+ x z)))) "Open expression (free occurrence of z)")
(test (calc (parse '(with (x (+  5 5)) (with (y (- 3 x)) (+ x y))))) 3)
(test (calc (parse '(with (x 10) (with (x 1) x)))) 1)
(test (calc (parse '(with (x 10) (+ x (with (x 1) x))))) 11)

;; tests clase 5
(test (calc (num 1)) 1)
(test (calc (add (num 1) (num 1))) 2)
(test (calc (sub (num 1) (num 1))) 0)
(test (calc (if0 (num 0) (num 1) (num 2))) 1)
(test (calc (if0 (num 1) (num 1) (num 2))) 2)
(test (calc (if0 (add (num 1) (num 0)) (num 1) (num 2))) 2)
(test (calc (if0 (sub (num 1) (num 1)) (num 1) (num 2))) 1)

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


#|
(eval (parse '(f 1))       
      (list (fundef 'f 'x (parse '(+ x 1)))))

-----> (+ x 1) donde 'x' es el argumento de la función
-----> entonces (f 1) --> (+ 1 1) --> 2
-----> entonces (f 1) --> (+ x 1) con x=1 --> (+ 1 1) --> 2
-----> entonces (f 1) --> (subst (+x 1) 'x 1) --> (+ 1 1) --> 2

(eval (parse '(f (+ 1 10)))
      (list (fundef 'f 'x (parse '(+ x 1)))))
-----> entonces (f 1) --> (subst (+x 1) 'x (+ 1 10)) --> ...

(eval (parse '(f (with (x 1) (+ x x)))))
      (list (fundef 'f 'x (parse '(+ x 1)))))
-----> entonces (f 1) --> (subst (+x 1) 'x (with (x 1) (+ x x))) --> ...

(eval (parse '{with {z (+ 5 5)} (f z)})
      (list (fundef 'f 'x (parse '(+ x 1)))))
-----> entonces (f 1) --> (subst (+x 1) 'x (with (x 1) z)) --> ...
|#


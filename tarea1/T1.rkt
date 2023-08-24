#lang play

(print-only-errors #t)

#| P1 |#
#| Parte A |#

;; <Prop> ::= (varp <String>)
;;          | (notp <Prop>)
;;          | (andp <Prop> <Prop>)
;;          | (orp <Prop> <Prop>)
;; Define el tipo de datos Prop, que representa una proposición lógica.
(deftype Prop
    [varp value]
    [notp prop]
    [andp prop1 prop2]
    [orp prop1 prop2])

(test (Prop? (varp "a")) #t)
(test (Prop? (notp (varp "a"))) #t)
(test (Prop? (andp (varp "a") (varp "b"))) #t)
(test (Prop? (orp (varp "a") (varp "b"))) #t)
(test (Prop? (list "a")) #f)
(test (varp? (varp "a")) #t)
(test (varp? (notp (varp "a"))) #f)
(test (notp? (varp "a")) #f)
(test (notp? (notp (varp "a"))) #t)
(test (andp? (varp "a")) #f)
(test (andp? (andp (varp "a") (varp "b"))) #t)
(test (orp? (varp "a")) #f)
(test (orp? (orp (varp "a") (varp "b"))) #t)

#| Parte B |#

;; occurrences :: Prop String -> Number
;; devuelve la cantidad de veces que una variable aparece en una proposición
(define (occurrences prop var)
    (match prop
        [(varp v) (if (string=? v var) 1 0)]
        [(notp p) (occurrences p var)]
        [(andp p1 p2) (+ (occurrences p1 var) (occurrences p2 var))]
        [(orp p1 p2) (+ (occurrences p1 var) (occurrences p2 var))]))

(test (occurrences (varp "a") "a") 1)
(test (occurrences (varp "a") "b") 0)
(test (occurrences (notp (varp "a")) "a") 1)
(test (occurrences (notp (varp "a")) "b") 0)
(test (occurrences (andp (varp "a") (varp "b")) "a") 1)
(test (occurrences (andp (varp "a") (varp "b")) "b") 1)
(test (occurrences (andp (varp "a") (varp "b")) "c") 0)
(test (occurrences (orp (varp "a") (varp "b")) "a") 1)
(test (occurrences (orp (varp "a") (varp "b")) "b") 1)
(test (occurrences (orp (varp "a") (varp "b")) "c") 0)
(test (occurrences (andp (varp "a") (orp (varp "a") (varp "b"))) "a") 2)

#| Parte C |#

;; vars :: Prop -> (Listof String)
;; devuelve una lista con todos los nombres de variables que ocurren en la
;; proposición. La lista retornada no debe tener duplicados.
(define (vars prop)
    (match prop
        [(varp v) (list v)]
        [(notp p) (vars p)]
        [(andp p1 p2) (remove-duplicates (append (vars p1) (vars p2)))]
        [(orp p1 p2) (remove-duplicates (append (vars p1) (vars p2)))]))

(test (vars (varp "a")) (list "a"))
(test (vars (notp (varp "a"))) (list "a"))
(test (vars (andp (varp "a") (varp "b"))) (list "a" "b"))
(test (vars (orp (varp "a") (varp "b"))) (list "a" "b"))
(test (vars (andp (varp "a") (orp (varp "a") (varp "b")))) (list "a" "b"))


#| Parte D |#

;; all-environments :: (Listof String) -> (Listof (Listof (Pair String Boolean)))
;; dada una lista de variables (sin duplicados), crea todos los ambientes de evaluación posible
(define (all-environments vars)
    (match vars 
        ['() (list '())]
        [(cons head tail) (append (map (lambda (env) (cons (cons head #t) env)) (all-environments tail))
                                  (map (lambda (env) (cons (cons head #f) env)) (all-environments tail)))]))

(test (all-environments (list )) (list (list )))
(test (all-environments (list "a")) (list (list (cons "a" #t)) (list (cons "a" #f))))
(test (all-environments (list "a" "b")) (list (list (cons "a" #t) (cons "b" #t))
                                              (list (cons "a" #t) (cons "b" #f))
                                              (list (cons "a" #f) (cons "b" #t))
                                              (list (cons "a" #f) (cons "b" #f))))

#| Parte E |#

;; eval :: Prop (Listof (Pair String Boolean)) -> Boolean
;; evalúa una proposición p, obteniendo los valores de cada variables desde una ambiente env,
;; devolviendo el valor de verdad de dicha fórmula. Se asume que la lista no contiene dos veces una misma variable.
;; En caso de que el nombre de una variable no aparezca en el ambiente, lanza un error:
;; (error 'eval "variable <var-name> is not defined in environment"
(define (eval prop env)
    (match prop
        [(notp p) (not (eval p env))]
        [(andp p1 p2) (and (eval p1 env) (eval p2 env))]
        [(orp p1 p2) (or (eval p1 env) (eval p2 env))]
        [(varp v) (match (assoc v env)
                        [(cons v1 truth-value) truth-value]
                        [else (error 'eval "variable ~a is not defined in environment" v)])]))

(test (eval (varp "a") (list (cons "a" #t))) #t)
(test (eval (varp "a") (list (cons "a" #f))) #f)
(test (eval (notp (varp "a")) (list (cons "a" #t))) #f)
(test (eval (andp (varp "a") (varp "b")) (list (cons "a" #t) (cons "b" #t))) #t)
(test (eval (andp (varp "a") (varp "b")) (list (cons "a" #t) (cons "b" #f))) #f)
(test (eval (andp (varp "a") (varp "b")) (list (cons "a" #f) (cons "b" #t))) #f)
(test (eval (orp (varp "a") (varp "b")) (list (cons "a" #t) (cons "b" #t))) #t)
(test (eval (orp (varp "a") (varp "b")) (list (cons "a" #t) (cons "b" #f))) #t)
(test (eval (orp (varp "a") (varp "b")) (list (cons "a" #f) (cons "b" #f))) #f)
(test (eval (andp (varp "a") (orp (varp "a") (varp "b"))) (list (cons "a" #t) (cons "b" #f))) #t)
(test/exn (eval (varp "a") (list (cons "b" #t))) "not defined in environment")
(test/exn (eval (varp "a") (list )) "not defined in environment")

#| Parte F |#

;; tautology? :: Prop -> Boolean
;; retorna #t si la proposición es una tautología, es decir, si es verdadera para cualquier ambiente de evaluación
(define (tautology? prop)
    (let ([envs (all-environments (vars prop))])
        (fold (lambda env all-true? (and all-true? (eval prop env))) #t envs)))
    
    
#| P2 |#

#| Parte A |#

;; simplify-negations :: Prop -> Prop

#| Parte B |#

;; distribute-and :: Prop -> Prop

#| Parte C |#

;; apply-until :: (a -> a) (a a -> Boolean) -> a -> a

#| Parte D |#

;; DNF :: Prop -> Prop



#| P3 |#

#| Parte A |#

;; fold-prop :: (String -> a) (a a -> a) (a a -> a) (a -> a) -> Prop -> a

#| Parte B |#

;; occurrences-2 :: Prop String -> Number

;; vars-2 :: Prop -> (Listof String)

;; eval-2 :: Prop (Listof (Pair String Boolean)) -> Boolean

;; simplify-negations-2 :: Prop -> Prop

;; distribute-and-2 :: Prop -> Prop

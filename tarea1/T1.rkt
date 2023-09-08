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

;; print-prop :: Prop -> String
;; imprime una proposición en forma de string, para que sea más fácil de leer/debuggear
(define (print-prop prop)
    (match prop
        [(varp v) v]
        [(notp p) (string-append "~(" (print-prop p) ")")]
        [(andp p1 p2) (string-append "(" (print-prop p1) " ^ " (print-prop p2) ")")]
        [(orp p1 p2) (string-append "(" (print-prop p1) " v " (print-prop p2) ")")]))

(test (print-prop (varp "a")) "a")
(test (print-prop (notp (varp "a"))) "~(a)")
(test (print-prop (andp (varp "a") (varp "b"))) "(a ^ b)")
(test (print-prop (orp (varp "a") (varp "b"))) "(a v b)")
(test (print-prop (andp (varp "a") (orp (varp "a") (varp "b")))) "(a ^ (a v b))")
(test (print-prop (orp (varp "a") (notp (varp "a")))) "(a v ~(a))")
(test (print-prop (orp (andp (varp "a") (varp "b")) (orp (varp "a") (varp "b")))) "((a ^ b) v (a v b))")

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
        (foldl (lambda (env all_true) (and all_true (eval prop env))) #t envs)))
    
(test (tautology? (varp "a")) #f)
(test (tautology? (notp (varp "a"))) #f)
(test (tautology? (andp (varp "a") (varp "b"))) #f)
(test (tautology? (orp (varp "a") (varp "b"))) #f)
(test (tautology? (andp (varp "a") (orp (varp "a") (varp "b")))) #f)
(test (tautology? (orp (varp "a") (notp (varp "a")))) #t)
    
#| P2 |#

#| Parte A |#

;; simplify-negations :: Prop -> Prop
;; simplifica las negaciones de una proposición, aplicando las siguientes reglas:
;; ~(~p) = p ; ~(p ^ q) = ~p v ~q ; ~(p v q) = ~p ^ ~q
(define (simplify-negations prop)
    (match prop
        [(varp p) prop]
        [(notp (varp p)) prop]
        [(andp p1 p2) (andp (simplify-negations p1) (simplify-negations p2))]
        [(orp p1 p2) (orp (simplify-negations p1) (simplify-negations p2))]
        [(notp (notp p)) simplify-negations p] ; ~~p = p
        [(notp (andp p1 p2)) (orp (notp (simplify-negations p1)) (notp (simplify-negations p2)))] ; ~(p ^ q) = ~p v ~q
        [(notp (orp p1 p2)) (andp (notp (simplify-negations p1)) (notp (simplify-negations p2)))] ; ~(p v q) = ~p ^ ~q
    )
)

(test (simplify-negations (notp (notp (varp "a")))) (varp "a"))
(test (simplify-negations (notp (andp (varp "a") (varp "b")))) (orp (notp (varp "a")) (notp (varp "b"))))
(test (simplify-negations (notp (orp (varp "a") (varp "b")))) (andp (notp (varp "a")) (notp (varp "b"))))
(test (simplify-negations (notp (notp (andp (varp "a") (varp "b"))))) (andp (varp "a") (varp "b")))
(test (simplify-negations (notp (notp (orp (varp "a") (varp "b"))))) (orp (varp "a") (varp "b")))
(test (simplify-negations (notp (orp (notp (varp "a")) (varp "b")))) (andp (notp (notp (varp "a"))) (notp (varp "b"))))

#| Parte B |#

;; distribute-and :: Prop -> Prop

;; distribuye los ands en una proposición, aplicando las siguientes reglas:
;; (p v q) ^ r = (p ^ r) v (q ^ r) ; p ^ (q v r) = (p ^ q) v (p ^ r)
;; distribuye por la derecha primero.
(define (distribute-and prop)
    (match prop
        [(varp p) prop]
        [(notp (varp p)) prop]

        [(andp (orp p1 p2) p3) (orp (distribute-and (andp p1 p3)) (distribute-and (andp p2 p3)))] ; (p v q) ^ r = (p ^ r) v (q ^ r)        
        [(andp p1 (orp p2 p3)) (orp (distribute-and (andp p1 p2)) (distribute-and (andp p1 p3)))] ; p ^ (q v r) = (p ^ q) v (p ^ r)

        [(andp p1 p2) (andp (distribute-and p1) (distribute-and p2))]
        [(orp p1 p2) (orp (distribute-and p1) (distribute-and p2))]
    )
)

(test (distribute-and (andp (varp "a") (orp (varp "b") (varp "c")))) (orp (andp (varp "a") (varp "b")) (andp (varp "a") (varp "c"))))
(test (distribute-and (andp (orp (varp "a") (varp "b")) (varp "c"))) (orp (andp (varp "a") (varp "c")) (andp (varp "b") (varp "c"))))
(test (distribute-and (andp (varp "a") (varp "b"))) (andp (varp "a") (varp "b")))
(test (distribute-and (orp (varp "a") (varp "b"))) (orp (varp "a") (varp "b")))
(test (distribute-and (notp (varp "a"))) (notp (varp "a")))
(test 
    (distribute-and (andp (notp (varp "a")) (orp (varp "b") (varp "c"))))   
    (orp (andp (notp (varp "a")) (varp "b")) (andp (notp (varp "a")) (varp "c")))
)


#| Parte C |#

;; apply-until :: (a -> a) (a a -> Boolean) -> a -> a
;; aplica una función f hasta que se cumpla una condición c donde c recibe el valor anterior y el nuevo valor
;; devuelve la función que toma un valor inicial, aplica f hasta que se cumpla c y devuelve el último valor
(define (apply-until func cond-func)
    (lambda (initial-value)
        (let ([x-new (func initial-value)])
            (if (cond-func initial-value x-new)
                x-new
                ((apply-until func cond-func) x-new)))))

(test ((apply-until (lambda (x) (+ x 1)) (lambda (x new-x) (> new-x 10))) 0) 11)
(test ((apply-until (lambda (x) (/ x (add1 x))) (lambda (x new-x) (<= (- x new-x) 0.1))) 1) 0.25)
(define (drop-one-item lst item)
    (match lst
        ['() '()]
        [(cons head tail) (if 
                            (equal? head item)
                            tail
                            (cons head (drop-one-item tail item)))]))
(test ((apply-until (lambda (lst) (drop-one-item lst 1)) (lambda (lst new-lst) (equal? lst new-lst))) (list 1 1 2 3 1 4 1 5)) (list 2 3 4 5))

#| Parte D |#

;; DNF :: Prop -> Prop
;; devuelve la forma normal disyuntiva de una proposición
(define (DNF prop)
    (define (simplify prop)
        (distribute-and (simplify-negations prop)))
    ((apply-until simplify (lambda (p1 p2) (equal? p1 p2))) prop))

(test (DNF (andp (varp "a") (orp (varp "b") (varp "c"))))
    (orp (andp (varp "a") (varp "b")) (andp (varp "a") (varp "c"))))
(test 
    (DNF (andp (orp (varp "a") (varp "b")) (orp (varp "c") (varp "d"))))
    (orp
        (orp (andp (varp "a") (varp "c"))
            (andp (varp "a") (varp "d")))
        (orp (andp (varp "b") (varp "c"))
            (andp (varp "b") (varp "d")))
    )
)
(test 
    (DNF (orp (notp (notp (andp (varp "a") (orp (varp "b") (varp "c"))))) (orp (varp "f") (varp "g"))))
    (orp
        (orp (andp (varp "a") (varp "b")) (andp (varp "a") (varp "c")))
        (orp (varp "f") (varp "g"))
    )
)


#| P3 |#

#| Parte A |#

;; fold-prop :: (String -> a) (a a -> a) (a a -> a) (a -> a) -> Prop -> a
;; captura el esquema de recursión asociado a Prop
(define (fold-prop var-f and-f or-f not-f)
    (lambda (prop)
        (match prop
            [(varp v) (var-f v)]
            [(andp p1 p2) (and-f ((fold-prop var-f and-f or-f not-f) p1) ((fold-prop var-f and-f or-f not-f) p2))]
            [(orp p1 p2) (or-f ((fold-prop var-f and-f or-f not-f) p1) ((fold-prop var-f and-f or-f not-f) p2))]
            [(notp p) (not-f ((fold-prop var-f and-f or-f not-f) p))])))

(test ((fold-prop (lambda (v) 1) + + +) (varp "a")) 1)

#| Parte B |#

;; occurrences-2 :: Prop String -> Number
;; devuelve la cantidad de veces que una variable aparece en una proposición
;; usando fold-prop
(define (occurrences-2 prop var)
    ((fold-prop 
        (lambda (v) (if (string=? v var) 1 0))
        + 
        +
        identity
    ) prop))

(test (occurrences-2 (varp "a") "a") 1)
(test (occurrences-2 (varp "a") "b") 0)
(test (occurrences-2 (notp (varp "a")) "a") 1)
(test (occurrences-2 (notp (varp "a")) "b") 0)
(test (occurrences-2 (andp (varp "a") (varp "b")) "a") 1)
(test (occurrences-2 (andp (varp "a") (varp "b")) "b") 1)
(test (occurrences-2 (andp (varp "a") (varp "b")) "c") 0)
(test (occurrences-2 (orp (varp "a") (varp "b")) "a") 1)
(test (occurrences-2 (orp (varp "a") (varp "b")) "b") 1)
(test (occurrences-2 (orp (varp "a") (varp "b")) "c") 0)
(test (occurrences-2 (andp (varp "a") (orp (varp "a") (varp "b"))) "a") 2)

;; vars-2 :: Prop -> (Listof String)
;; devuelve una lista con todos los nombres de variables que ocurren en la
;; proposición usando fold-prop. La lista retornada no debe tener duplicados.
(define (vars-2 prop)
    ((fold-prop 
        (lambda (v) (list v)) ; var-f
        (lambda (l1 l2) (remove-duplicates (append l1 l2))) ; and-f
        (lambda (l1 l2) (remove-duplicates (append l1 l2))) ; or-f
        identity ; not-f
    ) prop))

(test (vars-2 (varp "a")) (list "a"))
(test (vars-2 (notp (varp "a"))) (list "a"))
(test (vars-2 (andp (varp "a") (varp "b"))) (list "a" "b"))
(test (vars-2 (orp (varp "a") (varp "b"))) (list "a" "b"))
(test (vars-2 (andp (varp "a") (orp (varp "a") (varp "b")))) (list "a" "b"))

;; eval-2 :: Prop (Listof (Pair String Boolean)) -> Boolean
;; evalúa una proposición p, obteniendo los valores de cada variables desde una ambiente env,
;; devolviendo el valor de verdad de dicha fórmula. Se asume que la lista no contiene dos veces una misma variable.
;; En caso de que el nombre de una variable no aparezca en el ambiente, lanza un error:
;; (error 'eval "variable <var-name> is not defined in environment".
;; Usando fold-prop
(define (eval-2 prop env)
    ((fold-prop 
        (lambda (v) (match (assoc v env)
                        [(cons v1 truth-value) truth-value]
                        [else (error 'eval "variable ~a is not defined in environment" v)])) ; var-f
        (lambda (p1 p2) (and p1 p2)) ; and-f
        (lambda (p1 p2) (or p1 p2)) ; or-f
        (lambda (p) (not p)) ; not-f
    ) prop))

(test (eval-2 (varp "a") (list (cons "a" #t))) #t)
(test (eval-2 (varp "a") (list (cons "a" #f))) #f)
(test (eval-2 (notp (varp "a")) (list (cons "a" #t))) #f)
(test (eval-2 (andp (varp "a") (varp "b")) (list (cons "a" #t) (cons "b" #t))) #t)
(test (eval-2 (andp (varp "a") (varp "b")) (list (cons "a" #t) (cons "b" #f))) #f)
(test (eval-2 (andp (varp "a") (varp "b")) (list (cons "a" #f) (cons "b" #t))) #f)
(test (eval-2 (orp (varp "a") (varp "b")) (list (cons "a" #t) (cons "b" #t))) #t)
(test (eval-2 (orp (varp "a") (varp "b")) (list (cons "a" #t) (cons "b" #f))) #t)
(test (eval-2 (orp (varp "a") (varp "b")) (list (cons "a" #f) (cons "b" #f))) #f)
(test (eval-2 (andp (varp "a") (orp (varp "a") (varp "b"))) (list (cons "a" #t) (cons "b" #f))) #t)
(test/exn (eval-2 (varp "a") (list (cons "b" #t))) "not defined in environment")
(test/exn (eval-2 (varp "a") (list )) "not defined in environment")


; TODO: fix this
;; simplify-negations-2 :: Prop -> Prop
;; simplifica las negaciones de una proposición usando fold-prop, aplicando las siguientes reglas:
;; ~(~p) = p ; ~(p ^ q) = ~p v ~q ; ~(p v q) = ~p ^ ~q
(define (simplify-negations-2 prop)
    ((fold-prop 
        (lambda (v) (varp v)) ; var-f
        (lambda (p1 p2) (andp p1 p2)) ; and-f
        (lambda (p1 p2) (orp p1 p2)) ; or-f
        (lambda (p) (match p
                        [(notp p1) p1]
                        [(andp p1 p2) (orp (notp p1) (notp p2))]
                        [(orp p1 p2) (andp (notp p1) (notp p2))]
                        [else (notp p)])) ; not-f
    ) prop))

(test (simplify-negations-2 (notp (notp (varp "a")))) (varp "a"))
(test (simplify-negations-2 (notp (andp (varp "a") (varp "b")))) (orp (notp (varp "a")) (notp (varp "b"))))
(test (simplify-negations-2 (notp (orp (varp "a") (varp "b")))) (andp (notp (varp "a")) (notp (varp "b"))))
(test (simplify-negations-2 (notp (notp (andp (varp "a") (varp "b"))))) (andp (varp "a") (varp "b")))
(test (simplify-negations-2 (notp (notp (orp (varp "a") (varp "b"))))) (orp (varp "a") (varp "b")))
(test (simplify-negations-2 (notp (orp (notp (varp "a")) (varp "b")))) (andp (notp (notp (varp "a"))) (notp (varp "b"))))

;; distribute-and-2 :: Prop -> Prop


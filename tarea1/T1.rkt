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

;; print-prop :: Prop -> String
;; imprime una proposición en forma de string, para que sea más fácil de leer/debuggear
(define (print-prop prop)
    (match prop
        [(varp v) v]
        [(notp p) (string-append "~(" (print-prop p) ")")]
        [(andp p1 p2) (string-append "(" (print-prop p1) " ^ " (print-prop p2) ")")]
        [(orp p1 p2) (string-append "(" (print-prop p1) " v " (print-prop p2) ")")]))

#| Parte B |#

;; occurrences :: Prop String -> Number
;; devuelve la cantidad de veces que una variable aparece en una proposición
(define (occurrences prop var)
    (match prop
        [(varp v) (if (string=? v var) 1 0)]
        [(notp p) (occurrences p var)]
        [(andp p1 p2) (+ (occurrences p1 var) (occurrences p2 var))]
        [(orp p1 p2) (+ (occurrences p1 var) (occurrences p2 var))]))

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

#| Parte D |#

;; all-environments :: (Listof String) -> (Listof (Listof (Pair String Boolean)))
;; dada una lista de variables (sin duplicados), crea todos los ambientes de evaluación posible
(define (all-environments vars)
    (match vars 
        ['() (list '())]
        [(cons head tail) (append (map (lambda (env) (cons (cons head #t) env)) (all-environments tail))
                                  (map (lambda (env) (cons (cons head #f) env)) (all-environments tail)))]))

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

#| Parte F |#

;; tautology? :: Prop -> Boolean
;; retorna #t si la proposición es una tautología, es decir, si es verdadera para cualquier ambiente de evaluación
(define (tautology? prop)
    (let ([envs (all-environments (vars prop))])
        (foldl (lambda (env all_true) (and all_true (eval prop env))) #t envs)))
    
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

#| Parte D |#

;; DNF :: Prop -> Prop
;; devuelve la forma normal disyuntiva de una proposición
(define (DNF prop)
    (define (simplify prop)
        (distribute-and (simplify-negations prop)))
    ((apply-until simplify (lambda (p1 p2) (equal? p1 p2))) prop))

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

;; distribute-and-2 :: Prop -> Prop
;; distribuye los ands en una proposición usando fold-prop, aplicando las siguientes reglas:
;; (p v q) ^ r = (p ^ r) v (q ^ r) ; p ^ (q v r) = (p ^ q) v (p ^ r)
(define (distribute-and-2 prop)
    ((fold-prop 
        (lambda (v) (varp v)) ; var-f
        (lambda (p1 p2) (andp p1 p2)) ; and-f
        (lambda (p1 p2) (orp p1 p2)) ; or-f
        (lambda (p) (match p 
                        [(andp (orp p1 p2) p3) (orp (andp p1 p3) (andp p2 p3))] ; (p v q) ^ r = (p ^ r) v (q ^ r)        
                        [(andp p1 (orp p2 p3)) (orp (andp p1 p2) (andp p1 p3))] ; p ^ (q v r) = (p ^ q) v (p ^ r)
                        [else p])) ; not-f
    ) prop))

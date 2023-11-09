#lang play
(require "T3.rkt")

(print-only-errors #t)

;; Tests P1

;; Expr
(test (Expr? (num 1)) #t)
(test (Expr? (binop '+ (num 1) (num 2))) #t)
(test (Expr? (binop '- (num 1) (num 2))) #t)
(test (Expr? (binop '* (num 1) (num 2))) #t)
(test (Expr? (binop '<= (num 1) (num 2))) #t)
(test (Expr? (id 'x)) #t)
(test (Expr? (tt)) #t)
(test (Expr? (ff)) #t)
(test (Expr? (ifc (binop '<= (num 1) (num 2)) (num 1) (num 2))) #t)


;; Type
(test (Type? (numT)) #t)
(test (Type? (arrowT (numT) (numT))) #t)
(test (Type? (arrowT (numT) (arrowT (numT) (numT)))) #t)
(test (Type? 10) #f)
(test (Type? 'foo) #f)
(test (Type? (boolT)) #t)
(test (Type? (arrowT (boolT) (boolT))) #t)

(test (Expr? (fun 'x (numT) (id 'x))) #t)
(test (Expr? (app (fun 'x (numT) (id 'x)) (num 1))) #t)


;; parse-type : s-expr-type -> Type 
; tests p1.a
(test (parse-type 'Number) (numT))
(test (parse-type '(-> Number Number)) (arrowT (numT) (numT)))
(test (parse-type '(-> (-> Number Number) Number)) (arrowT (arrowT (numT) (numT)) (numT)))
(test/exn (parse-type 'foo) "invalid type: foo")
; tests p1.c
(test (parse-type 'Boolean) (boolT))
(test (parse-type '(-> Boolean Boolean)) (arrowT (boolT) (boolT)))
(test (parse-type '(-> (-> Boolean Number) Boolean)) (arrowT (arrowT (boolT) (numT)) (boolT)))


;; parse : s-expr -> Expr
(test (parse 10) (num 10))
(test (parse 'foo) (id 'foo))
(test (parse '(+ 10 20)) (binop '+ (num 10) (num 20)))
(test (parse '(- 10 20)) (binop '- (num 10) (num 20)))
(test (parse '(* 10 20)) (binop '* (num 10) (num 20)))
(test (parse '(fun (x : Number) x)) (fun 'x (numT) (id 'x)))
(test (parse '(fun (x : Number) (+ x 1))) (fun 'x (numT) (binop '+ (id 'x) (num 1))))
(test (parse '(fun (x : Number) (+ x x))) (fun 'x (numT) (binop '+ (id 'x) (id 'x))))
(test (parse '(if (<= 5 6) true false)) (ifc (binop '<= (num 5) (num 6)) (tt) (ff)))


;; infer-type : Expr TypeEnv -> Type
; tests p1.a
(test (infer-type (num 1) empty-tenv) (numT))
(test (infer-type (fun 'x (numT) (id 'x)) empty-tenv) (arrowT (numT) (numT)))
(test (infer-type (fun 'x (arrowT (numT) (numT)) (id 'x)) empty-tenv)
(arrowT (arrowT (numT) (numT)) (arrowT (numT) (numT))))
(test/exn (infer-type (binop '+ (num 1) (fun 'x (numT) (id 'x))) empty-tenv) "infer-type: invalid operand for +")
(test/exn (infer-type (app (num 1) (num 2)) empty-tenv) "infer-type: function application to a non-function")
(test/exn (infer-type (app (fun 'x (numT) (id 'x)) (fun 'x (numT) (id 'x))) empty-tenv) "infer-type: function argument type mismatch")

(test (infer-type (parse 10) empty-tenv) (numT))
(test (infer-type (parse '(+ 10 20)) empty-tenv) (numT))
(test (infer-type (parse '(- 10 20)) empty-tenv) (numT))
(test (infer-type (parse '(* 10 20)) empty-tenv) (numT))
(test (infer-type (parse 'foo) (extend-tenv 'foo (numT) empty-tenv)) (numT))
(test (infer-type (parse '(fun (x : Number) x)) empty-tenv) (arrowT (numT) (numT)))
(test (infer-type (parse '(fun (x : Number) (+ x 1))) empty-tenv) (arrowT (numT) (numT)))
(test (infer-type (parse '(fun (x : Number) (+ x x))) empty-tenv) (arrowT (numT) (numT)))
(test/exn (infer-type (parse '(fun (x : Number) (+ x (fun (y : Number) y)))) empty-tenv) "infer-type: invalid operand for +")
(test/exn (infer-type (parse '(fun (x : Number) (+ x y))) empty-tenv) "tenv-lookup")

; tests p1.c
(test (infer-type ( ifc (binop '<= (num 5) (num 6)) (num 2) (num 3))
empty-tenv) (numT))
(test/exn (infer-type ( ifc (num 5) (num 2) (num 3)) empty-tenv) "infer-type: if condition must be a boolean")
(test/exn (infer-type ( ifc (binop '<= (num 5) (num 6)) (num 2) (tt)) empty-tenv) "infer-type: if branches type mismatch")

(test (infer-type (parse 'true) empty-tenv) (boolT))
(test (infer-type (parse 'false) empty-tenv) (boolT))
(test (infer-type (parse '(<= 5 6)) empty-tenv) (boolT))
(test (infer-type (parse '(if (<= 5 6) true false)) empty-tenv) (boolT))
(test/exn (infer-type (parse '(if (<= 5 6) 5 false)) empty-tenv) "infer-type: if branches type mismatch")
(test/exn (infer-type (parse '(if (<= 5 6) true 5)) empty-tenv) "infer-type: if branches type mismatch")
(test/exn (infer-type (parse '(if (+ 5 6) true false)) empty-tenv) "infer-type: if condition must be a boolean")


;; Tests P2

;; final? : Expr -> Boolean
(test (final? (num 1)) #t)
(test (final? (id 'x)) #t)
(test (final? (fun 'x (numT) (id 'x))) #t)
(test (final? (binop '+ (num 1) (num 2))) #f)
(test (final? (app (fun 'x (numT) (id 'x)) (num 1))) #f)


;; Kont
(test (Kont? (mt-k)) #t)
(test (Kont? (binop-r-k '+ (num 2) empty-env empty-kont)) #t)
(test (Kont? (binop-l-k '+ (num 1) empty-env empty-kont)) #t)
(test (Kont? (arg-k (num 1) empty-env empty-kont)) #t)
(test (Kont? (fun-k (fun 'x (numT) (id 'x)) empty-env empty-kont)) #t)


;; State
(test (State? (st (num 1) empty-env empty-kont)) #t)
(test (State? (st (num 1) empty-env (binop-r-k '+ (num 2) empty-env empty-kont))) #t)


;; inject : Expr -> State
(test (inject (num 1)) (st (num 1) empty-env empty-kont))
(test (inject (id 'x)) (st (id 'x) empty-env empty-kont))
(test (inject (fun 'x (numT) (id 'x))) (st (fun 'x (numT) (id 'x)) empty-env empty-kont))
(test (inject (binop '+ (num 1) (num 2))) (st (binop '+ (num 1) (num 2)) empty-env empty-kont))


;; step : State -> State
(test
(step (st (binop '+ (num 1) (num 2)) (mtEnv) (mt-k)))
(st (num 1) (mtEnv) (binop-r-k '+ (num 2) (mtEnv) (mt-k)))) ; (Rleft)

(test 
(step (st (num 1) (mtEnv) (binop-r-k '+ (num 2) (mtEnv) (mt-k))))
(st (num 2) (mtEnv) (binop-l-k '+ (num 1) (mtEnv) (mt-k)))) ; (Rright)

(test 
(step (st (num 2) (mtEnv) (binop-l-k '+ (num 1) (mtEnv) (mt-k))))
(st (num 3) (mtEnv) (mt-k))) ; (Rbinop)

(test 
(step (st (app (fun 'x (numT) (id 'x)) (num 2))
(mtEnv)
(mt-k)))
(st (fun 'x (numT) (id 'x)) (mtEnv) (arg-k (num 2)
(mtEnv)
(mt-k)))) ; (Rfun)

(test
(step (st (fun 'x (numT) (id 'x)) (mtEnv) (arg-k (num 2)
(mtEnv)
(mt-k))))
(st (num 2)
(mtEnv)
(fun-k (fun 'x (numT) (id 'x)) (mtEnv) (mt-k)))) ; (Rarg)

(test
(step (st (num 2)
(mtEnv)
(fun-k (fun 'x (numT) (id 'x)) (mtEnv) (mt-k))))
(st (id 'x)
(extend-env 'x (cons (num 2) (mtEnv)) (mtEnv))
(mt-k))) ; (Rapp)

(test
(step (st (id 'x)
(extend-env 'x (cons (num 2) (mtEnv)) (mtEnv))
(mt-k)))
(st (num 2) (mtEnv) (mt-k))) ; (Rvar)


;; run : s-expr -> (Val, Type)
(test (run '(+ 1 2)) (cons (num 3) (numT)))
(test (run '(fun (x : Number) x)) (cons (fun 'x (numT) (id 'x)) (arrowT (numT) (numT))))
(test (run '(fun (x : Number) (+ x 1))) (cons (fun 'x (numT) (binop '+ (id 'x) (num 1))) (arrowT (numT) (numT))))
(test (run '(fun (x : Number) (- x x))) (cons (fun 'x (numT) (binop '- (id 'x) (id 'x))) (arrowT (numT) (numT))))
(test (run '(* 10 20)) (cons (num 200) (numT))) 

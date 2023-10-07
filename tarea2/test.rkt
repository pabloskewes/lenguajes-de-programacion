#lang play
(require "T2.rkt")

(print-only-errors #t)

; I) tests Expr

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
(test (Expr? (tupl (list (num 1) (num 2) (num 3)))) #t)
(test (Expr? (proj (tupl (list (num 1) (num 2) (num 3))) (num 1))) #t)

; II) tests parse

; tests core
(test (parse 1) (num 1))
(test (parse (list '+ 1 2)) (add (num 1) (num 2)))
(test (parse '(+ 7 9)) (add (num 7) (num 9)))
(test (parse (list '- 3 4)) (sub (num 3) (num 4)))
(test (parse (list '+ 3 (list '- 2 1))) (add (num 3) (sub (num 2) (num 1))))
(test (parse (list '* 10 (list '+ 1 2))) (mul (num 10) (add (num 1) (num 2))))
(test (parse (list '* (list '+ 5 6) 7)) (mul (add (num 5) (num 6)) (num 7)))
; tests p1.a
(test (parse 'true) (tt))
(test (parse 'false) (ff))
(test (parse (list '<= 1 2)) (leq (num 1) (num 2)))
(test (parse (list '<= 1 (list '+ 1 2))) (leq (num 1) (add (num 1) (num 2))))
(test (parse (list '<= (list '+ 1 2) 1)) (leq (add (num 1) (num 2)) (num 1)))
(test (parse '(<= 3 5)) (leq (num 3) (num 5)))
(test (parse (list 'if 0 1 2)) (ifc (num 0) (num 1) (num 2)))
(test (parse (list 'if (list '- 1 2) 1 2)) (ifc (sub (num 1) (num 2)) (num 1) (num 2)))
(test (parse '(if (<= 3 5) 2 4)) (ifc (leq (num 3) (num 5)) (num 2) (num 4)))
(test (parse '(if true 2 4)) (ifc (tt) (num 2) (num 4)))
; tests p1.b
(test (parse 'x) (id 'x))
(test (parse '(fun (x y) (+ x y))) (fun (list 'x 'y) (add (id 'x) (id 'y))))
(test (parse '(my-function 2 3 4)) (app (id 'my-function) (list (num 2) (num 3) (num 4))))
; tests p1.f
(test (parse '(tuple 1 2 3)) (tupl (list (num 1) (num 2) (num 3))))
(test (parse '(proj (tuple 1 2 3) 1)) (proj (tupl (list (num 1) (num 2) (num 3))) (num 1))) 
(test (parse '(proj (tuple 1 2 3) 2)) (proj (tupl (list (num 1) (num 2) (num 3))) (num 2)))


; III) tests extend-env* (auxiliary function)
(test (extend-env* (list) empty-env) empty-env)
(test (extend-env* (list (cons 'x (numV 1)) (cons 'y (numV 2))) empty-env)
      (aEnv 'y (numV 2) (aEnv 'x (numV 1) empty-env)))
(define anEnv (extend-env* (list (cons 'x (numV 1)) (cons 'y (numV 2))) empty-env))
(test (extend-env* (list (cons 'z (numV 3))) anEnv)
      (aEnv 'z (numV 3) (aEnv 'y (numV 2) (aEnv 'x (numV 1) empty-env))))


; IV) tests operator functions

(test (num+ (numV 1) (numV 2)) (numV 3))
(test (num- (numV 1) (numV 2)) (numV -1))
(test (num* (numV 1) (numV 2)) (numV 2))
(test (num<= (numV 1) (numV 2)) (boolV #t))
(test (num<= (numV 2) (numV 1)) (boolV #f))
(test/exn (num+ (numV 1) (boolV #t)) "invalid operands")


; V) tests eval

; tests core
(test (eval (num 1) empty-env) (numV 1))
(test (eval (add (num 1) (num 2)) empty-env) (numV 3))
(test (eval (sub (num 1) (num 2)) empty-env) (numV -1))
(test (eval (mul (num 1) (num 2)) empty-env) (numV 2))
; core + parse
(test (eval (parse '1) empty-env) (numV 1))
(test (eval (parse '(+ 1 2)) empty-env) (numV 3))
(test (eval (parse '(- 1 2)) empty-env) (numV -1))
(test (eval (parse '(* 1 2)) empty-env) (numV 2))
; tests p1.a
(test (eval (tt) empty-env) (boolV #t))
(test (eval (ff) empty-env) (boolV #f))
(test (eval (leq (num 1) (num 2)) empty-env) (boolV #t))
(test (eval (leq (num 2) (num 1)) empty-env) (boolV #f))
(test (eval (ifc (tt) (num 1) (num 2)) empty-env) (numV 1))
(test (eval (ifc (ff) (num 1) (num 2)) empty-env) (numV 2))
; p1.a + parse
(test (eval (parse 'true) empty-env) (boolV #t))
(test (eval (parse 'false) empty-env) (boolV #f))
(test (eval (parse '(<= 1 2)) empty-env) (boolV #t))
(test (eval (parse '(<= 2 1)) empty-env) (boolV #f))
(test (eval (parse '(if true 1 2)) empty-env) (numV 1))
(test (eval (parse '(if false 1 2)) empty-env) (numV 2))
; tests p1.b
(test (eval (id 'x) (extend-env* (list (cons 'x (numV 1))) empty-env)) (numV 1))
(test (eval (fun (list 'x 'y) (add (id 'x) (id 'y))) empty-env)
      (closureV (list 'x 'y) (add (id 'x) (id 'y)) empty-env))
(test (eval (app (fun (list 'x 'y) (add (id 'x) (id 'y))) (list (num 1) (num 2))) empty-env)
      (numV 3))
(test/exn (eval (add (num 1) (tt)) empty-env) "invalid operands")
(test/exn (eval (add (num 1) (id 'x)) empty-env) "free identifier")
; p1.b + parse
(test (eval (parse 'x) (extend-env* (list (cons 'x (numV 1))) empty-env)) (numV 1))
(test (eval (parse '(fun (x y) (+ x y))) empty-env)
      (closureV (list 'x 'y) (add (id 'x) (id 'y)) empty-env))
(test (eval (parse '((fun (x y) (+ x y)) 1 2)) empty-env) (numV 3))
(test/exn (eval (parse '(+ 1 true)) empty-env) "invalid operands")
(test/exn (eval (parse '(+ 1 x)) empty-env) "free identifier")
; tests p1.g
(test (eval (tupl (list (num 1) (num 2) (num 3))) empty-env)
      (tuplV (list (numV 1) (numV 2) (numV 3))))
(test (eval (proj (tupl (list (num 1) (num 2) (num 3))) (num 1)) empty-env)
      (numV 1))
; p1.g + parse
(test (eval (parse '(tuple 1 2 3)) empty-env)
      (tuplV (list (numV 1) (numV 2) (numV 3))))
(test (eval (parse '(proj (tuple 1 2 3) 1)) empty-env)
      (numV 1))

; VI) tests globals functions (inside the language)

; swap*
; f(x,y) = x - y
(test (eval 
(parse '((swap (fun (x y) (- x y))) 2 1)) ; source code
(extend-env 'swap swap* empty-env) ; environment
) (numV -1)) ; expected result

; f(x,y) = 2x + y
(test (eval
(parse '((swap (fun (x y) (+ (* 2 x) y))) 2 1)) ; source code
(extend-env 'swap swap* empty-env)) ; environment
      (numV 4)) ; expected result

; function without env
(test/exn (eval 
(parse '((swap (fun (x y) (- x y))) 2 1)) ; source code
empty-env) "free identifier")

; curry*
; f(x, y) = x + y
(test (eval
(parse '(((curry (fun (x y) (+ x y))) 2) 1)) ; source code
(extend-env 'curry curry* empty-env)) ; environment
      (numV 3)) ; expected result

; f(x, y) = (2x + y) <= 5 
(test (eval
(parse '(((curry (fun (x y) (<= (+ (* 2 x) y) 5))) 2) 1)) ; source code
(extend-env 'curry curry* empty-env)) ; environment
      (boolV #t)) ; expected result

; function without env
(test/exn (eval
(parse '(((curry (fun (x y) (+ x y))) 2) 1)) ; source code
empty-env) "free identifier")

; uncurry*
; f(x, y) = x - y
(test (eval
(parse '((uncurry (fun (x) (fun (y) (- x y)))) 2 1)) ; source code
(extend-env 'uncurry uncurry* empty-env)) ; environment
      (numV 1)) ; expected result

; f(x, y) = (2x + y) <= 5
(test (eval
(parse '((uncurry (fun (x) (fun (y) (<= (+ (* 2 x) y) 5)))) 2 1)) ; source code
(extend-env 'uncurry uncurry* empty-env)) ; environment
      (boolV #t)) ; expected result

; function without env
(test/exn (eval
(parse '((uncurry (fun (x) (fun (y) (- x y)))) 2 1)) ; source code
empty-env) "free identifier")

; partial*
; f(x, y) = x + y
(test (eval
(parse '((partial (fun (x y) (+ x y)) 2) 4)) ; source code
(extend-env 'partial partial* empty-env)) ; environment
      (numV 6)) ; expected result

; f(x, y) = (2x + y) <= 5
(test (eval
(parse '((partial (fun (x y) (<= (+ (* 2 x) y) 5)) 2) 4)) ; source code
(extend-env 'partial partial* empty-env)) ; environment
      (boolV #f)) ; expected result

; function without env
(test/exn (eval
(parse '((partial (fun (x y) (+ x y)) 2) 4)) ; source code
empty-env) "free identifier")


; VII) tests run

(test (run '1 globals) (numV 1))
(test (run '(+ 1 2) globals) (numV 3))

(test (run 
  '((swap (fun (x y) (<= x y))) 1 2) ; source code
  globals) ; environment
  (boolV #f)) ; expected result

(test (run
'((partial (fun (x y) (<= x y)) 1) 2) ; source code
globals) ; environment
(boolV #t)) ; expected result

(test (run
'(((curry (fun (x y) (<= x y))) 1) 2)
globals) ; environment
(boolV #t)) ; expected result

(test (run
'((uncurry (fun (x) (fun (y) (<= x y)))) 1 2) ; source code
globals) ; environment
(boolV #t)) ; expected result

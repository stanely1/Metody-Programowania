#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add) (sub) (mul) (div) (eql) (leq))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op] [l : Exp] [r : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (varE [x : Symbol])
  (letE [ls : (Listof (Symbol * Exp))] [e : Exp])
  (let*E [ls : (Listof (Symbol * Exp))] [e : Exp]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    ;; parse let
    [(s-exp-match? `{let ANY ANY} s)
     (letE (parse-let-list (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let* ANY ANY} s)
     (let*E (parse-let-list (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [else (error 'parse "invalid input")]))

;; parsownie listy wiązań
(define (parse-let-list [ls : S-Exp]) : (Listof (Symbol * Exp))
  (map (lambda (x)
         (pair (s-exp->symbol (first (s-exp->list x)))
               (parse (second (s-exp->list x)))))
       (s-exp->list ls)))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))

;; eval --------------------------------------

;; values

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean]))

;; primitive operations

(define (op-num-num->proc [f : (Number Number -> Number)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (numV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op-num-bool->proc [f : (Number Number -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (boolV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(div) (op-num-num->proc /)]
    [(eql) (op-num-bool->proc =)]
    [(leq) (op-num-bool->proc <=)]))

;; environments

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define (extend-env [env : Env] [x : Symbol] [v : Value]) : Env
  (cons (bind x v) env))
(define (lookup-env [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? n (bind-name b))
                         (bind-val b)]
                        [else (lookup-env n rst-env)])]))

;; evaluation function

(define (eval [e : Exp] [env : Env]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(opE o l r) ((op->proc o) (eval l env) (eval r env))]
    [(ifE b l r)
     (type-case Value (eval b env)
       [(boolV v)
        (if v (eval l env) (eval r env))]
       [else
        (error 'eval "type error")])]
    [(varE x)
     (lookup-env x env)]
    [(letE ls e)
     (eval e (bind-let-list ls env))]
    [(let*E ls e)
     (eval e (bind-let*-list ls env))]))

;; wiazanie listy - let* (wiazania sa widoczne w następnych wiązaniach)
(define (bind-let*-list [ls : (Listof (Symbol * Exp))] [env : Env]) : Env
  (type-case (Listof (Symbol * Exp)) ls
    [empty env]
    [(cons p ls)
     (bind-let*-list ls (extend-env env (fst p) (eval (snd p) env)))]))

;; wiązanie listy - let (wiązanie niewidoczne dla nastepnych wiązań)
(define (bind-let-list [ls : (Listof (Symbol * Exp))] [env : Env]) : Env
  (append
   (foldr (lambda (b env) ;; nowe srodowisko - z nowymi wiązaniami
           (if (member (fst b) ;; sprawdzam czy nie ma duplikatow
                       (map (lambda (x) (bind-name x)) env))
               (error 'let "duplicate identifier")
               (extend-env env (fst b) (snd b))))
         mt-env
         (map (lambda (x) (pair (fst x) (eval (snd x) env))) ls))
   env)) ;; doklejam nowe srodowisko do env

(define (run [e : S-Exp]) : Value
  (eval (parse e) mt-env))

(module+ test
  (test (run `(let ([x 1] [y 2])
                (let [(z (+ x y))]
                  (+ (* y z) x))))
        (numV (let ([x 1] [y 2])
                (let [(z (+ x y))]
                  (+ (* y z) x)))))
  (test (run `(let* ([x 1]
                     [y (+ x 1)]
                     [z (* y 2)])
                (+ x (+ y z))))
        (numV (let* ([x 1]
                     [y (+ x 1)]
                     [z (* y 2)])
                (+ x (+ y z))))))
                           

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e) mt-env)))

#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op-bin
  (add) (sub) (mul) (div)
  (eql) (leq) (and-op) (or-op)) 

(define-type Op-un
  (not-op) (neg)) ;; neg - minus unarny, not-op - negacja logiczna - operatory unarne

(define-type Exp
  (numE [n : Number])
  (op-binE [op : Op-bin] [l : Exp] [r : Exp])
  (op-unE  [op : Op-un]  [e : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (condE [cs : (Listof (Exp * Exp))]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{cond ANY ...} s)
     (condE (parse-cond (rest (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (op-binE (parse-op-bin (s-exp->symbol (first (s-exp->list s))))
              (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY} s)
     (op-unE  (parse-op-un (s-exp->symbol (first (s-exp->list s))))
              (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-cond [ss : (Listof S-Exp)]) : (Listof (Exp * Exp))
  (type-case (Listof S-Exp) ss
    [empty
     empty]
    [(cons s ss)
     (if (s-exp-match? `{ANY ANY} s)
         (cons (pair (parse (first (s-exp->list s)))
                     (parse (second (s-exp->list s))))
               (parse-cond ss))
         (error 'parse "invalid input: cond"))]))

(define (parse-op-bin [op : Symbol]) : Op-bin
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
    [(eq? op 'and) (and-op)]
    [(eq? op 'or) (or-op)]
    [else (error 'parse "unknown operator")]))

;; nowe
(define (parse-op-un [op : Symbol]) : Op-un
  (cond
    [(eq? op '-) (neg)]
    [(eq? op 'not) (not-op)]
    [else (error 'parse "unknown operator")]))
  
;; eval --------------------------------------

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean]))

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

;; nowe
(define (op-bool-bool->proc [f : (Boolean Boolean -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(boolV n1)
       (type-case Value v2
         [(boolV n2)
          (boolV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")]))) 

(define (op-un-num->proc [f : (Number -> Number)]) : (Value -> Value)
  (lambda (x)
    (type-case Value x
      [(numV x) (numV (f x))]
      [else (error 'eval "type error")])))

(define (op-un-bool->proc [f : (Boolean -> Boolean)]) : (Value -> Value)
  (lambda (x)
    (type-case Value x
      [(boolV x) (boolV (f x))]
      [else (error 'eval "type error")])))

;; zmienione
(define (op-bin->proc [op : Op-bin]) : (Value Value -> Value)
  (type-case Op-bin op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(div) (op-num-num->proc /)]
    [(eql) (op-num-bool->proc =)]
    [(leq) (op-num-bool->proc <=)]
    [(and-op) (op-bool-bool->proc (lambda (v1 v2) (and v1 v2)))]
    [(or-op)  (op-bool-bool->proc (lambda (v1 v2) (or v1 v2)))]))

;; nowe
(define (op-un->proc [op : Op-un]) : (Value -> Value)
  (type-case Op-un op
    [(neg) (op-un-num->proc  (lambda (x) (* -1 x)))] ;; -x
    [(not-op) (op-un-bool->proc (lambda (x) (not x)))]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(op-binE o l r) ((op-bin->proc o) (eval l) (eval r))]
    [(op-unE o e) ((op-un->proc o) (eval e))]
    [(ifE b l r)
     (type-case Value (eval b)
       [(boolV v)
        (if v (eval l) (eval r))]
       [else
        (error 'eval "type error")])]
    [(condE cs)
     (eval (cond->if cs))]))

(define (cond->if [cs : (Listof (Exp * Exp))]) : Exp
  (type-case (Listof (Exp * Exp)) cs
    [empty
     (numE 42)]
    [(cons c cs)
     (ifE (fst c)
          (snd c )
          (cond->if cs))]))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))

(module+ test
  (test (run `(and (<= 1 2) (= 3 4))) (boolV #f))
  (test (run `(and (<= 1 2) (= 4 4))) (boolV #t))
  (test (run `(or (<= 1 2) (= 3 4))) (boolV #t))
  (test (run `(or (<= 3 2) (= 3 4))) (boolV #f))
  (test (run `(not (<= 4 5))) (boolV #f))
  (test (run `(not (= 6 5))) (boolV #t))
  (test (run `(- 5)) (numV -5))
  (test (run `(- -8)) (numV 8)))
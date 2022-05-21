#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add) (sub) (mul) (div) (eql) (leq))

(define-type Exp
  (numE [n : Number])
  (boolE [b : Boolean]) ; stale #t #f
  (opE [op : Op] [l : Exp] [r : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(or (s-exp-match? `#t s) (s-exp-match? `#f s)) ;; dodane parsowanie stalych boolowskich
     (boolE (s-exp->boolean s))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))

(module+ test
  (test (parse `#f) (boolE #f))
  (test (parse `#t) (boolE #t))
  (test (parse `(if #t #t #f)) (ifE (boolE #t) (boolE #t) (boolE #f))))

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

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(div) (op-num-num->proc /)]
    [(eql) (op-num-bool->proc =)]
    [(leq) (op-num-bool->proc <=)]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(boolE b) (boolV b)]
    [(opE o l r) ((op->proc o) (eval l) (eval r))]
    [(ifE b l r)
     (type-case Value (eval b)
       [(boolV v)
        (if v (eval l) (eval r))]
       [else
        (error 'eval "type error")])]))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

(module+ test
  (test (run `#f) (boolV #f))
  (test (run `#t) (boolV #t))
  (test (run `(if #t (if #f 1 2) #f)) (numV 2)))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))

;; abstract machine

(define-type Stack
  (emptyS)
  (rightS [op : Op] [exp : Exp] [s : Stack])
  (leftS  [op : Op] [val : Value] [s : Stack])
  (ifS    [l : Exp] [r : Exp] [s : Stack])) ;; if

(define (evalAM [e : Exp] [s : Stack]) : Value
  (type-case Exp e
    [(numE n)  (continueAM s (numV n))]
    [(boolE b) (continueAM s (boolV b))]
    [(opE op e1 e2) (evalAM e1 (rightS op e2 s))]
    [(ifE b l r) (evalAM b (ifS l r s))]))

(define (continueAM [s : Stack] [v : Value]) : Value
  (type-case Stack s
    [(emptyS) v]
    [(rightS op e s)
     (evalAM e (leftS op v s))]
    [(leftS op u s)
     (continueAM s ((op->proc op) u v))]
    [(ifS l r s)
     (type-case Value v
       [(boolV b)
        (evalAM (if b l r) s)]
       [else (error 'evalAM "type error")])]))
  
(define (runAM [e : S-Exp]) : Value
  (evalAM (parse e) (emptyS)))

#;(trace evalAM)

(define (mainAM [e : S-Exp]) : Void
  (print-value (runAM e)))

(define test1 `(+ 7 (if (<= 8 9) 87 (if #t (= 7 7) #f))))
(define test2 `(- 9 (+ 6 (if (= 9 9) (if #t 3 #f) (<= #f 12)))))

(module+ test
  (test (runAM test1) (run test1))
  (test (runAM test2) (run test2)))
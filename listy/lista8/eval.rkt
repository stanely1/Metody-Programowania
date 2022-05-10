#lang plait

(require "syntax.rkt")
(require (typed-in "parser.rkt"
                   (parse-exp : (S-Exp -> Exp))))

(define (op->func op)
  (type-case Op op
    [(op-add) +]
    [(op-sub) -]
    [(op-mul) *]
    [(op-div) /]))

(define (eval e)
  (type-case Exp e
    [(exp-number n) n]
    [(exp-op op e1 e2)
     ((op->func op) (eval e1) (eval e2))]))

(define (calc e)
  (eval (parse-exp e)))
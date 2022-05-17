#lang plait

(require "syntax.rkt")
(require (typed-in "zad1.rkt"
                   (parse-exp : (S-Exp -> Exp))))

(define (pow x n) ; x ^ n
  (local [(define (it x n r)
            (if (= 0 n)
                r
                (it (* x x)
                    (floor (/ n 2))
                    (if (= (modulo n 2) 1)
                        (* x r)
                        r))))]
    (it x n 1)))

(define (fact n) ; n!
  (if (= 0 n)
      1
      (* n (fact (- n 1)))))
   
(define (op-bin->func op)
  (type-case Op-bin op
    [(op-add) +]
    [(op-sub) -]
    [(op-mul) *]
    [(op-div) /]
    [(op-pow) pow]))

(define (op-un-pref->func op)
  (type-case Op-un-pref op
    [(op-neg) (lambda (x) (* -1 x))]))

(define (op-un-suf->func op)
  (type-case Op-un-suf op
    [(op-fact) fact]))

(define (eval e)
  (type-case Exp e
    [(exp-number n) n]
    [(exp-op-bin op e1 e2)
     ((op-bin->func op) (eval e1) (eval e2))]
    [(exp-op-un-pref op e)
     ((op-un-pref->func op) (eval e))]
    [(exp-op-un-suf e op)
     ((op-un-suf->func op) (eval e))]))

(define (calc e)
  (eval (parse-exp e)))
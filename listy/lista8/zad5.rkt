#lang racket

(require "parsing.rkt")

(define grammar
  `(("operator"
     ((+) ,op-add)
     ((-) ,op-sub)
     ((*) ,op-mul)
     ((/) ,op-div))
    
    ("expression"
      (("simple-expr" "expr-rest") ,(lambda (e1 r) (r e1)))) ; wywolujemy funkcje ktora dostajemy dla expr-rest
    
    ("expr-rest"
      (("operator" "simple-expr") ,(lambda (op e2) (lambda (e1) (exp-op op e1 e2)))) ; funkcja budujaca operator
      (() ,(lambda () (lambda (e) e)))) ; samo wyrazenie proste
    
    ("simple-expr"
     ((NUMBER)           ,exp-number)
     (( ("expression") ) ,(lambda (e) e)))))

(define (run-exp-parser se)
  (run-named-parser grammar "expression" (list se)))

(define (parse-exp se)
  (run-exp-parser (s-exp-content se)))

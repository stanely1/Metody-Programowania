#lang racket

(require "parsing.rkt")

(define grammar
  `(("operator"
     ((+) ,(lambda () +))
     ((-) ,(lambda () -))
     ((*) ,(lambda () *))
     ((/) ,(lambda () /)))
    
    ("expression"
     (("simple-expr" "operator" "simple-expr")
          ,(lambda (e1 op e2) (op e1 e2)))
     (("simple-expr") ,(lambda (e) e)))
    
    ("simple-expr"
     ((NUMBER)           ,(lambda (e) e))
     (( ("expression") ) ,(lambda (e) e)))))

(define (run-exp-parser se)
  (run-named-parser grammar "expression" (list se)))

(define (parse-exp se)
  (run-exp-parser (s-exp-content se)))

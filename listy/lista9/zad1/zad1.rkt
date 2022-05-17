#lang racket

(require "../../lista8/parsing.rkt")
(require "syntax.rkt")
(require (only-in plait s-exp-content))

(provide parse-exp)

(define grammar
  `(("add-operator"
     ((+) ,op-add)
     ((-) ,op-sub))

    ("mult-operator"
     ((*) ,op-mul)
     ((/) ,op-div))

    ("pow-operator" ; silniejszy niz mult
     ((^) ,op-pow))

    ("pref-un-operator"
     ((-) ,op-neg))

    ("suf-un-operator"
     ((!) ,op-fact))
    
    ("expression"
     (("mult-expr" "expr-rest")
          ,(lambda (e1 f) (f e1))))

    ("expr-rest"
     (("add-operator" "mult-expr" "expr-rest")
         ,(lambda (op e2 f) (lambda (e1)
                              (f (exp-op-bin op e1 e2)))))
     (() ,(lambda () (lambda (e) e))))

    ("mult-expr"
     (("pow-expr" "mult-expr-rest")
         ,(lambda (e1 f) (f e1))))

    ("mult-expr-rest"
     (("mult-operator" "pow-expr" "mult-expr-rest")
         ,(lambda (op e2 f) (lambda (e1)
                              (f (exp-op-bin op e1 e2)))))
     (() ,(lambda () (lambda (e) e))))

    ("pow-expr"
     (("simple-expr" "pow-expr-rest") ,(lambda (e1 f) (f e1))))

    ("pow-expr-rest"
     (("pow-operator" "simple-expr" "pow-expr-rest")
      ,(lambda (op e2 f) (lambda (e1)
                           (f (exp-op-bin op e1 e2)))))
     (("suf-un-operator" "pow-expr-rest"), (lambda (op f) (lambda (e) (f (exp-op-un-suf e op)))))
     (() ,(lambda () identity)))
    
    ("simple-expr"
     ((NUMBER)           ,exp-number)
     (( ("expression") ) ,(lambda (e) e))
     (("pref-un-operator" "simple-expr") ,(lambda (op e) (exp-op-un-pref op e))))))

(define (run-exp-parser se)
  (run-named-parser grammar "expression" (list se)))

(define (parse-exp se)
  (run-exp-parser (s-exp-content se)))
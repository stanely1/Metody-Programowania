#lang racket

(require "../../lista8/parsing.rkt")
(require "syntax.rkt")

(define grammar
  `(("special-form"
     ((if "expression" "expression" "expression")
      ,(lambda (c t e) (exp-if c t e)))
     ((cond ("expression" "expression") "cond-rest")
      ,(lambda (e1 e2 f) (exp-cond (f (cons e1 e2)))))
     ((let (("variable" "expression") "let-def-rest") "expression")
      ,(lambda (v e1 f e) (exp-let (f (cons v e1)) e))))

    ("cond-rest"
     ((("expression" "expression") "cond-rest")
      ,(lambda (e1 e2 f) (lambda (e) (cons e (f (cons e1 e2))))))
     (() ,(lambda () identity)))

    ("let-def-rest"
     ((("variable" "expression") "let-def-rest")
      ,(lambda (e1 e2 f) (lambda (e) (cons e (f (cons e1 e2))))))
     (() ,(lambda () identity)))

    ("variable"
     ((SYMBOL) ,exp-var))

    ("expression"
     ((NUMBER) ,exp-number)
     (("variable") ,identity)
     (("special-form") ,identity)
     ((lambda ("variable" "lambda-rest") "expression")
      ,(lambda (v f e) (exp-lambda (f v) e)))
     (("variable" "expression" "app-rest")
      ,(lambda (a e1 f) (exp-app a (f e1)))))

    ("app-rest"
     (("expression" "app-rest")
      ,(lambda (e1 f) (lambda (e2) (cons e1 (f e2)))))
     (() ,(lambda () identity)))))

(define (run-exp-parser se)
  (run-named-parser grammar "expression" (list se)))
#lang racket

(define/contract (suffixes xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  ;(-> list? (listof list?)) ; ten jest za slaby
  (if (null? xs)
      (list null)
      (cons xs (suffixes (cdr xs)))))
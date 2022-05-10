#lang racket

(define/contract (sublists xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (if (null? xs)
      (list null)
      (let ([x (sublists (cdr xs))])
        (append (map (lambda (ys) (cons (car xs) ys)) x) x))))

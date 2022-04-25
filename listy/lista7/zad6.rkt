#lang racket

(define/contract (foldl-map f a xs)
  (parametric->/c [a b x] (-> (-> x a (cons/c b a)) a (listof x) (cons/c (listof b) a)))
  (define (it a xs ys)
    (if (null? xs)
        (cons (reverse ys) a)
        (let [(p (f (car xs) a))]
          (it (cdr p)
              (cdr xs)
              (cons (car p) ys)))))
  (it a xs null))
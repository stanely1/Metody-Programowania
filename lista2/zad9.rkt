#lang racket

(define (split xs)
  (define len-xs (length xs))
  (define (it xs n ac)
    (if (= n (quotient len-xs 2))
        (cons ac xs)
        (it (cdr xs) (- n 1) (cons (car xs) ac))))
  (it xs len-xs null))

(define (merge xs ys)
  (cond [(equal? xs null) ys]
        [(equal? ys null) xs]
        [(< (car xs) (car ys)) (cons (car xs) (merge (cdr xs) ys))]
        [else (cons (car ys) (merge xs (cdr ys)))]))
  
(define (merge-sort xs)
  (if (<= (length xs) 1)
      xs
      (let [(x (split xs))]
        (merge (merge-sort (car x)) (merge-sort (cdr x))))))

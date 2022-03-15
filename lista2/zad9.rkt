#lang racket

(define (split xs)
  (define len-xs (length xs))
  (define (it xs n ac)
    (if (= n (quotient len-xs 2))
        (cons ac xs)
        (it (cdr xs) (- n 1) (append ac (list (car xs))))))
  (it xs len-xs null))

(define (merge xs ys)
  (define (it xs ys ac)
    (cond [(equal? xs null) (append ac ys)]
          [(equal? ys null) (append ac xs)]
          [(< (car xs) (car ys))
           (it (cdr xs) ys (append ac (list (car xs))))]
          [else (it xs (cdr ys) (append ac (list (car ys))))]))
  (it xs ys null))

(define (merge-sort xs)
  (if (<= (length xs) 1)
      xs
      (let [(x (split xs))]
        (merge (merge-sort (car x)) (merge-sort (cdr x))))))
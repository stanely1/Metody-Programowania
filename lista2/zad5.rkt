#lang racket

(define (maximum xs)
  (define (it xs ac)
    (if (equal? xs null) ac
        (if (> (car xs) ac)
            (it (cdr xs) (car xs))
            (it (cdr xs) ac))))
  (it xs -inf.0))
#lang racket

(define (sorted? xs)
  (define (it xs ac)
    (if (equal? xs null)
        #t
        (if (< (car xs) ac)
            #f
            (it (cdr xs) (car xs)))))
  (it xs -inf.0))
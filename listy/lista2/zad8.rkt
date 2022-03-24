#lang racket

(define (select xs)
  (if (<= (length xs) 1)
      xs
      (let ([x (select (cdr xs))])
        (if (< (car xs) (car x))
            xs
            (cons (car x)
                  (cons (car xs) (cdr x)))))))

(define (select-sort xs)
  (define (it xs ac)
    (if (equal? xs null)
        ac
        (let ([x (select xs)])
          (it (cdr x) (append ac (list (car x)))))))
  (it xs null))
          
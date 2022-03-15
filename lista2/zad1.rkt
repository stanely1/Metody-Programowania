#lang racket

 (define (fib n)
   (if (<= n 1)
       n
       (+ [fib (- n 1)] [fib (- n 2)])))

(define (fib-iter n)
  (define (it n ac1 ac2) ;ac1 = fib n-1 ; ac2 = fib n-2
    (if(= n 1)
       ac1
       (it [- n 1] [+ ac1 ac2] ac1)))
  (if (<= n 1)
      n
      (it n 1 0)))
#lang racket

(define (fib n cont)
  (if (< n 2)
      (cont n)
      (fib (- n 1)
           (lambda (fib-of-n-1)
             (fib (- n 2)
                  (lambda (fib-of-n-2)
                    (cont (+ fib-of-n-1 fib-of-n-2))))))))
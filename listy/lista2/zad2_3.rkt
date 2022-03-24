#lang racket

(define-struct matrix (a b c d)) ; |a b|
                                 ; |c d|
(define (matrix-mult m n)
  (matrix (+ [* (matrix-a m) (matrix-a n)] [* (matrix-b m) (matrix-c n)])   ;a
          (+ [* (matrix-a m) (matrix-b n)] [* (matrix-b m) (matrix-d n)])   ;b
          (+ [* (matrix-c m) (matrix-a n)] [* (matrix-d m) (matrix-c n)])   ;c
          (+ [* (matrix-c m) (matrix-b n)] [* (matrix-d m) (matrix-d n)]))) ;d

(define matrix-id (matrix 1 0 0 1))

(define (matrix-exp m k) ;zwykle
  (define (it m k ac)
    (if (= k 0)
        ac
        (it m (- k 1) (matrix-mult ac m))))
  (it m k matrix-id))

(define (matrix-exp-fast m k) ;szybkie
  (define (it m k ac)
    (if (= k 0)
        ac
        (if (= (bitwise-and k 1) 1) ; k = 1 mod 2
            (it (matrix-mult m m) (quotient k 2) (matrix-mult ac m))
            (it (matrix-mult m m) (quotient k 2) ac))))
  (it m k matrix-id))

(define (fib-matrix k)
  (define fib-base-matrix (matrix 1 1 1 0))
  (define fib-res-matrix (matrix-exp fib-base-matrix k))
  (matrix-b fib-res-matrix))

(define (fib-matrix-fast k)
  (define fib-base-matrix (matrix 1 1 1 0))
  (define fib-res-matrix (matrix-exp-fast fib-base-matrix k))
  (matrix-b fib-res-matrix))

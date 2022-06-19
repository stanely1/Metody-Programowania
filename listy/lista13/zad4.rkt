#lang racket

;; delay and force

(define-syntax-rule
  (stream-cons v s)
  (cons v (delay s)))

(define stream-car car)

(define (stream-cdr s)
  (force (cdr s)))

(define stream-null null)
(define stream-null? null?)

;; operations on streams

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; prime stream

(define (divides? n m)
  (= (modulo m n) 0))

(define (prime? n s l)
  (cond
    [(< l 0) #t]
    [(divides? (stream-ref s l) n) #f]
    [else (prime? n s (- l 1))]))

(define (next-prime n s l)
  (if (prime? n s l)
      n
      (next-prime (+ n 1) s l)))

(define (prime-stream n s l)
  (stream-cons n (prime-stream (next-prime (+ n 1) (stream-cons n s) l)
                               (stream-cons n s)
                               (+ l 1))))

(define primes (prime-stream 2 stream-null 0))
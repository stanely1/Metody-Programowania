#lang racket

(require "streams.rkt")

;; zad 1
(define fact-stream
  (stream-cons 1 (map2 * (integers-from 1) fact-stream)))

;; zad 2
(define (partial-sums s)
  (stream-cons (stream-car s) (map2 + (partial-sums s) (stream-cdr s))))

(define p (partial-sums (integers-from 0)))

;; zad 3
(define (merge s t)
  (let ([scar (stream-car s)]
        [tcar (stream-car t)])
    (cond [(= scar tcar)
           (stream-cons scar (merge (stream-cdr s) (stream-cdr t)))]
          [(< scar tcar)
           (stream-cons scar (merge (stream-cdr s) t))]
          [else (stream-cons tcar (merge s (stream-cdr t)))])))

(define (scale s n)
  (stream-cons (* n (stream-car s)) (scale (stream-cdr s) n)))

(define hamming-stream
  (letrec ([s2 (stream-cons 2 (merge (scale s2 2) (merge (scale s2 3) (scale s2 5))))]
           [s3 (stream-cons 3 (merge (scale s3 2) (merge (scale s3 3) (scale s3 5))))]
           [s5 (stream-cons 5 (merge (scale s5 2) (merge (scale s5 3) (scale s5 5))))])
    (merge s2 (merge s3 s5))))
#lang racket

(define x #f)
(define y 11)
(define (war) #t #;(< 1 y))

(or (and (war) x) y)
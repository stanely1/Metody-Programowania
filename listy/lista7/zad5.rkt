#lang racket

(define p1 (parametric->/c [a b] (-> a b a)))
; (-> a - neg  b - neg  a - pos)

(define p2 (parametric->/c [a b c] (-> (-> a b c) (-> a b) a c)))
; (-> (-> a - neg  b - neg  c - pos) (-> a - neg  b - pos) a - neg  c - pos)

(define p3 (parametric->/c [a b c] (-> (-> b c) (-> a b) (-> a c))))
; (-> (-> b - neg  c - pos) (-> a - neg  b - pos) (-> a - neg  c - pos))

(define p4 (parametric->/c [a] (-> (-> (-> a a) a) a)))
; (-> (-> (-> a - neg  a - pos) a - pos) a - pos)

(define/contract (f1 a b)
  p1
  a)

(define/contract (f2 f g x)
  p2
  (f x (g x)))

(define/contract (f3 f g) ; compose
  p3
  (lambda (x) (f (g x))))

(define/contract (f4 f)
  p4
  (f identity))
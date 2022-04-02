#lang plait

(define (f1 x y) x) ; ('a 'b -> 'a)

(define (f2 f g x) ; (('a 'b -> 'c) ('a -> 'b) 'a -> 'c)
  (f x (g x)))

(define (f3 [f : (('a -> 'a) -> 'a)]) ; ((('a -> 'a) -> 'a) -> 'a)
  (f (lambda (x) x)))

(define (f4 f g) ; (('a -> 'b) ('a -> 'c) -> ('a -> ('b * 'c)))
  (lambda (x) (pair (f x) (g x))))

(define (f5 f x)
  (list (snd (some-v (f (fst (some-v (f x)))))))) ; (('a -> (Optionof ('a * 'b))) 'a -> (Listof 'b))

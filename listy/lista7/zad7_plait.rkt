#lang plait

;zmieniony typ
(define (fold-right [f : ('a 'a -> 'a)] [x : 'a] [xs : (Listof 'a)])
  (if (empty? xs)
      x
      (f (first xs) (fold-right f x (rest xs)))))
#lang racket

; poprawna definicja:
(define/contract (fold-right f x xs)
  (parametric->/c [a b] (-> (-> a b b) b (listof a) b))
  (if (null? xs)
      x
      (f (car xs) (fold-right f x (cdr xs)))))

; zmieniony kontrakt + bledna implementacja:
(define/contract (fold-right1 f x xs)
  (parametric->/c [a] (-> (-> a a a) a (listof a) a))
  (if (null? xs)
      x
      (f (fold-right1 f x (cdr xs)) (car xs))))

;zmieniony kontrakt (bez zmiany implementacji): -> nie ogranicza sposobu uzyktowania
(define/contract (fold-right2 f x xs)
  (parametric->/c [a] (-> (-> a a a) a (listof a) a))
  (if (null? xs)
      x
      (f (car xs) (fold-right2 f x (cdr xs)))))

;zmieniony typ ogranicza - nie mozna zrobic np (fold-right cons empty xs)
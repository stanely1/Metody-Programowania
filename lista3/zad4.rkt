#lang racket

(define ((my-compose f g) x)
  (f (g x)))

;((my-compose square inc) 5) -> (square (inc 5)) 
;(my-compose square inc) zwroci procedure przyjmującą x i zwracajaca
;(square (inc x)) potem podstawi x=5

;((my-compose inc square) 5) -> (inc (square 5)) / analogicznie
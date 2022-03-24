#lang racket

(define (foldr-reverse xs)
  (foldr (lambda (y ys) (append ys (list y))) null xs))

(length (foldr-reverse (build-list 10000 identity)))

;xs [0|]->[1|]->...->[n|]->null
;                    [n|]->null
;            [n|]->[n-1|]->null
;            ...
;   [n|]->...->[0|]->null
; stworzy n(n+1) / 2, z czego nieuzytki to n(n-1)/2 = O(n^2)
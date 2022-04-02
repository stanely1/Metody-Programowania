#lang plait

;ZAD 2
(define (apply f x) (f x)) ; (('a -> 'b) 'a -> 'b)
(define (compose f g) (lambda (x) (f (g x)))) ; (('a -> 'b) ('c -> 'a) -> ('c -> 'b)
(define (flip f) (lambda (x y) (f y x))) ; (('a 'b -> 'c) -> ('b 'a -> 'c))
(define (curry f) (lambda (x) (lambda (y) (f x y)))) ; (('a 'b -> c) -> ('a -> ('b -> 'c)))

;ZAD3 (?)
(curry compose) ; w curry: 'a = ('_a -> '_b), 'b = ('_c -> '_a), 'c = ('_c -> '_b)
                ; (curry compose) - (('_a -> '_b) -> (('_c -> '_a) -> ('_c -> '_b)))

((curry compose) (curry compose))
; w (curry compose): 'a = ?
; (('_a -> ('_b -> '_c)) -> ('_a -> (('_d -> '_b) -> ('_d -> '_c))))

((curry compose) (curry apply))
; w (curry apply): 'a = ('_a -> '_b), 'b = '_a, 'c - '_b
;(curry apply) - (('_a -> 'b) '_a -> '_b)
((curry apply) (curry compose))
(compose curry flip)
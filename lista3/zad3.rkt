#lang racket

((lambda (x y) (+ x (* x y))) 1 2)
;-> (+ 1 (* 1 2)) -> 3  /podstawienie x=1, y=2

((lambda (x) x) (lambda (x) x))
;-> (lambda (x) x) -> #<procedure>  /podstawienie x = (lambda (x) x)

((lambda (x) (x x)) (lambda (x) x)) ; ?
;-> /w (x x) za x podstawiamy (lambda ...)/ -> ((lambda (x) x) (lambda (x) x)) -> (lambda (x) x)

((lambda (x) (x x)) (lambda (x) (x x)))
;-> /jak wyzej/ -> ((lambda (x) (x x)) (lambda (x) (x x))) -> /za (x x) wstawimay (lambda ...)/ ->
;-> ((lambda (x) (x x)) ((lambda (x) (x x))) -> ... -> to samo - zapetla sie
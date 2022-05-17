#lang racket

#;(define (valid-field? board x y n)
  (match board
    ['() #t]
    [(cons p xs)
     (if (equal? p (cons x y))
         #f
         (and (<= 0 x) (<= 0 y) (< x n) (< y n) (valid-field? xs x y n)))]))

(define (valid-field? board x y n)
  (and (<= 0 x) (<= 0 y) (< x n) (< y n)
       (if (member (cons x y) board) #f #t)))

(define (select xs cont)
  (cond [(null? xs) #f]
        [else
         (match (cont (caar xs) (cdar xs))
           [#f (select (cdr xs) cont)]
           [xs xs])]))

(define (select-field i j cont)
  (select (list (cons (- i 2) (- j 1))
                (cons (- i 2) (+ j 1))
                (cons (- i 1) (- j 2))
                (cons (- i 1) (+ j 2))
                (cons (+ i 1) (- j 2))
                (cons (+ i 1) (+ j 2))
                (cons (+ i 2) (- j 1))
                (cons (+ i 2) (+ j 1)))
          cont))

(define (fail cont) #f)

(define (init-cont xs)
  (reverse xs))

(define (knight n)
  (define (knight-it board left i j cont)
    (if (= 0 left)
        (cont board)
        (select-field i j (lambda (x y)
                            (if (valid-field? board x y n)
                                (knight-it (cons (cons x y) board)
                                           (- left 1)
                                           x
                                           y
                                           cont)
                                (fail cont))))))
  (knight-it (list (cons 0 0)) (- (* n n) 1) 0 0 init-cont)) 
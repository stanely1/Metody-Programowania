#lang racket

(define (build-list n f)
  (define (pom m)
    (if (= m n)
        null
        (cons (f m) (pom (+ m 1)))))
  (pom 0))

(define (negatives n)
  (build-list n (lambda (x) (- 0 (+ x 1)))))

(define (reciprocals n)
  (build-list n (lambda (x) (/ 1 (+ x 1)))))

(define (evens n)
  (build-list n (lambda (x) (* 2 x))))

(define (identityM n)
  (define (id-matrix-row pos num)
    (if (= pos n)
        null
        (cons (if (= pos num) 1 0)
              (id-matrix-row (+ pos 1) num))))
  (build-list n (lambda (x) (id-matrix-row 0 x))))
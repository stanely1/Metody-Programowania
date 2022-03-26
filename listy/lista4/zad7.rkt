#lang racket

(define empty-queue  (cons null null))
(define (empty? q) (null? (car q)))

(define (push-back x q)
  (if (empty? q)
      (cons (list x) null)
      (cons (car q) (cons x (cdr q)))))

(define (front q)
  (caar q))

(define (pop q)
  (define tmp (cons (cdar q) (cdr q)))
  (if (null? (car tmp))
      (cons (reverse (cdr tmp)) null)
      tmp))

(define q (push-back 4 (push-back 3 (push-back 2 (push-back 1 empty-queue)))))
(define w (pop q))
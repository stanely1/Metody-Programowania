#lang racket

(define (elem? x xs)
  (if (equal? xs null) #f
      (if (equal? x (car xs)) #t
          (elem? x (cdr xs)))))
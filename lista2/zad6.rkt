#lang racket

(define (suffixes xs)
  (if (equal? xs null) (list null)
      (cons xs (suffixes (cdr xs)))))
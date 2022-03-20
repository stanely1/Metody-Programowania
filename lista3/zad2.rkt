#lang racket

(define (product xs)
  (foldl * 1 xs))
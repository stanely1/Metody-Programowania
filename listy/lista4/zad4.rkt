#lang racket

(provide flatten-fast)

(require "template_tree.rkt")

(define (list->left-tree xs)
  (foldl (lambda (x t) (node t x (leaf))) (leaf) xs))
(define test-tree (list->left-tree (build-list 20000 identity)))

(define (flat-append t xs)
  (cond [(leaf? t) xs]
        [(node? t) (flat-append (node-l t)
                                (cons (node-elem t)
                                      (flat-append (node-r t) xs)))]))

(define (flatten-fast t)
  (flat-append t null))
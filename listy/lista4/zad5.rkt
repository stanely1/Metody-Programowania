#lang racket

(require "template_tree.rkt")
(require "zad4.rkt")

(define (insert-bst x t)
  (cond [(leaf? t) (node (leaf) x (leaf))]
        [(node? t)
         (cond [(<= x (node-elem t))
                (node
                 (insert-bst x (node-l t))
                 (node-elem t)
                 (node-r t))]
               [else
                (node
                 (node-l t)
                 (node-elem t)
                 (insert-bst x (node-r t)))])]))

(define (treesort xs)
  (define (it xs bst)
    (if (null? xs)
        bst
        (it (cdr xs) (insert-bst (car xs) bst))))
  (flatten-fast (it xs (leaf))))
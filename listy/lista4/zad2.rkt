#lang racket

(require "template_tree.rkt")

(define (fold-tree f x t)
  (cond [(leaf? t) x]
        [(node? t) (f (fold-tree f x (node-l t))
                      (node-elem t)
                      (fold-tree f x (node-r t)))]))

;ZAD 2:
(define (tree-sum t)
  (fold-tree + 0 t))

(define (tree-flip t)
  (fold-tree (lambda (l e r) (node r e l)) (leaf) t))

(define (tree-height t)
  (fold-tree (lambda (l e r) (+ 1 (max l r))) 0 t))

(define (tree-span t)
  (cons (fold-tree (lambda (l e r) (min l e r)) +inf.0 t)
        (fold-tree (lambda (l e r) (max l e r)) -inf.0 t)))

(define (flatten t)
  (fold-tree (lambda (l e r) (append l (cons e r))) null t))
#lang racket

(require "template_tree.rkt")

(define (bst? t)
  (define-struct aux (min max is-bst))
  (define (check t)
    (cond [(leaf? t) (aux +inf.0 -inf.0 #t)]
          [(node? t)
           (let ([xl (check (node-l t))]
                 [xr (check (node-r t))])
             (aux (min (aux-min xl) (aux-min xr) (node-elem t))
                  (max (aux-max xl) (aux-max xr) (node-elem t))
                  (and (aux-is-bst xl)
                       (aux-is-bst xr)
                       (< (aux-max xl) (node-elem t))
                       (< (node-elem t) (aux-min xr)))))]))
  (aux-is-bst (check t)))

(define (sum-paths t)
  (define (it t val)
    (cond [(leaf? t) t]
          [(node? t)
           (node (it (node-l t) (+ val (node-elem t)))
                 (+ val (node-elem t))
                 (it (node-r t) (+ val (node-elem t))))]))
  (it t 0))
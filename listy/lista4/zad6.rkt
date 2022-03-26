#lang racket

(require "template_tree.rkt")
(require "zad4.rkt")

(define (delete x t)
  (define (pom w t)
    (cond [(leaf? t) w]
          [(node? t) (node (pom w (node-l t))
                           (node-elem t)
                           (node-r t))]))
  (cond [(leaf? t) (leaf)]
        [(node? t)
         (cond [(= x (node-elem t))
                (pom (node-l t) (node-r t))]
               [(< x (node-elem t))
                (node (delete x (node-l t))
                      (node-elem t)
                      (node-r t))]
               [else (node (node-l t)
                           (node-elem t)
                           (delete x (node-r t)))])]))
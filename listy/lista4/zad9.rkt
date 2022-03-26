#lang racket

(require "zad8.rkt")

(define empty-pq (hleaf))
(define (pq-empty? q) (hleaf? q))

(define (pq-insert e q)
  (heap-merge q (make-node e empty-pq empty-pq)))

(define (pq-pop q)
  (heap-merge (hnode-l q) (hnode-r q)))

(define (pq-min q)
  (hnode-elem q))

(define (pqsort xs)
  (define (ins-it xs ac)
    (if (null? xs)
        ac
        (ins-it (cdr xs)
            (pq-insert (ord (car xs) (car xs)) ac))))
  (define (out q)
    (if (pq-empty? q)
        null
        (cons (ord-val (pq-min q)) (out (pq-pop q)))))
  (out (ins-it xs empty-pq)))
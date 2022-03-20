#lang racket

(define (list->llist xs)
  (lambda (x) (if (null? x) xs (append xs x))))

(define (llist->list f)
  (f null))

(define llist-null (list->llist null))

(define (llist-singleton x)
  (list->llist (list x)))

(define (llist-append f g)
  (lambda (x) (f (g x))))

(define (foldr-llist-reverse xs)
  (llist->list (foldr (lambda (y ys)
           (llist-append ys (list->llist (list y))))
         llist-null xs)))

(define (foldr-reverse xs)
  (foldr (lambda (y ys) (append ys (list y))) null xs))

(length (foldr-reverse (build-list 20000 identity)))
(length (foldr-llist-reverse (build-list 20000 identity)))
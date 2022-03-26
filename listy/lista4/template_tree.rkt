#lang racket

(provide (struct-out leaf)
         (struct-out node)
         example-tree
         tree1)
 
(define-struct leaf () #:transparent)
(define-struct node (l elem r) #:transparent)

(define example-tree (node (node (leaf) 1 (leaf))
                           2
                           (node (node (leaf) 3 (leaf))
                                 4
                                 (node (leaf) 5 (leaf)))))
(define tree1
  (node
   (node (leaf) 2 (leaf))
   5
   (node (node (leaf) 6 (leaf))
         8
         (node (leaf) 9 (leaf)))))
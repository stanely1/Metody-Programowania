#lang plait

(define-type (Tree 'a)
  (leaf)
  (node [l : (Tree 'a)] [elem : 'a] [r : (Tree 'a)]))

(define example-tree
  (node (node (leaf) (pair 1 "foo") (leaf))
        (pair 2 "bar")
        (node (leaf) (pair 3 "baz") (leaf))))

(define example-bst
  (node (node
         (node (leaf) 1 (leaf))
         2
         (leaf))
        4
        (node (leaf) 5 (leaf))))

(define (process-tree fnode fleaf acl acr ac t)
  (if (leaf? t)
      (fleaf ac)
      (fnode ac
             (process-tree fnode fleaf acl acr (acl ac (node-elem t)) (node-l t))
             (node-elem t)
             (process-tree fnode fleaf acl acr (acr ac (node-elem t)) (node-r t)))))

(define (bst? t)
  (process-tree
   (lambda (a l v r) ; fnode
     (and l r (> v (fst a)) (< v (snd a))))
   (lambda (x) #t) ; fleaf
   (lambda (a v) ; acl
     (pair (fst a) v))
   (lambda (a v) ; acr
     (pair v (snd a)))
   (pair -inf.0 +inf.0) ; ac - (min, max)
   t))

(define (sum-paths t)
  (process-tree
   (lambda (a l v r)
     (node l (+ a v) r))
   (lambda (x) (leaf))
   (lambda (a v) (+ a v))
   (lambda (a v) (+ a v))
   0
   t))
#lang plait

(define-type (rose-tree 'a)
  (leaf [e : 'a])
  (node [lst : (Listof (rose-tree 'a))]))

(define example-rose-tree
  (node (list
         (node (list (leaf 1)))
         (node (list (leaf 2) (leaf 3)))
         (node (list (leaf 4) (leaf 5))))))

(define (print-rose-tree t)
  (local [(define (it ls)
            (if (empty? ls)
                empty
                (append (print-rose-tree (first ls)) (it (rest ls)))))]
    (if (leaf? t)
        (list (leaf-e t))
        (it (node-lst t)))))
#lang plait

;ZAD 1
(define-type (2-3-tree 'a)
  (leaf)
  (2-node [l : (2-3-tree 'a)] [elem : 'a] [r : (2-3-tree 'a)])
  (3-node [l : (2-3-tree 'a)] [a : 'a] [mid : (2-3-tree 'a)] [b : 'a] [r : (2-3-tree 'a)]))

(define (is-2-3-tree? t)
  (local [(define (val-check min max t)
            (type-case (2-3-tree 'a) t
              [(leaf) #t]
              [(2-node l elem r)
               (and (< min elem) (< elem max) (val-check min elem l) (val-check elem max r))]
              [(3-node l a mid b r)
               (and (< b a) (< min b) (< a max) (val-check min b l) (val-check b a mid) (val-check a max r))]))
          
          (define (height-check t) ; zwraca wysokosc jesli jest rowna dla kazdego poddrzewa lub -1 jesli nie
            (type-case (2-3-tree 'a) t
              [(leaf) 0]
              [(2-node l elem r)
               (let [(xl (height-check l))
                     (xr (height-check r))]
                 (if (or (= xl -1) (= xr -1) (not (= xl xr))) -1 (+ 1 xl)))]
              [(3-node l a mid b r)
               (let [(xl (height-check l))
                     (xm (height-check mid))
                     (xr (height-check r))]
                 (if (or (= xl -1) (= xm -1) (= xr -1) (not (= xl xm)) (not (= xl xr)) (not (= xm xr))) -1 (+ 1 xl)))]))]
    (and (val-check -inf.0 +inf.0 t) (not (= (height-check t) -1)))))

(define example-tree
  (3-node (2-node (leaf) 2 (leaf))
          6
          (2-node (leaf) 5 (leaf))
          3
          (3-node (leaf) 9 (leaf) 8 (leaf))))

;ZAD 2

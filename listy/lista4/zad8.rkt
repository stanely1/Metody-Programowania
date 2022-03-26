#lang racket

(provide (struct-out ord)
         (struct-out hleaf)
         (struct-out hnode)
         make-node
         heap-merge
         h g)

(define-struct ord (val priority) #:transparent)

(define-struct hleaf ())
(define-struct hnode (elem rank l r) #:transparent)

(define (make-node elem heap-a heap-b)
  (if (<= (rank heap-a) (rank heap-b))
      (hnode elem (+ 1 (rank heap-a)) heap-b heap-a)
      (hnode elem (+ 1 (rank heap-b)) heap-a heap-b)))

(define (hord? p h)
  (or (hleaf? h) (<= p (ord-priority (hnode-elem h)))))

(define (rank h)
  (if (hleaf? h) 0 (hnode-rank h)))

(define (heap? h)
  (or (hleaf? h)
      (and (hnode? h)
           (heap? (hnode-l h))
           (heap? (hnode-r h))
           (<= (rank (hnode-r h)) (rank (hnode-l h)))
           (= (rank h) (+ 1 (rank (hnode-r h))))
           (hord? (ord-priority (hnode-elem h)) (hnode-l h))
           (hord? (ord-priority (hnode-elem h)) (hnode-r h)))))

(define h (make-node (ord "d" 1)
                     (make-node (ord "xd" 4) (hleaf) (hleaf))
                     (make-node (ord "xdd" 3) (hleaf) (hleaf))))
(define g (make-node (ord "j" 2)
                     (make-node (ord "jd" 5) (hleaf) (hleaf))
                     (hleaf)))

(define (heap-merge h1 h2)
  (cond [(hleaf? h1) h2]
        [(hleaf? h2) h1]
        [else (let ([e1 (hnode-elem h1)]
                    [e2 (hnode-elem h2)])
                (cond [(< (ord-priority e1) (ord-priority e2))
                       (make-node e1
                                  (hnode-l h1)
                                  (heap-merge (hnode-r h1) h2))]
                      [else
                       (make-node e2
                                  (hnode-l h2)
                                  (heap-merge (hnode-r h2) h1))]))]))
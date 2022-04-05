#lang plait

(define (del x xs)
  (cond [(empty? xs) empty]
        [(equal? x (first xs)) (rest xs)]
        [else (cons (first xs) (del x (rest xs)))]))

(define (perms xs)
  (local [(define (aux ys ls) ; ys - wejsciowa ls -pozostale
            (cond [(empty? ys) empty]
                  [(empty? (rest ls)) (list ls)]
                  [else (append (map
                                 (lambda (x) (cons (first ys) x))
                                 (perms (del (first ys) ls)))
                                (aux (rest ys) xs))]))]
    (if (empty? xs) (list xs) (aux xs xs))))
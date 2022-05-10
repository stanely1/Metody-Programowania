#lang racket

(define (list->mlist xs)
  (if (null? xs)
      null
      (mcons (car xs) (list->mlist (cdr xs)))))

(define example (list->mlist '(1 2 3 4 5 6)))
(define l0 null)
(define l1 (mcons 'x null))
(define l2 (mcons 1 (mcons 2 null)))

(define (mlength xs)
  (define (it xs ac)
    (if (null? xs)
        ac
        (it (mcdr xs) (+ 1 ac))))
  (it xs 0))

(define (mreverse! xs)
  (define last xs)
  (define snd xs)
  (define len (mlength xs))
    
  (define (it ys prev)
    (if (null? (mcdr ys))
        (begin
          (set! last ys)
          (set-mcdr! xs prev))
        (begin
          (it (mcdr ys) ys)
          (set-mcdr! ys
                     (if (eq? ys snd)
                         last
                         prev)))))
  (define (swap-mcar! x y)
    (define tmp (mcar x))
    (set-mcar! x (mcar y))
    (set-mcar! y tmp))
  
  (cond [(<= len 1) (void)]
        [(= len 2) (swap-mcar! xs (mcdr xs))]
        [else (begin
                (set! snd (mcdr xs))
                (it (mcdr xs) last)
                (swap-mcar! xs last))]))
      

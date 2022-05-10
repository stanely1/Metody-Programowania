#lang racket

(provide
 dequeue?
 empty-dequeue
 nonempty-dequeue/c
 (contract-out
  [dequeue-empty?     (-> dequeue? boolean?)]
  [dequeue-push-front (-> dequeue? any/c any/c)]
  [dequeue-push-back  (-> dequeue? any/c any/c)]
  [dequeue-pop-front  (-> nonempty-dequeue/c any/c)]
  [dequeue-pop-back   (-> nonempty-dequeue/c any/c)]))

(struct dequeue-node ([prev #:mutable] [elem #:mutable] [next #:mutable]) #;#:transparent)
(struct dequeue ([front #:mutable] [back #:mutable]) #;#:transparent)

(define empty-dequeue (dequeue null null))

(define (dequeue-empty? q)
  (null? (dequeue-front q)))

(define nonempty-dequeue/c
  (and/c dequeue? (not/c dequeue-empty?)))

(define (dequeue-push-front q x)
  (define new-elem (dequeue-node null x (dequeue-front q)))
  (if (dequeue-empty? q)
      (set-dequeue-back! q new-elem) ; nowy element jest ostatnim
      (set-dequeue-node-prev! (dequeue-front q) new-elem)) ; nowy jest poprzednikiem pierwszego)
  (set-dequeue-front! q new-elem)) ; nowy staje sie pierwszym

(define (dequeue-push-back q x)
  (define new-elem (dequeue-node (dequeue-back q) x null))
  (if (dequeue-empty? q)
      (set-dequeue-front! q new-elem) ; nowy element jest pierwszym
      (set-dequeue-node-next! (dequeue-back q) new-elem)) ; nowy jest nastepnikiem ostatniego
  (set-dequeue-back! q new-elem)) ; nowy staje sie ostatnim

(define/contract (dequeue-pop-front q)
  (-> nonempty-dequeue/c any/c)
  (define pop-elem (dequeue-front q))
  (if (eq? pop-elem (dequeue-back q)) ; jesli jest ostatnim
      (set-dequeue-back! q null) ; usuwamy ostatni element
      (set-dequeue-node-prev! (dequeue-node-next pop-elem) null)) ; usuwamy poprzednik nastepnego elementu
  (set-dequeue-front! q (dequeue-node-next pop-elem)) ; nastepny staje sie pierwszym
  (dequeue-node-elem pop-elem))

(define/contract (dequeue-pop-back q)
  (-> nonempty-dequeue/c any/c)
  (define pop-elem (dequeue-back q))
  (if (eq? pop-elem (dequeue-front q)) ; jesli jest pierwszym
      (set-dequeue-front! q null) ; usuwamy pierwszy element
      (set-dequeue-node-next! (dequeue-node-prev pop-elem) null)) ; ususwany nastepnik poprzedniego elementu
  (set-dequeue-back! q (dequeue-node-prev pop-elem)) ; poprzedni staje sie ostatnim
  (dequeue-node-elem pop-elem))
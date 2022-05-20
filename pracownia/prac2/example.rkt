#lang racket

(struct wire ([value #:mutable] [actions #:mutable]))

(define (call-actions xs)
  (if (null? xs)
      (void)
      (begin
        ((car xs))
        (call-actions (cdr xs)))))

(define (set-value! w v)
  (if (eq? v (wire-value w))
      (void)
      (begin
        (set-wire-value! w v)
        (call-actions (wire-actions w)))))

(define (add-action! w f)
  (set-wire-actions! w (cons f (wire-actions w))))

(define (nand-gate a b c)
  (define (and-action)
    (set-value! c (not (and (wire-value a) (wire-value b)))))
  (add-action! a and-action)
  (add-action! b and-action))

(define (make-wire)
  (wire #f null))

(define (xor-gate a b c)
  (define d (make-wire))
  (define e (make-wire))
  (define f (make-wire))
  (nand-gate a b d)
  (nand-gate a d e)
  (nand-gate b d f)
  (nand-gate e f c))

(define (probe name w)
  (add-action! w (lambda ()
                   (display name)
                   (display " = ")
                   (display (wire-value w))
                   (display "\n"))))
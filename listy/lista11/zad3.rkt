#lang plait

(require "let-subst.rkt")

(define (remove-duplicates xs)
  (type-case (Listof 'a) xs
    [empty empty]
    [(cons x xs)
      (if (member x xs)
         (remove-duplicates xs)
         (cons x (remove-duplicates xs)))]))

(define (free-vars [e : Exp]) : (Listof Symbol)
  (local [(define (ls e)
            (type-case Exp e
              [(opE o l r) (append (ls l) (ls r))]
              [(ifE b l r) (append (ls b)
                                   (append (ls l) (ls r)))]
              [(varE v) (list v)]
              [(letE x e1 e2)
               (free-vars (subst e2 x e1))]
              [else empty]))]
    (remove-duplicates (ls e))))


(module+ test
  (print-only-errors #t))

(module+ test
  (test (free-vars
         (parse `(if (<= x y)
                     (+ x
                        (let x 1 (* x y)))
                     (+ z x))))
        '(y z x))
  (test (free-vars (parse
                    `(let x 1
                       (let y (+ x 1)
                         (+ x y)))))
        '())
  (test (free-vars (parse
                    `(if (<= 1 2)
                         (let x (+ 1 3) (+ x 2))
                         (+ x 2))))
        '(x)))
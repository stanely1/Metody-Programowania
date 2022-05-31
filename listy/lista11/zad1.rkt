#lang plait

(module+ test
  (print-only-errors #t))

(define-syntax my-and
  (syntax-rules ()
    [(my-and) #t]
    [(my-and a b ...)
     (if a (my-and b ...) #f)]))

(define-syntax my-or
  (syntax-rules ()
    [(my-or) #f]
    [(my-or a b ...)
     (if a #t (my-or b ...))]))

(define-syntax my-let
  (syntax-rules ()
    [(my-let () a) a]
    [(my-let ([x1 a1] [x2 a2] ...) a)
     #;(my-let ([x2 a2] ...) ((lambda (x1) a) a1))
     ((lambda (x1 x2 ...) a) a1 a2 ...)]))

(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () a) a]
    [(my-let* ([x1 a1] [x2 a2] ...) a)
       ((lambda (x1) (my-let* ([x2 a2] ...) a)) a1)]))

;; tests ---------------------------------------------

(module+ test
  (test (my-let* ([x 1]
                  [y (+ x 1)]
                  [z 3])
                 (+ (* y z) x))
        (let* ([x 1]
               [y (+ x 1)]
               [z 3])
          (+ (* y z) x)))
  (test (my-let* ([x 2]
                  [y (+ 1 x)])
                 (+ x (my-let* ([x 5])
                               (* x y))))
        (let* ([x 2]
               [y (+ 1 x)])
          (+ x (let* ([x 5])
                 (* x y)))))
  (test (my-let* ([x 1]
                  [x 2]) x)
        (let* ([x 1]
               [x 2]) x))
  (test (my-and #t #f) (and #t #f))
  (test (my-or #t #f) (or #t #f))
  (test (my-let ([x 1]
                 [y 2]
                 [z 3])
                (* z (+ x y)))
        (let ([x 1]
              [y 2]
              [z 3])
          (* z (+ x y))))
  (test (my-let ([x 1])
                 (+ x
                    (my-let ([x 2])
                            (+ x 3))))
        (let ([x 1])
              (+ x
                 (let ([x 2])
                   (+ x 3))))))
                         
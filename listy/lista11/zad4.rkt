#lang plait

(require "let-subst.rkt")
(require "zad3.rkt")

(define used-names empty)
(define counter 0)

(define (new-var-name [x : Symbol]) : Symbol
  (let ([y (string->symbol
            (string-append
             (symbol->string x) (to-string counter)))])
    (if (member y used-names)
        (begin
          (set! counter (+ 1 counter))
          (new-var-name x))
        (begin
          (set! used-names (cons y used-names))
          y))))

(define (subst2-aux [e : Exp] [x : Symbol] [a : Exp]) : Exp
  (type-case Exp e
    [(opE o l r) (opE o (subst2-aux l x a) (subst2-aux r x a))]
    [(ifE b l r) (ifE (subst2-aux b x a) (subst2-aux l x a) (subst2-aux r x a))]
    [(varE v)
     (if (eq? x v) a (varE v))]
    [(letE y e1 e2)
     (let* ([e1-new (subst2-aux e1 x a)]
            [y-new  (new-var-name y)]
            [e2-new (if (eq? x y)
                        (subst2-aux e2 y (varE y-new)) ;; podstawiamy za y nowa nazwe zmiennej
                        (subst2-aux (subst2-aux e2 y (varE y-new)) x a))])
       (letE y-new e1-new e2-new))]
    [else e]))

(define (subst2 [e : Exp] [x : Symbol] [a : Exp]) : Exp
  (begin
    (set! used-names (free-vars a))
    (set! counter 0)
    (subst2-aux e x a)))

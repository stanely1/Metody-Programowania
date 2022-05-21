#lang plait

(require "arith-vm.rkt")

(module+ test
  (print-only-errors #t))

;; stos - pomocnicza struktura
(define-type-alias StackExp (Listof Exp))

(define (stack-exp-push [s : StackExp] [e : Exp]) : StackExp
  (cons e s))

(define (stack-exp-pop [s : StackExp]) : (Exp * StackExp)
  (type-case StackExp s
    [empty (error 'pop "Empty stack")]
    [(cons e s) (pair e s)]))

;; decompile
(define (decompile-with-stack [code : Code] [s : StackExp]) : Exp
  (type-case Code code
    [empty (fst (stack-exp-pop s))]
    [(cons i cs)
     (type-case Instr i
       [(pushI n)
        (decompile-with-stack cs (stack-exp-push s (numE n)))]
       [(opI op)
        (let* ([e1-s1 (stack-exp-pop s)]
               [e1    (fst e1-s1)]
               [s1    (snd e1-s1)]
               [e2-s2 (stack-exp-pop s1)]
               [e2    (fst e2-s2)]
               [s2    (snd e2-s2)]
               [s0    (stack-exp-push s2 (opE op e2 e1))])
          (decompile-with-stack cs s0))])]))

(define (decompile [code : Code]) : Exp
 (decompile-with-stack code empty))

(define e1 (parse `(+ 1 (* (+ 3 2) (- 7 6)))))
(define e2 (parse `(/ (* 1 (+ 9 7)) (- (- 8 7) (+ (* 2 3) 1)))))
(define e3 (parse `(+ (- (/ (* 9 5) 8) 7) 6)))
(define e4 (parse `(+ 1 (+ 2 (+ 3 (- 4 5))))))

(module+ test
  (test (decompile (compile e1)) e1)
  (test (decompile (compile e2)) e2)
  (test (decompile (compile e3)) e3)
  (test (decompile (compile e4)) e4))
       
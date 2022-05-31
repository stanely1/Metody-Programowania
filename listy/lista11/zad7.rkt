#lang plait

(require "let-lex-addr.rkt")

(define counter 0)
(define (new-var-name)
  (begin
    (set! counter (+ 1 counter))
    (string->symbol (string-append "x" (to-string counter)))))

(define (addr->named [e : ExpA] [env : (EnvA Symbol)]) : Exp
  (type-case ExpA e
    [(numA n) (numE n)]
    [(opA o l r) (opE o (addr->named l env) (addr->named r env))]
    [(ifA b l r) (ifE (addr->named b env) (addr->named l env) (addr->named r env))]
    [(varA n) (varE (lookup-envA n env))]
    [(letA e1 e2)
     (let ([x (new-var-name)])
       (letE x
             (addr->named e1 env)
             (addr->named e2 (extend-envA env x))))]))

(define (translate-addr->named [e : ExpA]) : Exp
  (begin
    (set! counter 0)
    (addr->named e mt-envA)))
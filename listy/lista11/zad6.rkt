#lang plait

(require "let-lex-addr.rkt")

(define e (parse
           `{let x 3
              {let y 4
                {+ {let x {+ y 5}
                     {* x y}}
                   x}}}))
(define a
  (translate e mt-envA))

e
a
(evalA a mt-envA)

#lang racket

(require "parsing.rkt")
(require "syntax.rkt")

; ====================================================
; Gramatyki bezkontekstowe

; * Terminale (tokeny)
; * Nieterminale (np. "expression", "operator")
; * Lista produkcji:
;   "expression" -> NUMBER
;   "expression" -> "expression" "operator" "expression"

; 2 + 2 + 2

; "expression" -> "expression" "add-operator" "mult-exp"
; "expression" -> "mult-exp"
; "mult-exp"   -> "mult-exp" "mult-operator" "atom-exp"
; "mult-exp"   -> "atom-exp"
; "atom-exp"   -> NUMBER
; "atom-exp"   -> ( "expression" )

; ====================================================

(define grammar
  `(("operator"
     ((+) ,op-add)
     ((-) ,op-sub)
     ((*) ,op-mul)
     ((/) ,op-div))
    
    ("expression"
     (("simple-expr" "operator" "simple-expr")
          ,(lambda (e1 op e2) (exp-op op e1 e2)))
     (("simple-expr") ,(lambda (e) e)))
    
    ("simple-expr"
     ((NUMBER)           ,exp-number)
     (( ("expression") ) ,(lambda (e) e)))))

(define (run-exp-parser se)
  (run-named-parser grammar "expression" (list se)))
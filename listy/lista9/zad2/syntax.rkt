#lang plait

(define-type Exp
  (exp-var    [name : Symbol])
  (exp-number [n : Number])
  (exp-lambda [params : (Listof Exp)] [app : Exp])
  (exp-app    [f : Exp] [params : (Listof Exp)])
  (exp-if     [cond : Exp] [then : Exp] [else : Exp])
  (exp-let    [def : (Listof (Exp * Exp))] [body : Exp])
  (exp-cond   [cases : (Listof (Exp * Exp))]))
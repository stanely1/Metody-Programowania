#lang plait

(define-type Op-bin
  (op-add) (op-sub) (op-mul) (op-div) (op-pow))

(define-type Op-un-pref
  (op-neg)) ; - exp

(define-type Op-un-suf
  (op-fact)) ; exp !

(define-type Exp
  (exp-number [n : Number])
  (exp-op-bin [op : Op-bin] [e1 : Exp] [e2 : Exp])
  (exp-op-un-pref [op : Op-un-pref] [e : Exp])
  (exp-op-un-suf  [e : Exp]  [op : Op-un-suf]))

(define (s-exp->op-bin se)
  (if (s-exp-symbol? se)
      (let ([sym (s-exp->symbol se)])
        (cond
          [(symbol=? sym '+) (op-add)]
          [(symbol=? sym '-) (op-sub)]
          [(symbol=? sym '*) (op-mul)]
          [(symbol=? sym '/) (op-div)]
          [(symbol=? sym '^) (op-pow)]))
      (error 's-exp->op-bin "Syntax error")))

(define (s-exp->op-un-pref se)
  (if (s-exp-symbol? se)
      (let ([sym (s-exp->symbol se)])
        (cond
          [(symbol=? sym '-) (op-neg)]))
      (error 's-exp->op-un "Syntax error")))

(define (s-exp->op-un-suf se)
  (if (s-exp-symbol? se)
      (let ([sym (s-exp->symbol se)])
        (cond
          [(symbol=? sym '!) (op-fact)]))
      (error 's-exp->op-un "Syntax error")))

(define (s-exp->exp se)
  (cond
    [(s-exp-number? se) (exp-number (s-exp->number se))]
    [(s-exp-match? `(SYMBOL ANY ANY) se) ; operator binarny
     (let ([se-list (s-exp->list se)])
        (exp-op-bin (s-exp->op-bin (first se-list))
                    (s-exp->exp (second se-list))
                    (s-exp->exp (third se-list))))]
    [(s-exp-match? `(SYMBOL ANY) se) ; unarny prefixowy
     (let ([se-list (s-exp->list se)])
        (exp-op-un-pref (s-exp->op-un-pref (first se-list))
                        (s-exp->exp (second se-list))))]
    [(s-exp-match? `(ANY SYMBOL) se) ; unarny sufixowy
     (let ([se-list (s-exp->list se)])
       (exp-op-un-suf (s-exp->exp (second se-list))
                      (s-exp->op-un-suf (first se-list))))]
    [else (error 's-exp->exp "Syntax error")]))
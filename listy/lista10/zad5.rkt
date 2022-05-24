#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add) (sub) (mul) (div) (eql) (leq))

(define-type List-exp
  (cons-exp [l : Exp] [r : Exp])
  (car-exp  [ls : Exp])
  (cdr-exp  [ls : Exp])
  (null?-exp [ls : Exp])
  (null-exp)
  (list-exp [elems : (Listof Exp)]))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op] [l : Exp] [r : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (condE [cs : (Listof (Exp * Exp))])
  (listE [ls : List-exp]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{cond ANY ...} s)
     (condE (parse-cond (rest (s-exp->list s))))]
    ;; parsowanie list
    [(s-exp-match? `{cons ANY ANY} s)
     (listE (cons-exp
             (parse (second (s-exp->list s)))
             (parse (third  (s-exp->list s)))))]
    [(s-exp-match? `{car ANY} s)
     (listE (car-exp
             (parse (second (s-exp->list s)))))]
    [(s-exp-match? `{cdr ANY} s)
     (listE (cdr-exp
             (parse (second (s-exp->list s)))))]
    [(s-exp-match? `{null? ANY} s)
     (listE (null?-exp
             (parse (second (s-exp->list s)))))]
    [(s-exp-match? `null s)
     (listE (null-exp))]
    [(s-exp-match? `{list ANY ...} s)
     (listE (list-exp
             (parse-list (rest (s-exp->list s)))))]
     ;;;
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

;; parsowanie listy (list x1 x2 ...)
(define (parse-list [xs : (Listof S-Exp)]) : (Listof Exp)
  (map parse xs))

(define (parse-cond [ss : (Listof S-Exp)]) : (Listof (Exp * Exp))
  (type-case (Listof S-Exp) ss
    [empty
     empty]
    [(cons s ss)
     (if (s-exp-match? `{ANY ANY} s)
         (cons (pair (parse (first (s-exp->list s)))
                     (parse (second (s-exp->list s))))
               (parse-cond ss))
         (error 'parse "invalid input: cond"))]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))
  
;; eval --------------------------------------

(define-type Value
  (numV  [n : Number])
  (boolV [b : Boolean])
  (listV [ls : (Listof Value)])) ;; nowy rodzaj wartosci

(define (op-num-num->proc [f : (Number Number -> Number)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (numV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op-num-bool->proc [f : (Number Number -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (boolV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(div) (op-num-num->proc /)]
    [(eql) (op-num-bool->proc =)]
    [(leq) (op-num-bool->proc <=)]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(opE o l r) ((op->proc o) (eval l) (eval r))]
    [(ifE b l r)
     (type-case Value (eval b)
       [(boolV v)
        (if v (eval l) (eval r))]
       [else
        (error 'eval "type error")])]
    [(condE cs)
     (eval (cond->if cs))]
    ;; lista
    [(listE ls)
     (type-case List-exp ls ;; ma zwrocic Value
       [(cons-exp l r)
        (type-case Value (eval r) ;; drugi argument consa ma byc listą
          [(listV ls)
           (listV (cons (eval l) ls))]
          [else (error 'eval "type error")])]
       [(car-exp ls)
        (type-case Value (eval ls) ;; argument car ma byc lista
          [(listV ls)
           (first ls)]
          [else (error 'eval "type error")])]
       [(cdr-exp ls)
        (type-case Value (eval ls) ;; argument cdr ma byc lista
          [(listV ls)
           (listV (rest ls))]
           [else (error 'eval "type error")])]
        [(null?-exp ls)
         (type-case Value (eval ls) ;; argument null? ma byc lista
           [(listV ls)
            (boolV (empty? ls))]
           [else (error 'eval "type error")])]
        [(null-exp)
         (listV empty)]
        [(list-exp ls)
         (listV (eval-list ls))])]))

(define (eval-list [ls : (Listof Exp)]) : (Listof Value)
  (map eval ls))
        

(define (cond->if [cs : (Listof (Exp * Exp))]) : Exp
  (type-case (Listof (Exp * Exp)) cs
    [empty
     (numE 42)]
    [(cons c cs)
     (ifE (fst c)
          (snd c )
          (cond->if cs))]))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

(module+ test
  (test (run `(cons 1 (cons 2 null)))
        (listV (list (numV 1) (numV 2))))
  (test (run `(null? (list 1 2 (cons 4 null))))
        (boolV #f))
  (test (run `(cdr (list 1 2 3 4)))
        (listV (list (numV 2) (numV 3) (numV 4))))
  (test (run `(+ 2 (car (list 7 8 9))))
        (numV 9))
  (test/exn (run `(cons 1 2))
        "type error"))

;; printer ———————————————————————————————————-

(define (ListV->Listof-Symbol [ls : (Listof Value)]) : (Listof Symbol)
  (type-case (Listof Value) ls
    [empty empty]
    [(cons v ls)
     (cons (string->symbol (value->string v)) (ListV->Listof-Symbol ls))]))

(define (listV->string [ls : (Listof Value)]) : String
  (to-string (ListV->Listof-Symbol ls)))

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]
    [(listV ls) (listV->string ls)]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))

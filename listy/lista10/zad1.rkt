#lang plait

(require (typed-in racket
                   (apply : (('a 'a -> 'a) (Listof 'a) -> 'a))))

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add)
  (sub)
  (mul)
  (div))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op]
       [args : (Listof Exp)])) ;; lista argumentow

;; parse ----------------------------------------

(module+ test
  (test (parse `12)
        (numE 12))
  (test (parse `{+ 2 1})
        (opE (add) (list (numE 2) (numE 1))))
  (test (parse `{* 1 2 3})
        (opE (mul) (list (numE 1) (numE 2) (numE 3))))
  (test (parse `{+ 1 2 (* (- 1) 2 (/ 3 4 5))})
        (opE (add) (list (numE 1)
                         (numE 2)
                         (opE (mul) (list
                                     (opE (sub) (list (numE 1)))
                                     (numE 2)
                                     (opE (div)
                                          (list (numE 3)
                                                (numE 4)
                                                (numE 5)))))))))
  
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{SYMBOL ANY ...} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s)))) ; parsujemy operator
          (parse-op-args (rest (s-exp->list s))))]           ; i liste argumentow
    [else (error 'parse "invalid input")]))

; parsowanie listy argumentow
(define (parse-op-args [args : (Listof S-Exp)]) : (Listof Exp)
  (map parse args))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [else (error 'parse "unknown operator")]))
  
;; eval --------------------------------------

(define-type-alias Value Number)

(define (op->proc [op : Op]) : ((Listof Value) -> Value) ; nowe funkcje - dla list
  (type-case Op op
    [(add) (lambda (xs) (apply + xs))]
    [(sub) (lambda (xs) (apply - xs))]
    [(mul) (lambda (xs) (apply * xs))]
    [(div) (lambda (xs) (apply / xs))]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) n]
    [(opE op xs) ((op->proc op) (eval-op-args xs))]))

; evaluacja listy argumentow
(define (eval-op-args xs) : (Listof Value)
  (map eval xs))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

(module+ test
  (test (run `(+ 1 2 3)) 6)
  (test (run `(- 1 2 3 4)) -8)
  (test (run `(- 1 2 3 4 (+ 3 4 5))) -20)
  (test (run `(+ (- 1 2))) -1)
  (test (run `(+)) 0)
  (test (run `(/ 1 2 3)) 1/6)
  (test (run `(* 2)) 2))

;; printer ———————————————————————————————————-

(define (print-value [v : Value]) : Void
  (display v))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))
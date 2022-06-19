#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Exp
  (numE [n : Number])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (varE [x : Symbol])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (lamE [x : Symbol] [e : Exp])
  (appE [e1 : Exp] [e2 : Exp])
  (beginE [e1 : Exp] [e2 : Exp])
  #;(setE [x : Symbol] [e : Exp])
  (boxE [e : Exp])
  (unboxE [e : Exp])
  (set-box!E [b : Exp] [e : Exp]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{box ANY} s) ;; parse box expressions
     (boxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{unbox ANY} s)
     (unboxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{set-box! ANY ANY} s)
     (set-box!E (parse (second (s-exp->list s)))
                (parse (third  (s-exp->list s))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol
            (first (s-exp->list 
                    (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [(s-exp-match? `{let SYMBOL ANY ANY} s)
     (letE (s-exp->symbol (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{begin ANY ANY} s)
     (beginE(parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    #;[(s-exp-match? `{set! SYMBOL ANY} s)
     (setE (s-exp->symbol (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (appE (appE (varE (parse-op (s-exp->symbol (first (s-exp->list s)))))
                 (parse (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define prim-ops '(+ - * / = <=))

(define (parse-op [op : Symbol]) : Symbol
  (if (member op prim-ops)
      op 
      (error 'parse "unknown operator")))

;; eval --------------------------------------

;; values

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (funV [x : Symbol] [e : Exp] [env : Env])
  (primopV [f : (Value -> Value)])
  (voidV)
  (boxV [ref : Location])) ;; nowy typ value

;; storage (heap)

(define-type Storable
  (valS [v : Value])
  (undefS))

(define-type-alias Storage (Listof Storable))
(define-type-alias Location Number)

(define mt-sto : Storage empty)
(define (alloc-sto [sto : Storage] [u : Storable]) : (Location * Storage)
  (let ([next-free (length sto)])
    (pair next-free
          (append sto (list u)))))
(define (deref-sto [sto : Storage] [l : Location]) : Value
  (if (<= l (- (length sto) 1))
      (type-case Storable (list-ref sto l)
        [(valS v) v]
        [(undefS) (error 'lookup-env "undefined object")])
      (error 'deref "unknown location")))
(define (update-sto [sto : Storage] [l : Location] [u : Storable]) : Storage
  (local
    ((define (walk sto l)
       (if (= l 0)
           (cons u (rest sto))
           (cons (first sto)
                 (walk (rest sto) (- l 1))))))
    (if (<= l (- (length sto) 1))
        (walk sto l)
        (error 'deref "unknown location"))))

;; environments

(define-type Binding
  (bind [name : Symbol]
        [val : Value])) ;; powrot do value

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define (extend-env [env : Env] [x : Symbol] [v : Value]) : Env
  (cons (bind x v) env))
(define (lookup-env [env : Env] [x : Symbol]) : Value
  (type-case Env env
    [empty
     (error 'lookup-env "unbound variable")]
    [(cons b rst-env)
     (cond
       [(eq? x (bind-name b))
        (bind-val b)]
       [else
        (lookup-env rst-env x)])]))
  
;; primitive operations

(define (op-num-num->value [f : (Number Number -> Number)]) : Value 
  (primopV
   (λ (v1)
     (type-case Value v1
       [(numV n1)
        (primopV
         (λ (v2)
           (type-case Value v2
             [(numV n2)
              (numV (f n1 n2))]
             [else
              (error 'eval "type error")])))]
       [else
        (error 'eval "type error")]))))

(define (op-num-bool->value [f : (Number Number -> Boolean)]) : Value 
  (primopV
   (λ (v1)
     (type-case Value v1
       [(numV n1)
        (primopV
         (λ (v2)
           (type-case Value v2
             [(numV n2)
              (boolV (f n1 n2))]
             [else
              (error 'eval "type error")])))]
       [else
        (error 'eval "type error")]))))

(define (prim-op->value op)
  (cond [(eq? op '+) (op-num-num->value +)]
        [(eq? op '-) (op-num-num->value -)]
        [(eq? op '*) (op-num-num->value *)]
        [(eq? op '/) (op-num-num->value /)]
        [(eq? op '=) (op-num-bool->value =)]
        [(eq? op '<=) (op-num-bool->value <=)]))
 
(define init-env
  (foldl (λ (op env)
           (extend-env env op (prim-op->value op)))
         mt-env
         prim-ops))

;; evaluation function (eval/apply)

(define-type Answer
  (v*s [v : Value] [s : Storage]))

(define-syntax-rule
  (with [(val sto) call]
    body)
  (type-case Answer call
    [(v*s val sto) body]))

(define (eval [e : Exp] [env : Env] [sto : Storage]) : Answer
  (type-case Exp e
    [(numE n)
     (v*s (numV n) sto)]
    [(ifE b l r)
     (with [(v sto0) (eval b env sto)]
           (type-case Value v
             [(boolV v0)
              (if v0 (eval l env sto0) (eval r env sto0))]
             [else
              (error 'eval "type error")]))]
    [(varE x)
     (v*s (lookup-env env x) sto)]
    [(letE x e1 e2)
     (with [(v1 sto1) (eval e1 env sto)]
           (eval e2 (extend-env env x v1) sto1))]
    [(lamE x b)
     (v*s (funV x b env) sto)]
    [(appE e1 e2)
     (with [(v1 sto1) (eval e1 env sto)]
       (with [(v2 sto2) (eval e2 env sto1)]
         (apply v1 v2 sto2)))]
    [(beginE e1 e2)
     (with [(v1 sto1) (eval e1 env sto)]
       (eval e2 env sto1))]
    #;[(setE x e0)
     (with [(v0 sto0) (eval e0 env sto)]
       (let ([sto1 (update-sto sto0 (lookup-env env x) (valS v0))])
         (v*s (voidV) sto1)))]
    [(boxE e)
     (with [(v sto1) (eval e env sto)]
           (let ([l*s (alloc-sto sto1 (valS v))])
             (v*s (boxV (fst l*s)) (snd l*s))))]
    [(unboxE e)
     (with [(v sto1) (eval e env sto)]
           (type-case Value v
             [(boxV r) (v*s (deref-sto sto1 r) sto1)]
             [else (error 'unbox "not a box")]))]
    [(set-box!E b e)
     (with [(v1 sto1) (eval b env sto)]
           (type-case Value v1
             [(boxV r)
              (with [(v2 sto2) (eval e env sto1)]
                    (let ([sto3 (update-sto sto2 r (valS v2))])
                      (v*s (voidV) sto3)))]
             [else (error 'set-box! "not a box")]))]))

(define (apply [v1 : Value] [v2 : Value] [sto : Storage]) : Answer
  (type-case Value v1
    [(funV x e env)
     (eval e (extend-env env x v2) sto)]
    [(primopV f)
     (v*s (f v2) sto)]
    [else (error 'apply "not a function")]))

(define (run [e : S-Exp]) : Value
  (with [(v sto)
         (eval (parse e) init-env mt-sto)]
    v))

;;test
(module+ test
  (test (run `{let x {box {+ 1 2}}
                {begin {set-box! x {* 2 3}}
                       {unbox x}}})
        (numV 6))
  (test (run `{let x {lambda {x} {box x}}
                {unbox {x 10}}})
        (numV 10))
  (test (run `{let x {lambda {x} {box x}}
                {let y {x 10}
                  {begin {set-box! y 12}
                         {unbox y}}}})
        (numV 12))
  (test (run `{let x {lambda {x} {box x}}
                {let y {x 10}
                  {begin {set-box! y 12}
                         {unbox {x 10}}}}})
        (numV 10)))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]
    [(funV x e env) "#<procedure>"]
    [(primopV f) "#<primop>"]
    [(voidV) ""]
    [(boxV r) "#<box>"]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (run e)))

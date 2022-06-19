#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add) (sub) (mul) (div) (eql) (leq))

(define-type Exp
  (numE [n : Number])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (varE [x : Symbol])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (lamE [x : Symbol] [e : Exp])
  (appE [e1 : Exp] [e2 : Exp])
  (letrecE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (symE [x : Symbol]) ;; nowy typ exp
  (listE [ls : (Listof Exp)]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `'ANY s) ;; parse quote exp
     (parse-quote (second (s-exp->list s)))]
    [(s-exp-match? `{list ANY ...} s)
     (listE (map parse (rest (s-exp->list s))))] ;; parse list
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
    [(s-exp-match? `{letrec SYMBOL ANY ANY} s)
     (letrecE (s-exp->symbol (second (s-exp->list s)))
              (parse (third (s-exp->list s)))
              (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (appE (appE (varE (parse-op (s-exp->symbol (first (s-exp->list s)))))
                 (parse (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-quote [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (symE (s-exp->symbol s))]
    [(s-exp-match? `{ANY ...} s)
     (listE (map parse-quote (s-exp->list s)))]))
  

(define prim-ops '(+ - * / = <= symbol=? cons)) ;; symbol=? i cons będą primop

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
  (symV [x : Symbol]) ;; symbol value
  (listV [ls : (Listof Value)])) ;; list value

;; environments

(define-type Storable
  (valS [v : Value])
  (undefS))

(define-type Binding
  (bind [name : Symbol]
        [ref : (Boxof Storable)]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)

(define (extend-env-undef [env : Env] [x : Symbol]) : Env
  (cons (bind x (box (undefS))) env))

(define (extend-env [env : Env] [x : Symbol] [v : Value]) : Env
  (cons (bind x (box (valS v))) env))

(define (find-var [env : Env] [x : Symbol]) : (Boxof Storable)
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? x (bind-name b))
                         (bind-ref b)]
                        [else
                         (find-var rst-env x)])]))
  
(define (lookup-env [x : Symbol] [env : Env]) : Value
  (type-case Storable (unbox (find-var env x))
    [(valS v) v]
    [(undefS) (error 'lookup-env "undefined variable")]))
   
(define (update-env! [env : Env] [x : Symbol] [v : Value]) : Void
  (set-box! (find-var env x) (valS v)))


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

;; funkcja (symbol symbol -> bool) - symbol=?
(define (op-sym-bool->value [f : (Symbol Symbol -> Boolean)]) : Value 
  (primopV
   (λ (v1)
     (type-case Value v1
       [(symV n1)
        (primopV
         (λ (v2)
           (type-case Value v2
             [(symV n2)
              (boolV (f n1 n2))]
             [else
              (error 'eval "type error")])))]
       [else
        (error 'eval "type error")]))))

;; funkcja ('a listof 'a -> listof 'a) - cons
(define (op-list->value [f : ('a (Listof 'a) -> (Listof 'a))]) : Value 
  (primopV
   (λ (v1)
     (primopV
      (λ (v2)
        (type-case Value v2
          [(listV n2)
           (listV (f v1 n2))]
          [else
           (error 'eval "type error")]))))))

(define init-env 
  (foldr (λ (b env) (extend-env env (fst b) (snd b)))
         mt-env 
         (list (pair '+ (op-num-num->value +))
               (pair '- (op-num-num->value -))
               (pair '* (op-num-num->value *))
               (pair '/ (op-num-num->value /))
               (pair '= (op-num-bool->value =))
               (pair '<= (op-num-bool->value <=))
               (pair 'symbol=? (op-sym-bool->value symbol=?)) ;; rozszerzamy początkowe srodowisko o nowe funkcje
               (pair 'symbol?  (primopV (lambda (x) (boolV (symV? x)))))
               (pair 'number?  (primopV (lambda (x) (boolV (numV? x)))))
               (pair 'cons (op-list->value cons))
               (pair 'car  (primopV (lambda (x) (type-case Value x
                                                  [(listV ls) (first ls)]
                                                  [else (error 'car "type error")]))))
               (pair 'cdr  (primopV (lambda (x) (type-case Value x
                                                  [(listV ls) (listV (rest ls))]
                                                  [else (error 'cdr "type error")]))))
               (pair 'null? (primopV (lambda (x) (type-case Value x
                                                   [(listV ls) (boolV (empty? ls))]
                                                   [else (boolV #f)]))))
               (pair 'null  (listV empty)))))

;; evaluation function (eval/apply)

(define (eval [e : Exp] [env : Env]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(ifE b l r)
     (type-case Value (eval b env)
       [(boolV v)
        (if v (eval l env) (eval r env))]
       [else
        (error 'eval "type error")])]
    [(varE x)
     (lookup-env x env)]
    [(letE x e1 e2)
     (let ([v1 (eval e1 env)])
       (eval e2 (extend-env env x v1)))]
    [(lamE x b)
     (funV x b env)]
    [(appE e1 e2)
     (apply (eval e1 env) (eval e2 env))]
    [(letrecE x e1 e2)
     (let* ([new-env (extend-env-undef env x)]
            [v1 (eval e1 new-env)])
       (begin (update-env! new-env x v1) 
              (eval e2 new-env)))]
    [(symE x) (symV x)]
    [(listE ls) (listV (map (lambda (x) (eval x env)) ls))]))

(define (apply v1 v2)
  (type-case Value v1
    [(funV x b env)
     (eval b (extend-env env x v2))]
    [(primopV f)
     (f v2)]
    [else (error 'apply "not a function")]))

(define (run [e : S-Exp]) : Value
  (eval (parse e) init-env))

;; tests
(module+ test
  (test (run `null) (listV '()))
  (test (run `'x) (symV 'x))
  (test (run `'18) (numV 18))
  (test (run `'{1 2 3}) (listV (list (numV 1) (numV 2) (numV 3))))
  (test (run `{cons 1 {cons 'x {cons 7 null}}})
        (listV (list (numV 1) (symV 'x) (numV 7))))
  (test (run `{list 1 'd {+ 1 2}})
        (listV (list (numV 1) (symV 'd) (numV 3))))
  (test (run `{car {list 7 8 9}}) (numV 7))
  (test (run `{cdr {list 7 8 9}}) (listV (list (numV 8) (numV 9))))
  (test (run `(null? {cdr {list 'foo}})) (boolV #t))
  (test (run `(null? (cons 1 null))) (boolV #f))
  (test (run `(null? 89)) (boolV #f))
  (test (run `{car '{+ 1 {3 4}}}) (symV '+)))

;; evaluator in interpreted lang
(define (eval-int [s : S-Exp]) : Number
  (numV-n
   (run
    `{letrec eval {lambda {s}
                    {if {number? s}
                        s
                        {let op {car s}
                          {if {symbol=? '+ op}
                              {+ {eval {car {cdr s}}}
                                 {eval {car {cdr {cdr s}}}}}
                              {if {symbol=? '* op}
                                  {* {eval {car {cdr s}}}
                                     {eval {car {cdr {cdr s}}}}}
                                  error}}}}}
       {eval ',s}})))

(module+ test
  (test (eval-int `6) 6)
  (test (eval-int `{* 4 5}) 20)
  (test (eval-int `{+ 3 {* {+ 1 2} {* 5 6}}}) 93))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]
    [(funV x e env) "#<procedure>"]
    [(primopV f) "#<primop>"]
    [(symV x) (symbol->string x)]
    [(listV ls) (to-string (map value->string ls))]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e) init-env)))
#lang plait

(define-type Prop
  (var [v : String])
  (conj [l : Prop] [r : Prop])
  (disj [l : Prop] [r : Prop])
  (neg [f : Prop]))

(define example
  (conj (conj (var "q") (var "r")) (disj (neg (var "p")) (var "q"))))

;ZAD 7
(define (in x ls)
  (cond [(empty? ls) #f]
        [(equal? x (first ls)) #t]
        [else (in x (rest ls))]))

(define (free-vars f)
  (local [(define (it f ac)
            (cond [(var? f) (cons (var-v f) ac)]
                  [(conj? f) (append (it (conj-l f) ac) (it (conj-r f) ac))]
                  [(disj? f) (append (it (disj-l f) ac) (it (disj-r f) ac))]
                  [(neg? f) (it (neg-f f) ac)]))
    (define (fltr ls)
      (cond [(empty? ls) empty]
            [(in (first ls) (rest ls)) (rest ls)]
            [else (cons (first ls) (fltr (rest ls)))]))]
    (fltr (it f empty))))

;ZAD 8
(define example-s (hash (list (pair "p" #t) (pair "q" #t) (pair "r" #f))))

(define (eval s f)
  (cond [(var? f) (some-v (hash-ref s (var-v f)))]
        [(conj? f) (and (eval s (conj-l f)) (eval s (conj-r f)))]
        [(disj? f) (or (eval s (disj-l f)) (eval s (disj-r f)))]
        [(neg? f) (not (eval s (neg-f f)))]))

;ZAD9
(define (fast-pow n k)
  (local [(define (it n k ac)
            (cond [(= k 0) ac]
                  [(= (modulo k 2) 1) (it (* n n) (floor (/ k 2)) (* n ac))]
                  [else (it (* n n) (/ k 2) ac)]))]
  (it n k 1)))

  ; mięsko:
(define (tautology? f)
  (local [(define vars (free-vars f))
          (define n (fast-pow 2 (length vars))) ; 2^(length vars)
          (define (make-sigma k v) ; zwraca liste do zhaszowania, k - bitmask
            (if (empty? v)
                empty
                (cons (pair (first v) (= (modulo k 2) 1))
                      (make-sigma (floor (/ k 2)) (rest v)))))
          (define (it k)
            (if (= k n)
                #t
                (and (eval (hash (make-sigma k vars)) f) (it (+ 1 k)))))]
    (it 0)))

(define example-taut
  (disj (disj (var "p") (var "q")) (neg (var "q"))))

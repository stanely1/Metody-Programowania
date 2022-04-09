#lang plait

(define-type (NNF 'v)
  (nnf-lit [polarity : Boolean] [var : 'v])
  (nnf-conj [l : (NNF 'v)] [r : (NNF 'v)])
  (nnf-disj [l : (NNF 'v)] [r : (NNF 'v)]))

(define example-nnf (nnf-disj (nnf-conj (nnf-lit #t 'p)
                                        (nnf-lit #f 'q))
                              (nnf-lit #f 'r)))
;ZAD 4:
(define (neg-nnf f)
  (cond [(nnf-lit? f)
         (nnf-lit (not (nnf-lit-polarity f)) (nnf-lit-var f))]
        [(nnf-conj? f)
         (nnf-disj (neg-nnf (nnf-conj-l f)) (neg-nnf (nnf-conj-r f)))]
        [(nnf-disj? f)
         (nnf-conj (neg-nnf (nnf-disj-l f)) (neg-nnf (nnf-disj-r f)))]))
;ZAD 5:
(define (eval-nnf sigma f)
  (cond [(nnf-lit? f)
         (if (nnf-lit-polarity f)
             (sigma (nnf-lit-var f))
             (not (sigma (nnf-lit-var f))))]
        [(nnf-conj? f)
         (and (eval-nnf sigma (nnf-conj-l f))
              (eval-nnf sigma (nnf-conj-r f)))]
        [(nnf-disj? f)
         (or (eval-nnf sigma (nnf-disj-l f))
             (eval-nnf sigma (nnf-disj-r f)))]))

(define (example-sigma v)
  (cond [(equal? v 'p) #f]
        [(equal? v 'q) #f]
        [(equal? v 'r) #t]))
;ZAD 6:
(define-type (Formula 'v)
(var  [var : 'v])
(neg  [f : (Formula 'v)])
(conj [l : (Formula 'v)] [r : (Formula 'v)])
(disj [l : (Formula 'v)] [r : (Formula 'v)]))

(define (to-nnf f)
  (cond [(var? f) (nnf-lit #t (var-var f))] ; zmienna -> zmienna
        [(neg? f) (neg-nnf (to-nnf (neg-f f)))]
        [(conj? f) (nnf-conj (to-nnf (conj-l f))
                            (to-nnf (conj-r f)))]
        [(disj? f) (nnf-disj (to-nnf (disj-l f))
                            (to-nnf (disj-r f)))]))

(define example-form (conj (neg (disj (var 'p) (var 'q)))
                           (neg (neg (var 'r)))))
;ZAD 7:
(define (eval-formula sigma f)
  (cond [(var? f) (sigma (var-var f))]
        [(neg? f) (not (eval-formula sigma (neg-f f)))]
        [(conj? f) (and (eval-formula sigma (conj-l f))
                        (eval-formula sigma (conj-r f)))]
        [(disj? f) (or (eval-formula sigma (disj-l f))
                       (eval-formula sigma (disj-r f)))]))
;ZAD 8:
(define (insert x xs)
  (if (empty? xs)
      (cons x empty)
      (if (< x (first xs))
          (cons x xs)
          (cons (first xs) (insert x (rest xs))))))

(define (bound? x xs)
  (if (empty? xs)
      #t
      (<= x (first xs))))

(define (sorted? xs)
  (if (empty? xs)
      #t
      (and (sorted? (rest xs)) (bound? (first xs) (rest xs)))))
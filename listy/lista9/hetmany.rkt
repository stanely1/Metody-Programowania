#lang plait

; 4 x 4
; +---+---+---+---+
; |   | X |   |   |
; +---+---+---+---+
; |   |   |   | X |
; +---+---+---+---+
; | X |   |   |   |
; +---+---+---+---+
; |   |   | X |   |
; +---+---+---+---+

(define (concat xs)
  (if (empty? xs)
      empty
      (append (first xs) (concat (rest xs)))))

(define (valid-pos? i board)
  (local
    [(define (valid-it i j k board)
       (type-case (Listof Number) board
         [empty #t]
         [(cons x board)
          (and (not (= i x))
               (not (= j x))
               (not (= k x))
               (valid-it (- i 1) j (+ k 1) board))]))]
    (valid-it (- i 1) i (+ i 1) board)))

(define (queens n)
  (local
    [(define (queens-it board left)
       (if (= left 0)
           (list board)
           (concat (build-list n (lambda (i)
              (if (valid-pos? i board)
                  (queens-it (cons i board) (- left 1))
                  '()))))))]
    (queens-it '() n)))

; Styl kontynuacyjny (CPS)
(define (fact n cont)
  (if (= n 0)
      (cont 1)
      (fact (- n 1) (lambda (v)
        (cont (* n v))))))

;(define (select xs cont)
;  (concat (map cont xs)))
;
;(define (fail cont)
;  empty)
;
;(define (init-cont x) (list x))

(define (select xs cont)
  (type-case (Listof 'a) xs
    [empty (none)]
    [(cons x xs)
     (type-case (Optionof 'b) (cont x)
       [(none)   (select xs cont)]
       [(some v) (some v)])]))

(define (fail cont)
  (none))

(define (init-cont x) (some x))

(define (select-number n cont)
  (select (build-list n (lambda (i) i)) cont))

(define (queens2 n)
  (local
    [(define (queens-it board left cont)
       (if (= left 0)
           (cont board)
           (select-number n (lambda (i)
             (if (valid-pos? i board)
                 (queens-it (cons i board) (- left 1) cont)
                 (fail cont))))))]
    (queens-it '() n init-cont)))

; Prolog
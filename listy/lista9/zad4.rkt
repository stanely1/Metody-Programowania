#lang plait

(define-type-alias (Stream 'a) (-> (StreamData 'a)))
(define-type (StreamData 'a)
  (sempty)
  (scons [head : 'a] [tail : (Stream 'a)]))

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

(define (sappend xs ys)
  (if (sempty? xs)
      (ys)
      (scons (scons-head xs)
             (lambda () (sappend ((scons-tail xs)) ys)))))
  
(define (sconcat xs)
  (type-case (StreamData 'a) xs
    [(sempty) (sempty)]
    [(scons head tail)
     (sappend head (lambda () (sconcat (tail))))]))

(define (sbuild-list n f)
  (local [(define (it i)
            (if (= i n)
                (sempty)
                (scons (f i) (lambda () (it (+ i 1))))))]
    (it 0)))

(define (queens n)
  (local
    [(define (queens-it board left)
       (if (= left 0)
           (scons board (lambda () (sempty)))
           (sconcat (sbuild-list n (lambda (i)
              (if (valid-pos? i board)
                  (queens-it (cons i board)
                             (- left 1))
                  (sempty)))))))]
    (queens-it '() n)))
       
#lang racket
(require data/heap)
(provide sim? wire?
         (contract-out
          [make-sim        (-> sim?)]
          [sim-wait!       (-> sim? positive? void?)]
          [sim-time        (-> sim? real?)]
          [sim-add-action! (-> sim? positive? (-> any/c) void?)]

          [make-wire       (-> sim? wire?)]
          [wire-on-change! (-> wire? (-> any/c) void?)]
          [wire-value      (-> wire? boolean?)]
          [wire-set!       (-> wire? boolean? void?)]

          [bus-value (-> (listof wire?) natural?)]
          [bus-set!  (-> (listof wire?) natural? void?)]

          [gate-not  (-> wire? wire? void?)]
          [gate-and  (-> wire? wire? wire? void?)]
          [gate-nand (-> wire? wire? wire? void?)]
          [gate-or   (-> wire? wire? wire? void?)]
          [gate-nor  (-> wire? wire? wire? void?)]
          [gate-xor  (-> wire? wire? wire? void?)]

          [wire-not  (-> wire? wire?)]
          [wire-and  (-> wire? wire? wire?)]
          [wire-nand (-> wire? wire? wire?)]
          [wire-or   (-> wire? wire? wire?)]
          [wire-nor  (-> wire? wire? wire?)]
          [wire-xor  (-> wire? wire? wire?)]

          [flip-flop (-> wire? wire? wire? void?)]))

; symulacja
(struct sim ([time #:mutable] [queue #:mutable]))

; konstruktor symulacji
(define (make-sim)
  (sim 0 (make-heap (lambda (x y) (<= (car x) (car y)))))) ; kolejka trzyma pary (czas, akcja)

; uruchomienie symulacji
(define (sim-wait! sim time)
  (define (it)
    (cond [(= (heap-count (sim-queue sim)) 0) (void)]
          [(> (car (heap-min (sim-queue sim))) time) (void)] ; nastepna akcja po danym czasie
          [else
           (let [(next-event (heap-min (sim-queue sim)))]
             (begin
               (heap-remove-min! (sim-queue sim)) ; usuwamy akcję
               (set-sim-time! (car next-event))   ; przesuwamy czas
               ((cdr next-event))                 ; wywolanie akcji
               (it)))]))
  (it)
  (set-sim-time! sim time))

; dodanie akcji
(define (sim-add-action! sim time action)
  (heap-add! (sim-queue sim)
             (cons (+ (sim-time sim) time))))

; przewody
(struct wire ([value #:mutable] [actions #:mutable] sim))

; konstruktor przewodu
(define (make-wire sim)
  (wire #f null sim))

; dodanie akcji (wykonywanej przy zmianie stanu)
(define (wire-on-change! wire action)
  (set-wire-actions! wire (cons action (wire-actions wire))))

; ustawienie wartosci
(define (wire-set! wire value)
  (if (equal? (wire-value wire) value)
      (void) ; bez zmian
      (begin
       (set-wire-value! wire value)
       (call-wire-actions wire))))

; wywolanie akcji
(define (call-wire-actions wire)
  (define (it actions)
    (if (null? actions)
        (void)
        (begin
          ((car actions))
          (it (cdr actions)))))
  (it (wire-actions wire)))

; magistrala
(define (bus-set! wires value)
  (match wires
    ['() (void)]
    [(cons w wires)
     (begin
       (wire-set! w (= (modulo value 2) 1))
       (bus-set! wires (quotient value 2)))]))

(define (bus-value ws)
  (foldr (lambda (w value) (+ (if (wire-value w) 1 0) (* 2 value)))
         0
         ws))

; bramki logiczne
(define (gate-not out in)
  (define (action)
    (wire-set! out (not (wire-value in))))
  (wire-on-change! in action))

(define (gate-and out in1 in2)
  (define (action)
    (wire-set! out (and (wire-value in1) (wire-value in2))))
  (wire-on-change! in1 action)
  (wire-on-change! in2 action))

(define (gate-nand out in1 in2)
  (define (action)
    (wire-set! out (not (and (wire-value in1) (wire-value in2)))))
  (wire-on-change! in1 action)
  (wire-on-change! in2 action))

(define (gate-or out in1 in2)
  (define (action)
    (wire-set! out (or (wire-value in1) (wire-value in2))))
  (wire-on-change! in1 action)
  (wire-on-change! in2 action))

(define (gate-nor out in1 in2)
  (define (action)
    (wire-set! out (not (or (wire-value in1) (wire-value in2)))))
  (wire-on-change! in1 action)
  (wire-on-change! in2 action))

(define (gate-xor out in1 in2)
  (define (action)
    (wire-set! out (xor (wire-value in1) (wire-value in2))))
  (wire-on-change! in1 action)
  (wire-on-change! in2 action))

; przewody z bramkami
(define (wire-not in)
  (define new-wire (make-wire null))
  (gate-not new-wire in)
  new-wire)

(define (wire-and in1 in2)
  (define new-wire (make-wire null))
  (gate-and new-wire in1 in2)
  new-wire)

(define (wire-nand in1 in2)
  (define new-wire (make-wire null))
  (gate-nand new-wire in1 in2)
  new-wire)

(define (wire-or in1 in2)
  (define new-wire (make-wire null))
  (gate-or new-wire in1 in2)
  new-wire)

(define (wire-nor in1 in2)
  (define new-wire (make-wire null))
  (gate-nor new-wire in1 in2)
  new-wire)

(define (wire-xor in1 in2)
  (define new-wire (make-wire null))
  (gate-xor new-wire in1 in2)
  new-wire)

;
(define (flip-flop out clk data)
  (define sim (wire-sim data))
  (define w1  (make-wire sim))
  (define w2  (make-wire sim))
  (define w3  (wire-nand (wire-and w1 clk) w2))
  (gate-nand w1 clk (wire-nand w2 w1))
  (gate-nand w2 w3 data)
  (gate-nand out w1 (wire-nand out w3)))
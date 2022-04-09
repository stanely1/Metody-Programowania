#lang racket

(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join)

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define cities
  (table
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
   (list (list "Wrocław" "Poland"  293 #f)
         (list "Warsaw"  "Poland"  517 #t)
         (list "Poznań"  "Poland"  262 #f)
         (list "Berlin"  "Germany" 892 #t)
         (list "Munich"  "Germany" 310 #f)
         (list "Paris"   "France"  105 #t)
         (list "Rennes"  "France"   50 #f))))

(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number))
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47))))

(define (empty-table columns) (table columns '()))

; Wstawianie

(define (table-insert row tab)
  (define (valid? row col)
    (cond [(null? row) (null? col)] ; ten sam rozmiar
          [(null? col) (null? row)]
          [else (and
                 (cond [(equal? (column-info-type (car col)) 'number) (number? (car row))]
                       [(equal? (column-info-type (car col)) 'string) (string? (car row))]
                       [(equal? (column-info-type (car col)) 'symbol) (symbol? (car row))]
                       [(equal? (column-info-type (car col)) 'boolean) (boolean? (car row))])
                 (valid? (cdr row) (cdr col)))])) ; zgodnosc typow
  
  (if (valid? row (table-schema tab))
      (table (table-schema tab) (cons row (table-rows tab)))
      (error 'table-insert "invalid row!")))

; Projekcja

(define (get-col-num name tab) ; numer kolumny
  (define (it col num)
    (if (equal? (column-info-name (car col)) name)
        num
        (it (cdr col) (+ 1 num))))
  (it (table-schema tab) 0))

(define (table-project cols tab)
  (define (make-filter-list cols)
    (if (null? cols)
        null
        (cons (get-col-num (car cols) tab)
              (make-filter-list (cdr cols)))))
  
  (define filter-list (make-filter-list cols))
  
  (define (filter-row row lst)
    (if (null? lst)
        null
        (cons (list-ref row (car lst)) (filter-row row (cdr lst)))))
  
  (define (filter-all rows)
    (if (null? rows)
        null
        (cons (filter-row (car rows) filter-list)
              (filter-all (cdr rows)))))
  
  (define (make-schema cols lst)
    (if (null? lst)
        null
        (cons (list-ref cols (car lst)) (make-schema cols (cdr lst)))))
  
  (table (make-schema (table-schema tab) filter-list) (filter-all (table-rows tab))))

; Sortowanie

(define (table-sort cols tab)
  (define (cmp row1 row2 col key) ; porownuje po jednnym kluczu (1 - r1 < r2 ; 0 - r1 = r2 ; -1 - r1 > r2
    (cond [(null? col) 1]
          [(equal? (column-info-name (car col)) key)
           (cond [(equal? (car row1) (car row2)) 0]
                 [(equal? (column-info-type (car col)) 'number)
                  (if (< (car row1) (car row2)) 1 -1)]
                 [(equal? (column-info-type (car col)) 'string)
                  (if (string<? (car row1) (car row2)) 1 -1)]
                 [(equal? (column-info-type (car col)) 'symbol)
                  (if (symbol<? (car row1) (car row2)) 1 -1)]
                 [(equal? (column-info-type (car col)) 'boolean)
                  (if (car row1) -1 1)])]
          [else (cmp (cdr row1) (cdr row2) (cdr col) key)]))
  
  (define (compare row1 row2 keys) ; po kolei porownuje klucze
    (if (null? keys)
        #f
        (let [(x (cmp row1 row2 (table-schema tab) (car keys)))]
          (cond [(= x 1) #t]
                [(= x 0) (compare row1 row2 (cdr keys))]
                [else #f]))))
    
  (table (table-schema tab)
         (sort (table-rows tab) (lambda (x y) (compare x y cols)))))

; Selekcja

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (table-select form tab)
  (define (cmp val1 val2 type) ; porownaie val1 < val2 w zaleznosci of typu
    (cond [(equal? type 'number) (< val1 val2)]
          [(equal? type 'string) (string<? val1 val2)]
          [(equal? type 'symbol) (symbol<? val1 val2)]
          [(equal? type 'boolean) (if val1 #f val2)]))
    
  (define (check row f) ; sprawdzenie pojedynczego wiersza
    (cond [(lt-f? f)
           (let [(num (get-col-num (lt-f-name f) tab))]
             (cmp (list-ref row num) (lt-f-val f)
                  (column-info-type (list-ref (table-schema tab) num))))]
          [(eq-f? f)
           (let [(num (get-col-num (eq-f-name f) tab))]
             (equal? (list-ref row num) (eq-f-val f)))]
          [(eq2-f? f)
           (let [(num1 (get-col-num (eq2-f-name f) tab))
                 (num2 (get-col-num (eq2-f-name2 f) tab))]
             (equal? (list-ref row num1) (list-ref row num2)))]
          [(not-f? f) (not (check row (not-f-e f)))]
          [(or-f? f) (or (check row (or-f-l f)) (check row (or-f-r f)))]
          [(and-f? f) (and (check row (and-f-l f)) (check row (and-f-r f)))]))

  (table (table-schema tab)
         (filter (lambda (x) (check x form)) (table-rows tab))))
    

; Zmiana nazwy

(define (table-rename col ncol tab)
 (define (make-cols cols)
   (if (null? cols)
       null
       (cons
        (if (equal? (column-info-name (car cols)) col)
            (column-info ncol (column-info-type (car cols)))
            (car cols))
        (make-cols (cdr cols)))))
  
  (table (make-cols (table-schema tab)) (table-rows tab)))

; Złączenie kartezjańskie

(define (table-cross-join tab1 tab2)
  (define (make-rows rows1 rows2)
    (cond [(null? rows1) null]
          [(null? rows2) (make-rows (cdr rows1) (table-rows tab2))]
          [else (cons (append (car rows1) (car rows2)) (make-rows rows1 (cdr rows2)))]))
  
  (table (append (table-schema tab1) (table-schema tab2))
         (make-rows (table-rows tab1) (table-rows tab2))))

; Złączenie

(define (table-natural-join tab1 tab2)
  (define (symbol-append a b)
    (string->symbol (string-append (symbol->string a) (symbol->string b))))

  (define (find-col cols name)
    (cond [(null? cols) #f]
          [(equal? (column-info-name (car cols)) name) #t]
          [else (find-col (cdr cols) name)]))

  (define (rep-list cols1 cols2)
    (cond [(null? cols2) null]
          [(find-col cols1 (column-info-name (car cols2)))
           (cons
            (cons (column-info-name (car cols2))
                  (symbol-append (column-info-name (car cols2)) '_))
            (rep-list cols1 (cdr cols2)))]
          [else (rep-list cols1 (cdr cols2))]))

  (define (rename-rep cols tab)
    (if (null? cols)
        tab
        (table-rename (caar cols) (cdar cols) (rename-rep (cdr cols) tab))))

  (define (select-rep-eq cols tab)
    (if (null? cols)
        tab
        (table-select (eq2-f (caar cols) (cdar cols))
                      (select-rep-eq (cdr cols) tab))))
  
  (define (no-rep-list rlist cols)
    (cond [(null? cols) null]
          [(and (not (null? rlist))
                (equal? (column-info-name (car cols)) (caar rlist)))
           (no-rep-list (cdr rlist) (cdr cols))]
          [else (cons (column-info-name (car cols))
                      (no-rep-list rlist (cdr cols)))]))

  (letrec [(s1 (table-schema tab1))
           (s2 (table-schema tab2))
           (rlist (rep-list s1 s2))]
    (table-project (append (map column-info-name s1)
                           (no-rep-list rlist s2))
                   (select-rep-eq rlist
                                  (table-cross-join tab1
                                                    (rename-rep rlist tab2))))))
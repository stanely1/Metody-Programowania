#lang racket

(define code-table
  `((#\A . "._"  )
    (#\B . "_...")
    (#\C . "_._.")
    (#\D . "_.." )
    (#\E . "."   )
    (#\F . ".._.")
    (#\G . "__." )
    (#\H . "....")
    (#\I . ".."  )
    (#\J . ".___")
    (#\K . "_._" )
    (#\L . "._..")
    (#\M . "__"  )
    (#\N . "_."  )
    (#\O . "___" )
    (#\P . ".__.")
    (#\Q . "__._")
    (#\R . "._." )
    (#\S . "..." )
    (#\T . "_"   )
    (#\U . ".._" )
    (#\W . ".__" )
    (#\X . "_.._")
    (#\Y . "_.__")
    (#\Z . "__..")
    (#\1 . ".____")
    (#\2 . "..___")
    (#\3 . "...__")
    (#\4 . "...._")
    (#\5 . ".....")
    (#\6 . "_....")
    (#\7 . "__...")
    (#\8 . "___..")
    (#\9 . "____.")
    (#\0 . "_____")
    (#\. . "._._._")
    (#\, . "__..__")
    (#\? . "..__..")
    (#\! . "_._.__")))

(define (morse-code str)
  (define chars (map char-upcase (string->list str)))
  (define (it xs white?)
    (if (null? xs)
        null
        (if (char-whitespace? (car xs))
            (if white?
                (it (cdr xs) #t)
                (append (string->list "  ") (it (cdr xs) #t)))
            (append (string->list (cdr (assoc (car xs) code-table)))
                    (if (null? (cdr xs)) null (cons #\space (it (cdr xs) #f)))))))
  (list->string (it chars #f)))

; Zad 7

(define (morse-decode str)
  (define words (string-split str "  "))
  (define key (map (lambda (x) (cons (cdr x) (car x))) code-table))
  
  (define (word-decode word)
    (define chars (string-split word))
    (map (lambda (x) (cdr (assoc x key))) chars))
  
  (list->string
   (append-map
    (lambda (x)
      (if (eq? x (car words))
          (word-decode x)
          (cons #\space (word-decode x))))
    words)))
    
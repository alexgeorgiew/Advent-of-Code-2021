#lang racket



(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-line p)))
      (if (eof-object? c) (begin (close-input-port p) (reverse ls1))  (loop (cons c ls1) (read-line p)))
     )
    )
  )


(define fi "C:/Users/alexm/Desktop/day2_problem2_input.txt")

(define (input)
  (read-file fi)
  )

(define lis (input))  ;;list of strings


(define (take-commands-as-list-of-strings in)   
    (if (null? in) '() (cons (list->string (reverse (cdr (reverse (string->list(car in)))))) (take-commands-as-list-of-strings (cdr in)))) 
  )
(define getinput (take-commands-as-list-of-strings lis))   ;give list of  every line converted to strings

(define (split-by-space input)   ;every line is split by space and returned as list
  (if (null? input) '() (cons (regexp-split #px" "(car input)) (split-by-space (cdr input))))
  )

(define (calculate-pos input)
  (define (helper horizontal vertical aim commands)
    (cond
      ((null? commands) (* horizontal vertical))
      ((equal? (car (car commands)) "forward") (helper (+ horizontal (string->number(cadr (car commands)))) (+ vertical (* aim (string->number(cadr (car commands))))) aim (cdr commands)))
      ((equal? (car (car commands)) "up") (helper horizontal  vertical (- aim (string->number(cadr (car commands)))) (cdr commands)))
      ((equal? (car (car commands)) "down") (helper horizontal  vertical (+ aim (string->number(cadr (car commands)))) (cdr commands)))
      (else (helper horizontal vertical (cdr commands)))
      )
    )
  (helper 0 0 0 input)
  )



; enter (calculate-pos (split-by-space getinput))
;                                                  to get answer











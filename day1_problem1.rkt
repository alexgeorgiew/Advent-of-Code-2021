#lang racket



(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-line p)))
      (if (eof-object? c) (begin (close-input-port p) (reverse ls1))  (loop (cons c ls1) (read-line p)))
     )
    )
  )


(define fi "C:/Users/alexm/Desktop/day1_1.txt")

(define (input)
  (read-file fi)
  )

(define lis (input))  ;;list of strings

(define (take-nums-as-strings in)   
    (if (null? in) '() (cons (list->string (reverse (cdr (reverse (string->list(car in)))))) (take-nums-as-strings (cdr in)))) 
  )

(define (list-from-strings-to-list-of-int input)
  (if (null? input) '() (cons (string->number(car input)) (list-from-strings-to-list-of-int (cdr input)))) 
  )
;(list-from-strings-to-list-of-int (take-nums-as-strings lis))  this gives input as integers

(define (counter input)
 (define (helper cur nums)
   (if (null? (cdr nums)) cur (if (< (car nums) (cadr nums)) (helper (+ cur 1) (cdr nums)) (helper cur (cdr nums)) ))
 )
   (helper 0 input)
  )



;(counter (list-from-strings-to-list-of-int (take-nums-as-strings lis)))   this is the input for the console that solve day1_1
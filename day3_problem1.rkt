#lang racket



(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-line p)))
      (if (eof-object? c) (begin (close-input-port p) (reverse ls1))  (loop (cons c ls1) (read-line p)))
     )
    )
  )

(define fi "C:/Users/alexm/Desktop/day3_problem1_input.txt")

(define (input)
  (read-file fi)
  )

(define lis (input))  ;list of strings

(define (take-commands-as-list-of-strings in)   
    (if (null? in) '() (cons (list->string (reverse (cdr (reverse (string->list(car in)))))) (take-commands-as-list-of-strings (cdr in)))) 
  )

(define getinput (take-commands-as-list-of-strings lis))   ;give list of  every line converted to strings

(define (len-of list)
  (if (null? list) 0 (+ 1 (len-of (cdr list))))
  )

(define len-of-file (len-of lis))

(define bitinput-len (string-length (car getinput)))    ;length of every line in file

(define (create-list size)                                  
 (if (= size 0) '() (cons 0 (create-list (- size 1))))
  )


(define (add-1-at-index index list)                            ; index 0 , 1, 2.....  ,bitinput-len - 1
  
  (if (null? list) '() (if (= index 0) (cons (+ 1 (car list)) (cdr list)) (cons (car list) (add-1-at-index (- index 1) (cdr list)))))
  )

(define (count-ones-at-line line count-list)
 (define (helper index line count-list)
   (cond
    ((null? line) count-list)
    ((equal? (car line) #\1) (helper (+ index 1) (cdr line) (add-1-at-index index count-list)))
    (else (helper (+ index 1) (cdr line) count-list))
    )
   )
  (helper 0 line count-list)
  )

(define (count input count-list)
  (if (null? input) count-list (count (cdr input) (count-ones-at-line (string->list(car input)) count-list)))
  )
; 
;                                                  to get answer




;(count getinput (create-list bitinput-len))   to get count of 1 at every pos from whole input

(define (pow num step)
  (define (helper res num step)
    (cond
      ((= step 0) (* res 1))
      (else (helper (* res num) num (- step 1)))
      )
    )
  (helper 1 num step)
  )


(define (calculate-gamma input)
 (define (helper list res power-of-2 )
   (cond
    ((null? list) res)
    ((> (car list) (quotient len-of-file 2)) ( helper (cdr list) (+ res (pow 2 power-of-2)) (- power-of-2 1)))
    (else (helper (cdr list) res (- power-of-2 1)))
     )
   )
  (helper input 0 (- bitinput-len 1))
  )

(define (calculate-epsilon)
  (-(- (pow 2 bitinput-len) 1) (calculate-gamma (count getinput (create-list bitinput-len ))))
  )

(define (solve)
 (* (calculate-gamma (count getinput (create-list bitinput-len))) (calculate-epsilon))
  )

; (solve)  to get answer




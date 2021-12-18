#lang racket



(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-line p)))
      (if (eof-object? c) (begin (close-input-port p) (reverse ls1))  (loop (cons c ls1) (read-line p)))
     )
    )
  )

(define fi "C:/Users/alexm/Desktop/day3_problem2_input.txt")

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

(define (create-list size)                                  ;create list wit zeroes
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

(define (calculate-oxygen-bits input count1)            ;find oxygen bit mask
 (define (helper list)
   (cond
    ((null? list) '())
    ((or (> (car list) (quotient (len-of input) 2)) (= (car list) (+ (quotient (len-of input) 2) (remainder (len-of input) 2)))) (cons 1 ( helper (cdr list) )))
    (else (cons 0 (helper (cdr list) )))
     )
   )
  (helper count1)
 )

(define (calculate-co2-bits input count1)            ;find co2 scrubber bit mask
 (define (helper list)
   (cond
    ((null? list) '())
    ((or (> (car list) (quotient(len-of input) 2)) (= (car list) (+ (quotient (len-of input) 2) (remainder (len-of input) 2)))) (cons 0 ( helper (cdr list) )))
    (else (cons 1 (helper (cdr list) )))
     )
   )
  (helper count1)
 )

(define oxygen-bit-mask (calculate-oxygen-bits getinput (count getinput (create-list bitinput-len))))
(define co2-bit-mask (calculate-co2-bits getinput (count getinput (create-list bitinput-len))))

(define (symbol-at-index index list)   ;we know that index< lenght-of list
  (cond
    ((= index 0) (car list))
    (else (symbol-at-index (- index 1) (cdr list)))
    )
  )

(define (find-number-oxy numbers mask)
  (define (helper match-count numbers mask index-symbol list-match)
   (cond
    ((and (null? numbers) (= match-count 1)) (car list-match))
    ((null? numbers) (helper 0 list-match (calculate-oxygen-bits list-match (count list-match (create-list bitinput-len))) (+ index-symbol 1) '()))
    ((= (- (char->integer (symbol-at-index index-symbol (string->list(car numbers) ) ) ) 48) (symbol-at-index index-symbol mask)) (helper (+ match-count 1) (cdr numbers) mask index-symbol (cons (car numbers) list-match)) )
    (else (helper match-count (cdr numbers) mask index-symbol list-match))
     )
    )
  (helper 0 numbers mask 0 '())
  )

(define (find-number-co2 numbers mask)
  (define (helper match-count numbers mask index-symbol list-match)
   (cond
    ((and (null? numbers) (= match-count 1)) (car list-match))
    ((null? numbers) (helper 0 list-match (calculate-co2-bits list-match (count list-match (create-list bitinput-len))) (+ index-symbol 1) '()))
    ((= (- (char->integer (symbol-at-index index-symbol (string->list(car numbers) ) ) ) 48) (symbol-at-index index-symbol mask)) (helper (+ match-count 1) (cdr numbers) mask index-symbol (cons (car numbers) list-match)) )
    (else (helper match-count (cdr numbers) mask index-symbol list-match))
     )
    )
  (helper 0 numbers mask 0 '())
  )
  
(define (convert-list-string-bits-to-int strin)  ; give string of zereos and ones
  (define (helper input res len)
    (if (null? input) res (helper (cdr input) (+ res (* (- (char->integer(car input)) 48) (pow 2 len ))) (- len 1)))
    )
  (helper strin 0 (- bitinput-len 1))
  )

(define (solve)
 (* (convert-list-string-bits-to-int (string->list(find-number-oxy getinput oxygen-bit-mask)))  (convert-list-string-bits-to-int (string->list(find-number-co2 getinput co2-bit-mask))) )
  )
;enter (solve) to get answer

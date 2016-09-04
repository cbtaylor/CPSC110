;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listofnumbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ListOfNumbers is one of:
;;  - empty
;;  - (cons String ListOfNumbers)
;; interp. a list of numbers

(define LON-1 empty)
(define LON-2 (cons 4 empty))
(define LON-3 (cons 7 (cons 3 empty)))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]                   ;BASE CASE
        [else (... (first lon)                 ;Number
                   (fn-for-lon (rest lon)))])) ;NATURAL RECURSION
;;             /
;;            /
;;       COMBINATION
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumbers)
;;  - self-reference: (rest los) is ListOfNumbers

;; ListOfNumbers -> Number
;; produce the sum of the numbers in a list
(check-expect (sum empty) 0)
(check-expect (sum (cons 1 empty)) 1)
(check-expect (sum (cons 1 (cons 4 (cons 5 (cons 6 empty))))) 16)

;(define (sum lon) 0)   ;stub

(define (sum lon)
  (cond [(empty? lon) 0]          
        [else (+ (first lon)
                 (sum (rest lon)))]))

; template taken from ListOfNumbers


;; ListOfNumbers -> Number
;; produce the count of the numbers in a list
(check-expect (count empty) 0)
(check-expect (count (cons 5 empty)) 1)
(check-expect (count (cons 1 (cons 4 (cons 5 (cons 6 empty))))) 4)

;(define (count lon) 0)   ;stub

(define (count lon)
  (cond [(empty? lon) 0]          
        [else (+ 1
                 (count (rest lon)))]))

; template taken from ListOfNumbers


;; ListOfNumbers -> Boolean
;; produce true if the list contains 13
(check-expect (contains-13? empty) false)
(check-expect (contains-13? (cons 13 empty)) true)
(check-expect (contains-13? (cons 1 (cons 4 (cons 5 (cons 13 empty))))) true)

;(define (count lon) 0)   ;stub

(define (contains-13? lon)
  (cond [(empty? lon) false]          
        [else (if (= (first lon) 13)
              true
         (contains-13? (rest lon)))]))

01234567890123456789012345678901234567890123456789012345678901234567890123456789

; template taken from ListOfNumbers
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname contains-heartbreak?) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ListOfString is one of:
;;  - empty
;;  - (cons String ListOfString)
;; interp. a list of strings

(define TF empty)
(define FF (cons "Bruins" empty))
(define JF (cons "Sharks" (cons "Leafs" empty)))
(define HT (cons "Sharks" (cons "Penguins" (cons "Canucks" empty))))
(define HD (cons "Sens" HT))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]                   ;BASE CASE
        [else (... (first los)                 ;String
                   (fn-for-los (rest los)))])) ;NATURAL RECURSION
;;             /
;;            /
;;       COMBINATION
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons String ListOfString)
;;  - self-reference: (rest los) is ListOfString

;; ListOfString -> Boolean
;; produce true if list contains "Canucks"

(check-expect (contains-heartbreak? TF) false)
(check-expect (contains-heartbreak? FF) false)
(check-expect (contains-heartbreak? JF) false)
(check-expect (contains-heartbreak? HT) true)

;(define (contains-heartbreak? los) false)   ;stub

(define (contains-heartbreak? los)
  (cond [(empty? los) false]                   
        [else 
         (if (string=? "Canucks" (first los))
                        true
                        (contains-heartbreak? (rest los)))]))



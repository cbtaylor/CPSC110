;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname prime-factors) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; prime-factors: Natural -> (listof Natural)
;; produce a list of prime factors of a given number
(check-expect (prime-factors 2) (list 2))
(check-expect (prime-factors 4) (list 2 2))
(check-expect (prime-factors 20) (list 2 2 5))
(check-expect (prime-factors 75) (list 3 5 5))
(check-expect (prime-factors 29) (list 29))

(define (prime-factors n)
  (local [(define (prime-factors remaining primes divisor)
           (cond [(= remaining 1) (reverse primes)]
                 [(= (modulo remaining divisor) 0)
                  (prime-factors (/ remaining divisor) (cons divisor primes) divisor)]
                 [else
                  (prime-factors remaining primes (add1 divisor))]))]
    (prime-factors n empty 2)))

                  
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Mandelbrot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Tim Straubinger
; CompSci 110
; Sept 10th, 2014

(require racket/base)
(require racket/class)
(require racket/draw)

(define maxit 10) ; maximum number of iterations
(define escape 10) ; escape vale for the complex number Z
(define res 300) ; pixels, height and width
(define size 1) ; pixel size

(define target (make-bitmap (* size res) (* size res))) ; output image
(define dc (new bitmap-dc% [bitmap target])) ; drawing context

(send dc set-pen "black" 1 'transparent) ; no outline for these pixels
(send dc set-smoothing 'aligned) ; pixel-precise alignment

(define (renderPoint x y) ; function to calculate mandelbrot set at point (x, y) and colour to the drawing context
  (define Cr (/ (- x (/ res 1.5)) (/ res 4))) ; real value of C, scaled to the complex plane
  (define Ci (/ (- y (/ res 2)) (/ res 4))) ; imaginary value of C, also scaled
  (define temp 0) ; temporary variable used in calculations
  (define Zr 0) ; real value of Z
  (define Zi 0) ; imaginary value of Z
  (for ([i maxit]) ; iterate
    (set! temp Zr)
    (set! Zr (- (* Zr Zr) (* Zi Zi))) ; complex multiplaction of Z * Z
    (set! Zi (* 2 temp Zi))
    (set! Zr (+ Zr Cr)) ; addition of C to Z
    (set! Zi (+ Zi Ci))
    )
   (if (>= (* escape escape) (+ (sqr Zr) (sqr Zi))) (send dc set-brush "black" 'solid) (send dc set-brush "red" 'solid) ) ; colour screen black if the absolute value of Z escapes, red otherwise
    (send dc draw-rectangle (* x size) (* y size) size size)
)

; sort through all pixels
(for ([ay (+ res 1)])
  (for ([ax (+ res 1)])
    (renderPoint ax ay)
    )
   (display (string-append (number->string (exact->inexact (* (/ ay res) 100))) "% done...\n"))
  )

target ; display the image
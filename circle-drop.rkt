;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname circle-drop) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;; Circle drop program

;; =================
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)

(define SPEED 1)
(define BALL-RADIUS 20)

(define BALL-IMAGE (circle BALL-RADIUS "solid" "red"))

(define MTS (empty-scene WIDTH HEIGHT))

(define POS-X (/ WIDTH 2))

;; =================
;; Data definitions:

;; Circle is Number
;; Circle is Number[0, HEIGHT]
;; Circle is Number[BALL-RADIUS, HEIGHT - BALL-RADIUS]
;; Circle is Integer[BALL-RADIUS, HEIGHT - BALL-RADIUS]
;; interp. y position of center of ball in screen coordinates (pixels)
(define C1 (+ 0 BALL-RADIUS))       ;top
(define C2 (- HEIGHT BALL-RADIUS))  ;bottom
(define C3 (/ HEIGHT 2))            ;middle
#;
(define (fn-for-circle c)
  (... c))

;; Template rules used:
;;  - atomic non-distinct: Number




;; =================
;; Functions:

;; Circle -> Circle
;; start the world with (main 0)
;; 
(define (main c)
  (big-bang c                    ; Circle
            (on-tick   drop)     ; Circle -> Circle
            (to-draw   render)   ; Circle -> Image
            #;
            (stop-when ...)      ; Circle -> Boolean
            (on-mouse  handle-mouse)      ; Circle Integer Integer MouseEvent -> Circle
            #;
            (on-key    ...)    ; Circle KeyEvent -> Circle
            ))

;; Circle -> Circle
;; produce the next circle, but do not allow circle to drop off bottom of screen
(check-expect (drop 5) (+ 5 SPEED))

;(define (drop c) 0) ; stub

;<use template from Circle>

(define (drop c) 
  (cond [(<= c (- HEIGHT (+ BALL-RADIUS SPEED))) (+ c SPEED)]
        [else (- HEIGHT BALL-RADIUS)]))


;; Circle -> Image
;; render the circle image at appropriate place on MTS
(check-expect (render 5) (place-image BALL-IMAGE POS-X 5 MTS))

;(define (render c) MTS) ;stub

;<use template from Circle>

(define (render c)
  (place-image BALL-IMAGE POS-X c MTS))


;; Circle MouseEvent -> Circle
;; set center of circle to y-position of mouse click
(check-expect (handle-mouse 30 50 60 "button-down") 60)

;(define (handle-mouse c x y me) 0) ;stub

(define (handle-mouse c x y me)
  (cond [(mouse=? me "button-down") y]
        [else c]))
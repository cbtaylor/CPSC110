;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname circle-drop.v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;; Circle drop program

;; =================
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)

(define SPEED 1)         ; the speed at which the ball drops
(define RADIUS-SPEED 1)  ; the speed at which the radius shrinks
(define MTS (empty-scene WIDTH HEIGHT))
(define BALL-COLOR "red")

;; =================
;; Data definitions:

(define-struct ball (x y r))
;; Ball is (make-ball Integer[0 WIDTH]
;;                    Integer[0, HEIGHT]
;;                    Natural)
;; interp. a ball with center at position (x, y) with radius r

(define B1 (make-ball 10 0 100))       ;top and large
(define B2 (make-ball 10 0 100))       ;middle and small

#;
(define (fn-for-ball b)
  (... (ball-x b)     ;Integer[0, WIDTH]
       (ball-y b)     ;Integer[0, HEIGHT]
       (ball-r b)))   ;Natural

;; Template rules used:
;;  - compound: 3 fields



;; =================
;; Functions:

;; Ball -> Ball
;; start the world with (main 0)
;; 
(define (main b)
  (big-bang b                    ; Ball
            (on-tick   drop)     ; Ball -> Ball
            (to-draw   render)   ; Ball -> Image
            #;
            (stop-when ...)      ; Ball -> Boolean
            (on-mouse  handle-mouse)      ; Ball Integer Integer MouseEvent -> Ball
            #;
            (on-key    ...)    ; Ball KeyEvent -> Ball
            ))

;; Ball -> Ball
;; produce the next ball, but do not allow ball to drop off bottom of screen
(check-expect (drop (make-ball 100 100 10)) (make-ball 100 (+ 5 SPEED) (- 10 RADIUS-SPEED))
(check-expect (drop (make-ball 100 (- HEIGHT 8) 10)) (make-ball 100 HEIGHT (- 10 RADIUS-SPEED))
(check-expect (drop (make-ball 100 (- HEIGHT 8) 0)) (make-ball 100 HEIGHT 0))

#;  
(define (drop b)
  (... (ball-x b)     ;Integer[0, WIDTH]
       (ball-y b)     ;Integer[0, HEIGHT]
       (ball-r b)))   ;Natural ; stub

;<use template from Ball>

(define (drop b)
  (if (> (+ b-y SPEED) (- HEIGHT b-r))
      (make-ball (b-x (+ b-y SPEED) b-r))
      (make-ball (b-x (- HEIGHT br)))))


;; Ball -> Image
;; render the ball image at appropriate place on MTS
(check-expect (render (make-ball 10 20 30))
              (place-image (circle 30 "solid" BALL-COLOR) 10 20))

;(define (render b) MTS) ;stub

;<use template from Ball
#;  
(define (drop b)
  (... (ball-x b)     ;Integer[0, WIDTH]
       (ball-y b)     ;Integer[0, HEIGHT]
       (ball-r b)))   ;Natural ; stub

(define (render b)
  (place-image (circle b-r "solid" BALL-COLOR) b-x b-y))
 

  (place-image BALL-IMAGE POS-X c MTS))


;; Circle MouseEvent -> Circle
;; set center of circle to y-position of mouse click
(check-expect (handle-mouse 30 50 60 "button-down") 60)

;(define (handle-mouse c x y me) 0) ;stub

(define (handle-mouse c x y me)
  (cond [(mouse=? me "button-down") y]
        [else c]))
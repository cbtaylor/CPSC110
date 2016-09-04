;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require 2htdp/image)

(define W "W")
(define B "B")
(define G "G")

(define M0 (list W W W W
                 B B W W
                 W W B G
                 W W W W))

(define M1 (list W W W W
                 B B B W
                 W W B G
                 W W W W))

(define M2 (list W W W W W
                 B B B W G
                 W W B W B
                 W W B B B
                 W W W W W))

(define-struct pos (x y))
;; Pos is (make-pos Natural Natural)
;; interp the cell position in a maze (0, 0) in top left
(define P0 (make-pos 0 0))
(define P1 (make-pos 1 1))
(define P2 (make-pos 2 1))

;;=======================
;;  Question 3
;;=======================

;; Maze Pos -> (listof Pos)
;; produce a list of next possible moves given a position and a maze
(check-expect (next-moves M0 P1) (list (make-pos 0 1)))
(check-expect (next-moves M1 P1) (list (make-pos 2 1) (make-pos 0 1)))
(check-expect (next-moves M1 P2) (list (make-pos 1 1) (make-pos 2 2)))

(define (next-moves m p)
  (prune m (generate-4-pos p)))


;; Maze (listof Pos) -> (listof Pos)
;; get rid of invalid positions
(check-expect (prune M0 (list P0 P1 P2)) (list P1))
(check-expect (prune M1 (list P0 P1 P2)) (list P1 P2))
(check-expect (prune M1 (list (make-pos -1 2))) empty)
(check-expect (prune M1 (list (make-pos 4 2))) empty)
(check-expect (prune M1 (list (make-pos 2 -1))) empty)
(check-expect (prune M1 (list (make-pos 2 4))) empty)

(define (prune m lop)
  (local [;; Pos -> Boolean ;; produce true if it's inside the maze
          (define (inside? p) (and (<= 0 (pos-x p) (sub1 (sqrt (length m))))
                                   (<= 0 (pos-y p) (sub1 (sqrt (length m))))))
          ;; Pos -> Wall ;; produce true if it's not a wall
          (define (not-wall? p) (not (string=? W (list-ref m (board-ref p m)))))]
    (filter not-wall? (filter inside? lop))))


;; Pos Maze -> Natural
;; produce the list index given a position
(check-expect (board-ref P0 M0) 0)
(check-expect (board-ref P1 M0) 5)
(check-expect (board-ref (make-pos 3 2) M0) 11)

(define (board-ref p m)
  (+ (* (pos-y p) (sqrt (length m))) (pos-x p)))

;; Pos -> (listof Pos)
;; produce all four positions (up, down, left, right) given a position
(check-expect (generate-4-pos P0) (list (make-pos 1 0)
                                        (make-pos -1 0)
                                        (make-pos 0 1)
                                        (make-pos 0 -1)))

(define (generate-4-pos p)
  (list (make-pos (add1 (pos-x p)) (pos-y p))
        (make-pos (sub1 (pos-x p)) (pos-y p))
        (make-pos (pos-x p) (add1 (pos-y p)))
        (make-pos (pos-x p) (sub1 (pos-y p)))))

;;=======================
;;  Question 4
;;=======================

(define CELL-SIZE 20)
(define WALL (square CELL-SIZE "solid" "black"))
(define BLANK (square CELL-SIZE "solid" "white"))
(define GOAL (overlay (circle (/ CELL-SIZE 2.5) "solid" "black") BLANK))

;; Maze -> Image
;; produce an image of the maze
(check-expect (render-maze M1)
              (above (beside WALL  WALL  WALL  WALL)
                     (beside BLANK BLANK BLANK WALL)
                     (beside WALL  WALL  BLANK GOAL)
                     (beside WALL  WALL  WALL  WALL)))


(define (render-maze m0)
  (local [(define SIZE (sqrt (length m0)))
          (define MTS (empty-scene (* SIZE CELL-SIZE) (* SIZE CELL-SIZE)))
          (define (render-maze m s)
            (cond [(empty? m) MTS]
                  [else
                   (place-image/align
                    (element-to-image (first m))
                    (* (modulo (- (length m0) (length m)) s) CELL-SIZE)
                    (* (quotient (- (length m0) (length m)) s) CELL-SIZE)
                    "left" "top"
                    (render-maze (rest m) SIZE))]))]
    
    (render-maze m0 SIZE)))


;; String -> Image
;; produce the correct image given the element's value
(check-expect (element-to-image W) WALL)
(check-expect (element-to-image B) BLANK)
(check-expect (element-to-image G) GOAL)

(define (element-to-image s)
  (cond [(string=? s W) WALL]
        [(string=? s B) BLANK]
        [(string=? s G) GOAL]))

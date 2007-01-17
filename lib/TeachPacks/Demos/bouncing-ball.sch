(require "TeachPacks/world")

;; mzscheme compatibility
(define symbol=? eq?)

;; DATA:

;; World = (make-world Number UpDown)
;; UpDown is one of: 
;; -- 'up 
;; -- 'down 
;; interpretation: the number represents the y-coordinate of the ball, 
;;   the dir field says in which dir it is moving 

(define-struct world (y dir))

(define SIZE 100)
(define WORLD0 (empty-scene SIZE SIZE))

;; data example 
(define FIRST-WORLD (make-world 0 'down))

;; FUNCTIONS

;; World -> World 
;; create the next world, assuming the ball moves 
;; 3 pixels up or down per clock tick 
(define (next w)
  (cond
    ((< (world-y w) 0)
     (make-world 0  (dir-reverse (world-dir w))))
    ((<= 0 (world-y w) SIZE)
     (make-world (ball-move (world-y w) (world-dir w)) (world-dir w)))
    ((< SIZE (world-y w))
     (make-world SIZE (dir-reverse (world-dir w))))))

;; UpDown -> UpDown 
;; what is the opposite of the given direction 
(define (dir-reverse d)
  (cond
    ((symbol=? d 'up) 'down)
    ((symbol=? d 'down) 'up)))

;; Number UpDown -> Number 
;; move the ball in the appropriate direction 
(define (ball-move y dir)
  (+ y (cond 
         ((symbol=? dir 'up) -3)
         ((symbol=? dir 'down) +3))))

;; World -> Image 
;; create an image from the given world 
(define (image w)
  (place-image (circle 3 'solid 'red) (/ SIZE 2) (world-y w) WORLD0))

;; RUN PROGRAM RUN 
(on-redraw image)
(on-tick-event next)
(big-bang SIZE SIZE .1 FIRST-WORLD)



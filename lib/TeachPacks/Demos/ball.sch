(require "TeachPacks/world")

(define demos-dir
  (string-append (current-larceny-root) "/lib/TeachPacks/Demos"))

(define ball-file
  (string-append demos-dir "/ball.png"))

;; World = Number 
;; interpretation: where is the ball (its current y coordinate)

(define SIZE 300)
(define BALL (image-from-file ball-file))
(define WORLD0 (empty-scene SIZE SIZE))

;; drop the ball by 3 pixels, unless it has reached the bottom
;; then just stop the clock 
(define (next y)
  (cond
    ((= y SIZE) (end-of-time "The ball has landed."))
    (else (+ y 3))))

;; place the image of the ball into the scene at (100,y)
(define (new-image y) (place-image BALL (/ SIZE 2) y WORLD0))

;; --- run program run
(on-redraw new-image)
(on-tick-event next)
(big-bang SIZE SIZE .1 0)

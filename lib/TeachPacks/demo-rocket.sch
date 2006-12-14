;; load world definitions
(define teachpack-dir "/lib/teachpacks/")
(load (string-append (getenv "LARCENY_ROOT") teachpack-dir "world-dotnet.sch"))

;; WORLD is one of: 
;; -- Number 
;; -- false 
;; interpretation: how long since the rocket has been launched 
;;  the clock start ticking when the player presses any key 

(define ROCKET (image-from-file "rocket-s.jpg"))
(define SIZE 300)
(define WORLD0 (empty-scene SIZE SIZE))

;; World -> World 

;; if the clock has started ticking, add 1 to the ticks 
(define (next t)
  (cond
    ((number? t) (+ t 1))
    ((boolean? t) t)))

;; World -> World 
;; if the player presses any key, the clock has ticked 0 times 
(define (launch ke t)
  0)

;; World -> Image 
;; place the rocket into the picture 
(define (new-image t)
  (place-image ROCKET (/ SIZE 2) (y-coordinate t) WORLD0))

;; World -> Number 
;; what is the y coordinate of the ROCKET image at this time
(define (y-coordinate t)
  (cond
    ((number? t) (- SIZE 10 t))
    (else (- SIZE 10))))

;; --- RUN PROGRAM RUN
(on-key-event launch)
(on-tick-event next)
(on-redraw new-image)
(big-bang SIZE SIZE .1 false)
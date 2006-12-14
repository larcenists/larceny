;; load world definitions
(define teachpack-dir "/lib/teachpacks/")
(load (string-append (getenv "LARCENY_ROOT") teachpack-dir "world-dotnet.sch"))

;; DATA
;; 
;; A World is one of: 
;;  -- Number
;;  -- (make-world Number Number)

;; Intepretation: 
;;  -- a plain Number: the x-coordinate of the satellite
;;  -- a world struct: the x-coordinate of the satellite
;;        and the y-coordinate of the rocket

(define-struct world (sat rock))

;; dimensions: 
(define WIDTH  200)
(define HEIGHT 600)

(define WORLD0 (empty-scene WIDTH HEIGHT))

(define SATELLITE (image-from-file "satellite.png"))
(define ROCKET (image-from-file "rocket-s.jpg"))

;; where the satellite starts, where the rocket starts 
(define X0 (quotient WIDTH 2))
(define Y0 (- HEIGHT (image-height ROCKET)))

;; FUNCTIONS

;; World -> World 
;; compute the next world 
(define (next w)
  (cond
    ((number? w) (satellite-next w))
    (else
     (make-world (satellite-next (world-sat w))
                 (rocket-next (world-rock w))))))
    
;; World -> Image 
;; create an image that represents the world 
(define (image w)
  (cond
    ((number? w)
     (satellite-add w (rocket-add Y0 WORLD0)))
    (else 
     (satellite-add 
       (world-sat w)
       (rocket-add (world-rock w) WORLD0)))))

;; Number -> Number 
;; move the rocket for this tick 
(define (rocket-next w) 
  (- w 2))

;; Number Image -> Image 
;; add the satellite to the image
(define (rocket-add w scene)
  (place-image ROCKET (/ WIDTH 2) w scene))

;; Number -> Number 
;; where is the satellite now?
(define (satellite-next w) 
  (modulo (+ 1 w) WIDTH))

;; Number Image -> Image 
;; add the satellite to the image
(define (satellite-add w scene)
  (place-image SATELLITE w 10 scene))

;; World KeyEvent -> World
;; react to a keyevent
(define (launch w ke) 
  (cond
    ((symbol? ke) w) ;; ignore all symbolic events 
    ((char? ke) (cond
                  ((world? w) w)
                  (else (make-world w Y0))))))

;; RUN PROGRAM RUN 
(on-tick-event next)
(on-redraw image)
(on-key-event launch)
(big-bang WIDTH HEIGHT .1 0)
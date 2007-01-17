;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THIS FILE SHOULD NOT BE LOADED DIRECTLY
;; INSTEAD, LOAD THE IMPLEMENTATION-SPECIFIC FILE
;; e.g. world-dotnet.sch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Implement argument checking similar to that in image.sch

;; This is a port of the PLT HTDP world teachpack.
;; The methods that users are intended to "see" follow. All other methods 
;; should be left unexposed.
;;   big-bang	
;;   end-of-time	  
;;   nw:rectangle 
;;   place-image  
;;   empty-scene  
;;   run-movie        -- not yet implemented
;;   add-line

;; Note: add-line is named add-line-to-scene. When a 'package' or 'module' like
;; system is implemented, add-line-to-scene should be renamed to add-line,
;; hiding add-line provided by the image teachpack.

;; Additionally, the following higher-order-primitives should be exposed.
;;   on-tick-event
;;   on-redraw 
;;   on-key-event 
;;   on-mouse-event   -- not yet implemented

;; this file requires the record-types of srfi-9
(require 'srfi-9)

(require "TeachPacks/image")
(require "TeachPacks/Support/world-dotnet")

;; timer
(define-record-type :timer
  (timer rep)
  timer?
  (rep timer-rep))

;; type-safe constructor for timer
(define (make-timer rep)
  (if (not (timer-rep? rep))
      (error 'make-timer ": invalid timer representation")
      (timer rep)))

;; frame
(define-record-type :frame
  (frame rep timer)
  frame?
  (rep frame-rep)
  (timer frame-timer))

;; type-safe constructor for frame
(define (make-frame rep t)
  (cond 
    ((not (frame-rep? rep))
     (error 'make-frame ": invalid frame representation"))
    ((not (timer? t))
     (error 'make-frame ": bad timer"))
    (else (frame rep t))))

;; -----------------------------------------------------------------------------
;; IMPLEMENTATION
;; The following functions need to be implemented to fit the host environment,
;; e.g. Microsoft .NET Framework. Redefine impl-X procedures when writing the 
;; implementation.

;; timer-rep? : Any -> boolean
(define timer-rep? (lambda (rep) (impl-timer-rep? rep)))

;; timer-stert : timer ->
(define timer-start (lambda (t) (impl-timer-start t)))

;; timer-stop : timer ->
(define timer-stop (lambda (t) (impl-timer-stop t)))

;; set-timer-interval! timer int ->
;; sets the timer's interval in place
(define set-timer-interval! (lambda (t i) (impl-set-timer-interval! t i)))
 
;; frame-rep? : Any -> boolean
(define frame-rep? (lambda (rep) (impl-frame-rep? rep)))

;; new-frame : int int -> frame
;; creates a new w x h frame, and a timer associated with the frame. 
;; When the timer ticks, it should invoke the timer-callback method (see
;; World Procedures below). When the frame closes, it should stop its 
;; associated timer. Also, the frame should handle key events and pass 
;; key-codes along to on-char-proc (see World Procedures below).
(define new-frame (lambda (w h) (impl-new-frame w h)))

;; update-frame : frame image -> 
;; updates the frame to display the given image
(define update-frame (lambda (f i) (impl-update-frame f i)))

;; show-frame : frame ->
;; shows the given frame
(define show-frame (lambda (f) (impl-show-frame f)))

;; -----------------------------------------------------------------------------
;; World Procedures
(define (nw:rectangle width height mode color)
  (move-pinhole (rectangle width height mode color) (/ width -2) (/ height -2)))

(define (place-image image x y scene)
  (let* ((sw (image-width scene))
         (sh (image-height scene))
         (ns (overlay/xy scene x y image))
         (nw (image-width ns))
         (nh (image-height ns)))
    (if (and (= sw nw) (= sh nh)) 
        ns
        (shrink ns 0 0 (- sw 1) (- sh 1)))))

(define (add-line-to-scene image x0 y0 x1 y1 color)
  (let* ((sw (image-width image))
         (sh (image-height image))
         (ns (add-line image x0 y0 x1 y1 color))
         (nw (image-width ns))
         (nh (image-height ns)))
    (if (and (= sw nw) (= sh nh)) 
        ns
        (shrink ns 0 0 (- sw 1) (- sh 1)))))

(define (empty-scene width height)
  (move-pinhole 
   (rectangle width height 'outline 'black)
   (/ width -2) (/ height -2)))

;; FIXME: Implement this procedure
(define (run-movie movie) 
  (error 'run-movie ": not implemented"))

;; -----------------------------------------------------------------------------
;; The One and Only Visible World
(define the-world0 (cons 1 1))
(define the-delta 1000)
(define the-world the-world0)
(define the-frame #f)

;; big-bang should always be called LAST in a file running a world simluation.
;; This means that on-key-event, on-tick-event, and on-redraw need to
;; be evaluted before calling big-bang, otherwise their intended effects 
;; will not be observed in the simulation.
(define (big-bang w h delta world)
  (if the-frame (error 'big-bang "big-bang already called once"))
  (set! the-delta delta)
  (set! the-world world)
  (set! the-frame (new-frame w h))  
  (let* ((w (ceiling (* 1000 the-delta)))
         (w-exact (if (exact? w) w (inexact->exact w))))
    (set-timer-interval! (frame-timer the-frame) w-exact))
  (timer-start (frame-timer the-frame))
  (show-frame the-frame)
  #t)

(define (end-of-time s)
  (display s)
  (timer-stop (frame-timer the-frame))
  the-world)

;; Time ------------------------------------------------------------------------

;; (World -> World)
(define timer-callback unspecified)

(define (on-tick-event f)
  ;; FIXME : add checks
  (if (eq? timer-callback unspecified)
      (set! timer-callback
            (lambda () 
              ;; FIXME : call with exception handlers
              (set! the-world (f the-world))
              (on-redraw-proc)))
      (error 'on-tick ": the timing action has already been set"))
  #t)

;; Drawing ---------------------------------------------------------------------

(define on-redraw-proc unspecified)

(define (on-redraw f)
  ;; FIXME : add checks
  (if (eq? on-redraw-proc unspecified)
      (set! on-redraw-proc
            (lambda ()
              ;; FIXME : call with exception handlers
              (let ((img (f the-world)))
                ;; FIXME : check img
                (update-frame the-frame img))))
      (error 'on-redraw ": the redraw function has already been specified"))
  #t)

;; Key Handling ----------------------------------------------------------------

(define on-char-proc unspecified)

(define (on-key-event f)
  ;; FIXME : add checks, eventspace equivalents?
  (if (eq? on-char-proc unspecified)
      (set! on-char-proc
            (lambda (e)
              ;; FIXME : parameterize event-space (?)
              ;;         call with exception handlers
              (set! the-world (f the-world e))
              (on-redraw-proc)))
      (error 'on-event ": the event action has already been set"))
  #t)

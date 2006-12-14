;; A basic one-line graphic REPL (if you want to call it that)
;; Call (gui-repl) to launch. Enter an expression into the text box, 
;; press Evaluate and ...
;; if the result is an image, it will be drawn on-screen, otherwise
;; you will not notice anything happening (but the expression will
;; still be evaluated).

;; If the expression errors, then you will be transferred back to the
;; standard CommonLarceny REPL, and you will see the error. To get back
;; to the dialog box, type (exit). You will be faced with an error
;; message that says a SchemeExitException occurred. Hit Continue, and
;; you are back into the GUI-REPL.
;; ... Of course, all of the above quirky behavior should be fixed.

;; provide image/world functions
(define teachpack-dir "/lib/teachpacks/")
(load (string-append (getenv "LARCENY_ROOT") teachpack-dir "world-dotnet.sch"))

;; methods
(define (set-width! c w)
  (clr/%property-set! clr/control-width c (clr/int->foreign w) (vector)))
(define (set-height! c h)
  (clr/%property-set! clr/control-height c (clr/int->foreign h) (vector)))
(define (set-top! c t)
  (clr/%property-set! clr/control-top c (clr/int->foreign t) (vector)))
(define (set-left! c l)
  (clr/%property-set! clr/control-left c (clr/int->foreign l) (vector)))
(define (set-text! c t)
  (clr/%property-set! clr/control-text c (clr/string->foreign t) (vector)))
(define (get-text c)
  (clr/foreign->string (clr/%property-ref clr/control-text c (vector))))
(define (add-controls! c loc)
  (if (not (null? loc))
      (begin 
        (clr/%invoke clr/control-collection-add 
                     (clr/%property-ref clr/control-controls c (vector))
                     (vector (car loc)))
        (add-controls! c (cdr loc)))))
(define (show-form f)
  (clr/%invoke clr/form-show-dialog f (vector)))

;; the REPL
(define the-form (clr/%invoke-constructor clr/form-ctor (vector)))
(define the-textbox (clr/%invoke-constructor clr/textbox-ctor (vector)))
(define the-button (clr/%invoke-constructor clr/button-ctor (vector)))
(define the-bitmap #f)

(set-width! the-form 400)
(set-height! the-form 400)
(set-text! the-form "REPL")
(set-width! the-textbox 290)
(set-top! the-textbox 10)
(set-left! the-textbox 10)
(set-width! the-button 70)
(set-top! the-button 10)
(set-left! the-button 310)
(set-text! the-button "Evaluate")
(add-controls! the-form (list the-textbox the-button))

(define (paint-handler sender args)
  (let ((the-gfx (clr/%property-ref clr/paint-event-args-graphics 
                                      args 
                                      (vector))))
    (clr/%invoke clr/gfx-clear the-gfx (vector (make-dotnet-color "White")))
    (if the-bitmap
        (clr/%invoke clr/gfx-draw-image-with-size
                     the-gfx
                     (vector the-bitmap 
                             (clr/int->foreign 10)
                             (clr/int->foreign 40)
                             (clr/int->foreign (dotnet-image-width the-bitmap))
                             (clr/int->foreign
                              (dotnet-image-height the-bitmap)))))))

(clr/%add-event-handler the-form "Paint" (clr/%foreign-box paint-handler))

(define (click-handler sender args)
  (let* ((expr (get-text the-textbox))
         (result (eval (read (open-input-string expr)))))
     ;;(set-text! the-textbox "")
    (if (image? result)
        (set! the-bitmap (bitmap-rep (:image-bitmap result))))
    (clr/%invoke clr/form-refresh the-form (vector))))

(clr/%add-event-handler the-button "Click" (clr/%foreign-box click-handler))

(define (gui-repl) (show-form the-form))
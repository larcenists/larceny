;; helper functions
(define (find-clr-color name)
  (let ((name (if (symbol? name) (symbol->string name) name)))
    (clr/%invoke clr/color-from-name #f (vector (clr/string->foreign name)))))

(define (clr/add-event-handler! publisher event procedure)
  (let ((boxed-proc (clr/%foreign-box procedure)))
    (clr/%add-event-handler publisher event boxed-proc)))

;; System.String
(define clr/string-type (find-clr-type "System.String"))

;; System.Windows.Forms.Form
(define clr/form-type (find-clr-type "System.Windows.Forms.Form"))
(define clr/form-ctor (clr/%get-constructor clr/form-type (vector)))
(define clr/form-back-color (clr/%get-property clr/form-type "BackColor" (vector)))
(define clr/form-show-dialog
  (clr/%get-method clr/form-type "ShowDialog" (vector)))
(define clr/form-refresh
  (clr/%get-method clr/form-type "Refresh" (vector)))

;; System.Windows.Forms.Timer
(define clr/timer-type (find-clr-type "System.Windows.Forms.Timer"))
(define clr/timer-ctor (clr/%get-constructor clr/timer-type (vector)))
(define clr/timer-start (clr/%get-method clr/timer-type "Start" (vector)))
(define clr/timer-stop (clr/%get-method clr/timer-type "Stop" (vector)))

;; System.Drawing.Color
(define clr/color-type (find-clr-type "System.Drawing.Color"))
(define clr/color-from-name 
  (clr/%get-method clr/color-type "FromName" (vector clr/string-type)))
(define clr/color-white (find-clr-color "White"))
(define clr/color-black (find-clr-color "Black"))

;; System.Drawing.Brush
(define clr/brush-type (find-clr-type "System.Drawing.Brush"))
(define clr/solid-brush-type (find-clr-type "System.Drawing.SolidBrush"))
(define clr/solid-brush-ctor 
  (clr/%get-constructor clr/solid-brush-type (vector clr/color-type)))

;; System.Int32
(define clr/int32-type (find-clr-type "System.Int32"))

;; System.Drawing.Graphics
(define clr/gfx-type (find-clr-type "System.Drawing.Graphics"))
(define clr/gfx-clear (clr/%get-method clr/gfx-type "Clear" (vector clr/color-type)))
(define clr/gfx-fill-rectangle
  (clr/%get-method 
    clr/gfx-type 
    "FillRectangle" 
    (vector clr/brush-type 
       clr/int32-type 
       clr/int32-type 
       clr/int32-type 
       clr/int32-type)))

;; System.Windows.Forms.PaintEventArgs
(define clr/paint-event-args-type 
  (find-clr-type "System.Windows.Forms.PaintEventArgs"))
(define clr/paint-event-args-gfx
  (clr/%get-property clr/paint-event-args-type "Graphics" (vector)))

;; the demo
(define (run-demo)
  (let ((the-timer (clr/%invoke-constructor clr/timer-ctor (vector)))
        (the-form  (clr/%invoke-constructor clr/form-ctor (vector)))
        (the-brush (clr/%invoke-constructor clr/solid-brush-ctor 
                     (vector clr/color-black))))
    (clr/%property-set! clr/form-back-color the-form clr/color-white (vector))
    (clr/add-event-handler! the-form "Paint"
      (lambda (sender paint-args)
        (let ((the-gfx  
                (clr/%property-ref clr/paint-event-args-gfx paint-args (vector))))
          (clr/%invoke clr/gfx-clear the-gfx (vector clr/color-white))
          (clr/%invoke clr/gfx-fill-rectangle the-gfx
            (vector the-brush 
              (clr/int->foreign (random 400))
              (clr/int->foreign (random 400))
              (clr/int->foreign 50)
              (clr/int->foreign 50))))))
    (clr/add-event-handler! the-form "VisibleChanged"
      (lambda (sender args)
        (clr/%invoke clr/timer-start the-timer (vector))))
    (clr/add-event-handler! the-timer "Tick"
      (lambda (sender args)
        (clr/%invoke clr/form-refresh the-form (vector))))
    (clr/%invoke clr/form-show-dialog the-form (vector))))

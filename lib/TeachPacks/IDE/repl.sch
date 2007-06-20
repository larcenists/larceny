;; methods

(require "TeachPacks/Support/dotnet-defs")

(define (make-setter property . conversion)
  (let ((conv (cond ((null? conversion) clr/int->foreign)
                    (else (car conversion)))))
    (lambda (c v)
      (clr/%property-set! property c (conv v) (vector)))))

(define (make-getter property . conversion)
  (let ((conv (cond ((null? conversion) clr/foreign->int)
                    (else (car conversion)))))
    (lambda (c)
      (conv (clr/%property-ref property c (vector))))))

(define set-width!  (make-setter clr/control-width))
(define set-height! (make-setter clr/control-height))
(define set-top!    (make-setter clr/control-top))
(define set-left!   (make-setter clr/control-left))
(define set-text!   (make-setter clr/control-text clr/string->foreign))

(define get-text    (make-getter clr/control-text clr/foreign->string))
(define get-controls (make-getter clr/control-controls identity))

(define (add-controls! c loc)
  (for-each (lambda (each)
              (clr/%invoke clr/control-collection-add
                           (get-controls c) (vector each)))
            loc))

(define (show-form f)
  (clr/%invoke clr/form-show-dialog f (vector)))

;; the REPL
(define (gui-repl)
  (define the-form (clr/%invoke-constructor clr/form-ctor (vector)))
  (define the-textbox (clr/%invoke-constructor clr/textbox-ctor (vector)))
  (define the-result (clr/%invoke-constructor clr/textbox-ctor (vector)))
  (define the-button (clr/%invoke-constructor clr/button-ctor (vector)))

  (define (click-handler sender args)
    (let* ((expr (get-text the-textbox))
           (result (eval (read (open-input-string expr))))
           (out (open-output-string)))
      (write result out)
      (set-text! the-textbox "")
      (set-text! the-result (get-output-string out))))

  (set-width! the-form 400)
  (set-height! the-form 400)
  (set-text! the-form "Steal This REPL")
  (set-width! the-textbox 290)
  (set-top! the-textbox 10)
  (set-left! the-textbox 10)
  (set-width! the-button 70)
  (set-top! the-button 10)
  (set-left! the-button 310)
  (set-top! the-result 50)
  (set-left! the-result 10)
  (set-width! the-result 380)
  (set-text! the-button "Evaluate")
  (add-controls! the-form (list the-textbox the-button the-result))

  (clr/%add-event-handler the-button "Click"
                          (clr/%foreign-box click-handler))

  (show-form the-form))


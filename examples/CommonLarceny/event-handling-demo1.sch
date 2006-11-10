(define form-type                (find-clr-type "System.Windows.Forms.Form"))
(define form-controls-property   (clr/%get-property form-type "Controls" (vector)))
(define control-type             (find-clr-type "System.Windows.Forms.Control"))
(define controls-collection-type (find-clr-type "System.Windows.Forms.Control+ControlCollection"))
(define button-type              (find-clr-type "System.Windows.Forms.Button"))
(define button-text-property     (clr/%get-property button-type "Text" (vector)))
(define button-top-property      (clr/%get-property button-type "Top" (vector)))
(define label-type               (find-clr-type "System.Windows.Forms.Label"))
(define label-text-property      (clr/%get-property label-type "Text" (vector)))
(define message-box-type         (find-clr-type "System.Windows.Forms.MessageBox"))
(define form1                    (clr/%invoke-constructor (clr/%get-constructor form-type (vector)) (vector)))
(define form1-controls           (clr/%property-ref form-controls-property form1 (vector)))
(define button1                  (clr/%invoke-constructor (clr/%get-constructor button-type (vector)) (vector)))
(define label1                   (clr/%invoke-constructor (clr/%get-constructor label-type (vector)) (vector)))

(clr/%property-set! button-text-property button1 (clr/string->foreign "Click Me") (vector))
(clr/%property-set! button-top-property button1 (clr/int->foreign 50) (vector))
(clr/%property-set! label-text-property label1 (clr/string->foreign "no click received") (vector))

(define (button1-click sender args)
  (clr/%property-set! label-text-property label1 (clr/string->foreign "Click Received Successfully!") (vector)))

(define (add-controls collection controls)
  (for-each (lambda (c)
             (clr/%invoke 
               (clr/%get-method controls-collection-type "Add" (vector control-type))
               collection 
               (vector c)))
    controls))

(add-controls form1-controls (list label1 button1))
  
(define (add-event-handler publisher event-name procedure) 
  (clr/%add-event-handler publisher event-name (clr/%foreign-box procedure)))

(add-event-handler button1 "Click" button1-click)

(define (demo-event-handling)
  (clr/%invoke (clr/%get-method form-type "ShowDialog" (vector)) form1 (vector)))
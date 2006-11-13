(define (add-event-handler publisher event-name procedure) 
  (clr/%add-event-handler (clr-object/clr-handle publisher) event-name (clr/%foreign-box procedure)))

(enable-dotnet!)

(display "Defining Button...")  
(define button1 (System.Windows.Forms.Button.))

(display "Defining Label...")
(define label1 (System.Windows.Forms.Label.))

(display "Defining Form...")
(define form1 (System.Windows.Forms.Form.))

(display "Defining Timer...")
(define countdown-timer (System.Windows.Forms.Timer.))

(define *initial-time-remaining* 10)
(define time-remaining *initial-time-remaining*)
(define (reset-time-remaining!) (set! time-remaining *initial-time-remaining*))
(define (update-label) (set-.Text$! label1 (number->string time-remaining)))

(define (start-timer)
  (.Start countdown-timer)
  (set-.Text$! button1 "Stop Timer"))

(define (stop-timer)
  (.Stop countdown-timer)
  (set-.Text$! button1 "Start Timer")
  (reset-time-remaining!)
  (update-label))

(define (decrement-timer)
  (set! time-remaining (- time-remaining 1))
  (update-label))

(define (countdown-expire)
  (stop-timer)
  (System.Windows.Forms.MessageBox.Show "Time expired!"))

(define (init-components) 
  (set-.Top$! button1 50)
  (set-.Interval$! countdown-timer 1000)
  (.AddRange (.Controls$ form1) (vector label1 button1))
  (add-event-handler button1 "Click" 
    (lambda (sender args) 
      (if (.Enabled$ countdown-timer) (stop-timer) (start-timer))))
  (add-event-handler countdown-timer "Tick" 
    (lambda (sender args)
      (if (> time-remaining 0) (decrement-timer) (countdown-expire))))
  (stop-timer))

(define (demo-event-handling)
  (init-components)
  (.ShowDialog form1))
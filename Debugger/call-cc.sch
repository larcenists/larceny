; Proof-of-concept code; do not toss.
; 9 June 1999 / lth

; This file defines an experimental implementation of the CONTINUATION-EXPLODE
; procedure.  CONTINUATION-EXPLODE takes a continuation and returns a list
; of full-fledged continuations: one for each frame hidden behind the
; original continuation.
;
; Doing this right is more involved than it would appear at first glance,
; largely because of DYNAMIC-WIND.

; MAL code goes in MALCODE.MAL.  The rest goes in CONTROL.SCH.
; The only newly exported name is CONTINUATION-EXPLODE; add it to the
; top-level environment.

; CALL-WITH-CURRENT-CONTINUATION and APPLY-CONTINUATION are written in MAL
; because they manipulate the stack frames under the feet of the compiler;
; this has bitten us in the past. 

`((,$lambda ((,$.proc)
             (,$args=   1)                   ; p
             (,$op1     creg)
             (,$setreg  2)
             (,$global  call-with-packaged-continuation)
             (,$invoke  2))
            0
            #(call-with-current-continuation #f 1))
  (,$setglbl call-with-current-continuation)
  (,$const call-with-current-continuation)
  (,$return))

`((,$lambda ((,$.proc)
             (,$args=  2)                    ; k, results
             (,$reg    1) 
             (,$op1    creg-set!)
             (,$global values)
             (,$setreg 1)
             (,$global apply)
             (,$invoke 2))                ; (apply values results)
            0
            #(apply-continuation #f 2))
  (,$setglbl apply-continuation)
  (,$const apply-continuation)
  (,$return))

; SYS$PACKAGE-CONTINUATION: package up continuation data into a procedure.
;
; It is written in MAL so we know the offsets of the data:
;   1 -- k
;   2 -- dynamic-wind-state
;
; This is really a special case of APPLYHOOK, and we should finally integrate
; the code for APPLYHOOK and use that.

`((,$lambda ((,$.proc)
             (,$args=  3)                      ; k dynamic-wind-state p
             (,$lambda ((,$.proc)
                        (,$args>=  0)          ; results
                        (,$lexical 0 3)        ; p
                        (,$invoke  1))         ; (p results)
                       2
                       #(continuation-package #f 0.0))
             (,$return))
            0
            #(sys$package-continuation #f 3))
  (,$setglbl sys$package-continuation)
  (,$const sys$package-continuation)
  (,$return))


; Now we can write accessors in Scheme.

(define (sys$continuation-k c) (procedure-ref c 3))
(define (sys$continuation-dynamic-wind-state c) (procedure-ref c 4))


; Package continuation and dynamic-wind state into a procedure that
; receives the results and applies the continuation to the results.
;
; Could've written this all in MAL but I want to keep as much as
; possible in Scheme for the sake of transparency.

(define (package-continuation k dynamic-wind-state)
  (sys$package-continuation k
                            dynamic-wind-state
                            (lambda (results)
                              (reroot! dynamic-wind-state)
                              (apply-continuation k results))))

; Called by CALL-WITH-CURRENT-CONTINUATION to call the client procedure
; P with a packaged continuation.

(define (call-with-packaged-continuation p k)
  (p (package-continuation k *here*)))

(define continuation? 
  (let ((cont-code (procedure-ref (package-continuation #f #f) 0)))
    (lambda (obj)
      (and (procedure? obj)
           (eq? (procedure-ref obj 0) cont-code)))))

(define (continuation-explode c)
  (if (not (continuation? c))
      (error "Continuation-explode: " c " is not a continuation."))
  (package-frames-individually (sys$continuation-k c)))


; K is a continuation structure.  The DYNAMIC-WIND-STATE must be the one 
; that was in effect when the continuation was captured.
;
; The deep problem here is to recover the dynamic-wind-state in effect
; at every frame.  To do so, we walk the list of frames and and the
; dynamic-wind-state.  The dynamic-wind-state changes only inside
; DYNAMIC-WIND.  We recognize a dynamic occurence of DYNAMIC-WIND because
; it has been modified to leave a mark on the stack...  It's a hack,
; but it ought to work.

(define (package-frames-individually k dynamic-wind-state)
  
  (define frame:dynamic 2)
  (define frame:reg0 3)
  
  (define (continuation->list k)
    (if (not k)
        '()
        (cons k (continuation->list (vector-ref c frame:dynamic)))))
  
  (define (package-frames c d)
    (cond ((null? c) '())
          ((null? d) 
           (error "Package-frames-individually is broken."))
          ((dynamic-wind-marker? (vector-ref c frame:reg0))
           (cons (package-continuation c d)
                 (package-frames (cdr c) (cdr d))))
          (else
           (cons (package-continuation c d)
                 (package-frames (cdr c) d)))))
  
  (package-frames (continuation->list k) dynamic-wind-state))

(define (dynamic-wind-marker p)
  (lambda ()
    (call-with-values p
      (lambda results
        (apply values result)))))

(define dynamic-wind-marker?
  (let ((dwm-code (procedure-ref (dynamic-wind-marker #f) 0)))
    (lambda (p)
      (and (procedure? p)
           (eq? (procedure-ref p 0) dwm-code)))))

(define *here* (list #f))                    ; DYNAMIC-WIND state

(define (dynamic-wind before during after)
  (let ((here *here*))
    (reroot! (cons (cons before after) here))
    (call-with-values 
      (dynamic-wind-marker during)
      (lambda results
        (reroot! here)
        (apply values results)))))

(define (reroot! there)  ; not modified

  (define (reroot-loop there)
    (if (not (eq? *here* there))
        (begin (reroot-loop (cdr there))
               (let ((before (caar there))
                     (after  (cdar there)))
                 (set-car! *here* (cons after before))
                 (set-cdr! *here* there)
                 (set-car! there #f)
                 (set-cdr! there '())
                 (set! *here* there)
                 (before)))))

  (let ((ticks (disable-interrupts)))
    (reroot-loop there)
    (if ticks (enable-interrupts ticks))))


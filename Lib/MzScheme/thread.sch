;; $Id$

;; Basics
;; ------

;; thread : (-> values) -> thread

;; thread/suspend-to-kill : (-> values) -> thread

;; thread-suspend : thread -> void
(define (thread-suspend thread)
  (let ((custodian (thread-custodian thread)))
    (unless (custodian-below custodian (current-custodian))
      (raise-contract-error 
       'thread-suspend "thread not managed by current custodian")))
  (set-thread-state! 'suspended))

;; thread-resume : thread [thread|custodian] -> void
(define (thread-kill thread)
  (unless (custodian-below (thread-custodian thread) (current-custodian))
    (raise-contract-error
     'thread-kill "thread not managed by current custodian"))
  (set-thread-state 'dead))

;; Synchronization
;; ---------------

;; thread-wait : thread -> void

;; thread-dead-evt : thread -> evt

;; thread-resume-evt : thread -> evt

;; thread-suspend-evt : thread -> evt

;; Utilities
;; ---------

;; current-thread : -> procedure
;; thread? : value -> boolean
;; sleep : [number] -> void
;; thread-running? : thread -> boolean
;; thread-dead? : thread -> boolean
;; break-thread : thread -> void
;; call-in-nested-thread : (-> values) [custodian] -> values

;; Semaphores
;; ----------

;; make-semaphore : [integer] -> semaphore
;; semaphore? : value -> boolean
;; semaphore-post : semaphore -> void
;; semaphore-wait : semaphore -> void
;; semaphore-try-wait : semaphore -> void
;; semaphore-wait/enable-break : semaphore -> void
;; semaphore-peek-evt : semaphore -> evt
;; call-with-semaphore : semaphore (-> values) [(-> values)] -> values
;; call-with-semaphore/enable-break : semaphore (-> values) -> values

;; Channels
;; --------

;; make-channel : -> channel
;; channel? : value -> boolean
;; channel-get : channel -> value
;; channel-try-get : channel -> value
;; channel-put : channel value -> void
;; channel-put-evt : channel value -> evt

;; Alarms
;; ------

;; alarm-evt : number -> evt

;; Sync
;; ----

;; sync : evt ...1 -> evt-result
(define (sync . evts)
  (apply sync/timeout #f evts))

;; sync/timeout : number|#f evt ...1 -> evt-result

;; See documentation for lists of primitive evt types

;; always-evt : evt
;; never-evt : evt

;; sync/enable-break : evt ...1 -> evt-result
;; sync/timeout/enable-break : number evt ...1 -> evt-result

;; Implementation
;; --------------

(define-struct thread (next prev                        ;; threads
                       tset-parent tset-next tset-prev  ;; thread-sets

                       current-continuation
                       
                       config        ;; Scheme_Config
                       engine-weight ;; long

                       running?      ;; boolean
                       suspended-box ;; contains thread when it's suspended
                       resumed-box   ;; contains thread when it's resumed
                       dead-box      ;; contains non-zero when thread is dead
                       running-box   ;; contains thread when it's running
                       
                       nester nestee ;; threads
                       
                       sleep-time            ;; double
                       block-descriptor      ;; int
                       blocker               ;; value
                       block-check           ;; function, Scheme_Ready_Fun
                       block-needs-wakeup    ;; function Scheme_Needs_Wakeup_Fun
                       
                       ran-some?             ;; boolean
                       suspend-to-kill?      ;; boolean
                       
                       ;; When transforming...
                       ;; Why aren't these parameters?
                       current-local-env     ;; Scheme_Comp_Env
                       current-local-mark    ;; value
                       current-local-name    ;; value
                       
                       ;; Exception stuff
                       exn-raised            ;; boolean
                       error-invoked         ;; boolean
                       err-val-str-invoked   ;; boolean
                       
                       suspend-break         ;; short (why?)
                       external-break        ;; short (why?)
                       
                       user-tls              ;; void** w/ count
                       custodian             ;; custodian
                       extra-custodians      ;; (listof custodian)
                       
                       name                  ;; value
                       ))

(define-struct config (table            ;; hashtable
                       ;; Some copy-on-write stuff, omitted for now.
                       ))

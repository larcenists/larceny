;; Configuration File: Options and Constants

;; -------
;; OPTIONS
;; -------

;; Configuration System
(define codegen-option-table '())

;; set-codegen-option! : key [value]
(define (set-codegen-option! k . v)
  (let ((v (if (null? v) #t (car v))))
    (set! codegen-option-table (cons (cons k v) codegen-option-table))))

;; codegen-option : key [value] -> value | #f
(define (codegen-option k . v)
  (cond ((and (null? v) (assoc k codegen-option-table)) => cdr)
        ((and (pair? v) (member (cons k (car v)) codegen-option-table))
         => cdr)
        (else #f)))

;; Inlining of Instructions
(for-each set-codegen-option!
          (list $const
                $reg $setreg
                $movereg $nop 
                $invoke
                $lexical $setlex
                $global $setglbl
                $args= 
                $trap
                ; $save $pop
                $stack $setstk
                $load $store 
                $setrtn
                $return

                ;; $apply ; Maybe buggy, no real point in inlining
                ;; $args>= ; INCOMPLETE
                ))

;; Code Generation

;; 'ilasm-debug
;;   if set, passes the debug switch to ilasm: results in debuggable
;;   IL, but slower code
(set-codegen-option! 'ilasm-debug)

;; 'new-operations
;;   if set, uses visitor-like operations built into 
;;   SObject. Otherwise, uses ops implemented in Ops, OpsSpecial.
(set-codegen-option! 'new-operations)

;; 'insert-use-fuel
;;   if set, decrements and checks the timer on every backwards branch 
;;   and procedure invocation
(set-codegen-option! 'insert-use-fuel)

;; 'direct-tail-calls
;;   if set, uses tail calls for scheme-to-scheme calls; otherwise, 
;;   uses Call.call for trampoline bounce
(set-codegen-option! 'direct-tail-calls)

;; 'cache-result
;;   if set, uses a local variable to hold Result register, only writing
;;   to global static field when necessary
;(set-codegen-option! 'cache-result)

;; 'cache-constant-vector
;;   if set, uses a local variable to hold constant vector
;;   should speed up constant accesses, including globals
(set-codegen-option! 'cache-constant-vector)

;; 'fault-error-messages
;;   if set, passes human-readable error messages to fault method
;;   otherwise, just calls with exception code
;(set-codegen-option! 'fault-error-messages)

;; 'listify-debug-location
;;   if set, updates Reg.debug_location on entry to every instruction
;;   location = line number in .list file
;;   only makes sense if listify? also set
;(set-codegen-option! 'listify-debug-location)

;; 'listify-line-directive
;;   if set, generates line directives in IL file
;(set-codegen-option! 'listify-line-directive)

;; 'listify-write-list-file
;;   if set, generates a .list file with MAL instructions
;(set-codegen-option! 'listify-write-list-file)


;; ---------
;; CONSTANTS
;; ---------

;; ENV-REGISTER: currently executing procedure is in register 0
(define ENV-REGISTER 0)

;; THIS-ARG: 'this' is argument 0 in instance methods
(define THIS-ARG 0)

;; FIRST-LOCAL (possibly mutated below): 
;;   first local variable index allocated by allocate-local
(define FIRST-LOCAL 0)

;; -----------------------------
;; The following depend on the continuation representation. These are for ISH.cs

;; CONTINUATION-FRAME-SLOTS == Cont.NUM_SLOT_FIELDS from ISH.cs
(define CONTINUATION-FRAME-SLOTS 8)

;; -----------------------------
;; The following depend on the implementation of SchemeObjects. These 
;; constants should be adjusted whenever the corresponding constants change.

;; FIXNUM-POOL-MAX == SchemeFixnum.maxPreAlloc
(define FIXNUM-POOL-MAX 16000)

;; FIXNUM-POOL-MIN == (- SchemeFixnum.maxPreAlloc)
(define FIXNUM-POOL-MIN -16000)

;; CHAR-POOL-MIN
(define CHAR-POOL-MIN 0)
(define CHAR-POOL-MAX 255) ;; == (sub1 SChar.CHAR_COUNT)

;; -----------------------------

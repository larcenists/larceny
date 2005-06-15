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

;; Code Generation
;;
;; CLR Version
;;   Selects which version of the CLR that will be used for assembling
;;   the code.
(set-codegen-option! 'clr-1.1)
;(set-codegen-option! 'clr-2.0)

;; Inlining of Instructions

;; The CLR will inline static methods, so most of the instruction
;; inlining has no effect on performance and simply leads to code
;; bloat.

;;  $apply
;;    Apply is not inlined.  Not worth it because apply has to spread
;;    the argument list.

;;  $args=
;;    If set, inline argument count checking for fixed arity procedures.
(set-codegen-option! $args=)

;;  $args>=
;;    Currently not inlined.  This has to listify the rest args, so it
;;    probably isn't worth inlining anyway.

;;  $const
;;    If set, uses special instruction calls for first few constants
;;    in a procedure.  This has little effect on performance, but it
;;    reduces the size of image.
(set-codegen-option! 'special-const-instructions)

;;  $global
;;    Currently not inlined.  Globals are stored in the constants
;;    vector.  Inlining this has no effect on performance, but it
;;    makes the code larger.

;;  $invoke
;;    Setting this avoids looking up fixnums for the argument count on
;;    calls with a small number arguments, but otherwise it isn't much
;;    use.
(set-codegen-option! 'special-invoke-instructions)

;;  $lexical
;;    This should be set to speed up lexical variable fetches.
(set-codegen-option! $lexical)

;;  $load
;;    Setting this uses the inlined load instructions.  This is
;;    important for performance.
(set-codegen-option! 'special-load-instructions)

;;  $pop
;;    Set this to use special pop instructions.
(set-codegen-option! 'special-pop-instructions)

;;  $reg
;;    Set this to use special instructions for register fetches.
;;    A big performance improvement.
(set-codegen-option! 'special-reg-instructions)

;;  $return
;;    Not worth inlining.

;;  $save
;;    Set this to use specialized save instructions.
(set-codegen-option! 'special-save-instructions)

;;  $setglbl
;;    Currently not inlined.

;;  $setlex
;;    Never issued?

;;  $setreg
;;    Set this to use the specialized set register instructions.
;;    This is a big improvement.
(set-codegen-option! 'special-setreg-instructions)

;;  $setrtn
;;    Set this to inline setrtn instructions.
(set-codegen-option! $setrtn)

;;  $setstk
;;    Never issued?

;;  $stack
;;    Set this to use special instructions for retrieving the first
;;    few slots of the stack frame.
(set-codegen-option! 'special-stack-instructions)

;;  $store
;;    Set this to use special store instructions for the first few
;;    slots in the stack frame.
(set-codegen-option! 'special-store-instructions)

;;  $trap
;; Trap is so expensive anyway, there is no point in inlining

;; Code Generation

;; 'ilasm-debug
;;   if set, passes the debug switch to ilasm: results in debuggable
;;   IL, but slower code
;(set-codegen-option! 'ilasm-debug)

;; 'ilasm-opt
;;   if set, passes the optimize switch to ilasm
;;   can be used *with* ilasm-debug
(set-codegen-option! 'ilasm-opt)

;; 'new-operations
;;   if set, uses visitor-like operations built into
;;   SObject. Otherwise, uses ops implemented in Ops, OpsSpecial.
(set-codegen-option! 'new-operations)

;; 'insert-use-fuel
;;   if set, decrements and checks the timer on every backwards branch
;;   and procedure invocation
;; TURNED OFF:  No real point in polling because the runtime system
;; ensures safety at all times.
;(set-codegen-option! 'insert-use-fuel)

;; 'direct-tail-calls
;;   if set, uses tail calls for scheme-to-scheme calls; otherwise,
;;   uses Call.call for trampoline bounce
;; TURNED OFF:  This is slower than using the trampoline!
;(set-codegen-option! 'direct-tail-calls)

;; 'cache-result
;;   if set, uses a local variable to hold Result register, only writing
;;   to global static field when necessary
;;   TURNED OFF:  Not complete.
;(set-codegen-option! 'cache-result)

;; 'cache-constant-vector
;;   if set, uses a local variable to hold constant vector
;;   should speed up constant accesses, including globals
;;   TURNED OFF:  SEEMS TO BE BROKEN.
;(set-codegen-option! 'cache-constant-vector)

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
;; Depends on the number of special instructions
;; defined in Instructions.cs
(define SPECIAL-INSTRUCTION-LIMIT 8)

;; -----------------------------
;; The following depend on the implementation of SchemeObjects. These
;; constants should be adjusted whenever the corresponding constants change.

;; Fixnums from 0 below this limit have special names.
(define NAMED-FIXNUM-LIMIT 5)

;; FIXNUM-POOL-MAX == SchemeFixnum.maxPreAlloc
(define FIXNUM-POOL-MAX 32767)

;; FIXNUM-POOL-MIN == SchemeFixnum.minPreAlloc
(define FIXNUM-POOL-MIN -16384)

;; CHAR-POOL-MIN
(define CHAR-POOL-MIN 0)
(define CHAR-POOL-MAX 255) ;; == (sub1 SChar.CHAR_COUNT)

;; -----------------------------

; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$
;
; 9 November 1998

; Implements the following abstract data types.
;
; labels
;     (init-labels)
;     (make-label)
;     cg-label-counter
;
; assembly streams
;     (make-assembly-stream)
;     (assembly-stream-code as)
;     (gen! as . instruction)
;     (gen-instruction! as instruction)
;     (gen-save! as frame)
;     (gen-restore! as frame)
;     (gen-pop! as frame)
;     (gen-setstk! as frame v)
;     (gen-store! as frame r v)
;     (gen-load! as frame r v)
;     (gen-stack! as frame v)
;
; temporaries
;     (init-temps)
;     (newtemp)
;     (newtemps)
;     newtemp-counter
;
; register environments
;     (cgreg-initial)
;     (cgreg-copy regs)
;     (cgreg-tos regs)
;     (cgreg-liveregs regs)
;     (cgreg-live regs r)
;     (cgreg-vars regs)
;     (cgreg-bind! regs r v)
;     (cgreg-bindregs! regs vars)
;     (cgreg-rename! regs alist)
;     (cgreg-release! regs r)
;     (cgreg-clear! regs)
;     (cgreg-lookup regs var)
;     (cgreg-lookup-reg regs r)
;     (cgreg-join! regs1 regs2)
;
; stack frame environments
;     (cgframe-initial)
;     (cgframe-size-cell frame)
;     (cgframe-size frame)
;     (cgframe-copy frame)
;     (cgframe-join! frame1 frame2)
;     (cgframe-update-stale! frame)
;     (cgframe-used! frame)
;     (cgframe-bind! frame n v instruction)
;     (cgframe-touch! frame v)
;     (cgframe-rename! frame alist)
;     (cgframe-release! frame v)
;     (cgframe-lookup frame v)
;     (cgframe-spilled? frame v)
;
; environments
;     (entry.name entry)
;     (entry.kind entry)
;     (entry.rib entry)
;     (entry.offset entry)
;     (entry.label entry)
;     (entry.regnum entry)
;     (entry.arity entry)
;     (entry.op entry)
;     (entry.imm entry)
;     (cgenv-initial)
;     (cgenv-lookup env id)
;     (cgenv-extend env vars procs)
;     (cgenv-bindprocs env procs)
;     (var-lookup var regs frame env)

; Labels.

(define (init-labels)
  (set! cg-label-counter 1000))

(define (make-label)
  (set! cg-label-counter (+ cg-label-counter 1))
  cg-label-counter)

(define cg-label-counter 1000)

;    an assembly stream into which instructions should be emitted
;    an expression
;    the desired target register ('result, a register number, or '#f)
;    a register environment [cgreg]
;    a stack-frame environment [cgframe]
;      contains size of frame, current top of frame
;    a compile-time environment [cgenv]
;    a flag indicating whether the expression is in tail position

; Assembly streams, into which instructions are emitted by side effect.
; Represented as a pair whose car is a nonempty list whose cdr is a
; possibly empty list of MacScheme machine assembly instructions,
; and whose cdr is the last pair of the car.

(define (make-assembly-stream)
  (let ((stream (list (list 0))))
    (set-cdr! stream (car stream))
    stream))

(define (assembly-stream-code output)
  (if (local-optimizations)
      (filter-basic-blocks (cdar output))
      (cdar output)))

(define (gen! output . instruction)
  (gen-instruction! output instruction))

(define (gen-instruction! output instruction)
  (let ((pair (list instruction)))
    (set-cdr! (cdr output) pair)
    (set-cdr! output pair)
    output))

(define (gen-save! output frame t0)
  (let ((size (cgframe-size-cell frame)))
    (gen-instruction! output (cons $save size))
    (gen-store! output frame 0 t0)
    (cgframe:stale-set! frame '())))

(define (gen-restore! output frame)
  (let ((size (cgframe-size-cell frame)))
    (gen-instruction! output (cons $restore size))))

(define (gen-pop! output frame)
  (let ((size (cgframe-size-cell frame)))
    (gen-instruction! output (cons $pop size))))

(define (gen-setstk! output frame tempname)
  (let ((instruction (list $nop $setstk -1)))
    (cgframe-bind! frame tempname instruction)
    (gen-instruction! output instruction)))

(define (gen-store! output frame r tempname)
  (let ((instruction (list $nop $store r -1)))
    (cgframe-bind! frame tempname instruction)
    (gen-instruction! output instruction)))

(define (gen-load! output frame r tempname)
  (cgframe-touch! frame tempname)
  (let ((n (entry.slotnum (cgframe-lookup frame tempname))))
    (gen! output $load r n)))

(define (gen-stack! output frame tempname)
  (cgframe-touch! frame tempname)
  (let ((n (entry.slotnum (cgframe-lookup frame tempname))))
    (gen! output $stack n)))

; Returns a temporary name.
; Temporaries are compared using EQ?, so the use of small
; exact integers as temporary names is implementation-dependent.

(define (init-temps)
  (set! newtemp-counter 5000))

(define (newtemp)
  (set! newtemp-counter
        (+ newtemp-counter 1))
  newtemp-counter)

(define newtemp-counter 5000)

(define (newtemps n)
  (if (zero? n)
      '()
      (cons (newtemp)
            (newtemps (- n 1)))))

; New representation of
; Register environments.
; Represented as a pair whose car is one more than the highest
; index of a live register, and whose cdr is a mutable vector
; with *nregs* elements.
; The elements of the mutable vector are of the form
;    #f        (the register is dead)
;    #t        (the register is live)
;    v         (the register contains variable v)
;    t         (the register contains temporary variable t)

(define (cgreg-initial)
  (let ((v (make-vector *nregs* #f)))
    (cons 0 v)))

(define (cgreg-copy regs)
  (let* ((m (car regs))
         (v0 (cdr regs))
         (n (vector-length v0))
         (v (make-vector n #f)))
    (do ((i 0 (+ i 1)))
        ((= i n)
         (cons m v))
        (vector-set! v i (vector-ref v0 i)))))

(define (cgreg-tos regs)
  (- (car regs) 1))

(define (cgreg-liveregs regs)
  (car regs))

(define (cgreg-live regs r)
  (if (eq? r 'result)
      (cgreg-tos regs)
      (max r (cgreg-tos regs))))

(define (cgreg-vars regs)
  (let ((m (car regs))
        (v (cdr regs)))
    (do ((i (- m 1) (- i 1))
         (vars '()
               (cons (vector-ref v i)
                     vars)))
        ((< i 0)
         vars))))

(define (cgreg-bind! regs r t)
  (let ((m (car regs))
        (v (cdr regs)))
    (vector-set! v r t)
    (if (>= r m)
        (set-car! regs (+ r 1)))))

(define (cgreg-bindregs! regs vars)
  (do ((m (car regs) (+ m 1))
       (v (cdr regs))
       (vars vars (cdr vars)))
      ((null? vars)
       (set-car! regs m)
       regs)
      (vector-set! v m (car vars))))

(define (cgreg-rename! regs alist)
  (do ((i (- (car regs) 1) (- i 1))
       (v (cdr regs)))
      ((negative? i))
      (let ((var (vector-ref v i)))
        (if var
            (let ((probe (assv var alist)))
              (if probe
                  (vector-set! v i (cdr probe))))))))

(define (cgreg-release! regs r)
  (let ((m (car regs))
        (v (cdr regs)))
    (vector-set! v r #f)
    (if (= r (- m 1))
        (do ((m r (- m 1)))
            ((or (negative? m)
                 (vector-ref v m))
             (set-car! regs (+ m 1)))))))

(define (cgreg-release-except! regs vars)
  (do ((i (- (car regs) 1) (- i 1))
       (v (cdr regs)))
      ((negative? i))
      (let ((var (vector-ref v i)))
        (if (and var (not (memq var vars)))
            (cgreg-release! regs i)))))

(define (cgreg-clear! regs)
  (let ((m (car regs))
        (v (cdr regs)))
    (do ((r 0 (+ r 1)))
        ((= r m)
         (set-car! regs 0))
        (vector-set! v r #f))))

(define (cgreg-lookup regs var)
  (let ((m (car regs))
        (v (cdr regs)))
    (define (loop i)
      (cond ((< i 0)
             #f)
            ((eq? var (vector-ref v i))
             (list var 'register i '(object)))
            (else
             (loop (- i 1)))))
    (loop (- m 1))))

(define (cgreg-lookup-reg regs r)
  (let ((m (car regs))
        (v (cdr regs)))
    (if (<= m r)
        #f
        (vector-ref v r))))

(define (cgreg-join! regs1 regs2)
  (let ((m1 (car regs1))
        (m2 (car regs2))
        (v1 (cdr regs1))
        (v2 (cdr regs2)))
    (do ((i (- (max m1 m2) 1) (- i 1)))
        ((< i 0)
         (set-car! regs1 (min m1 m2)))
        (let ((x1 (vector-ref v1 i))
              (x2 (vector-ref v2 i)))
          (cond ((eq? x1 x2)
                 #t)
                ((not x1)
                 #t)
                (else
                 (vector-set! v1 i #f)))))))

; New representation of
; Stack-frame environments.
; Represented as a three-element list.
;
; Its car is a list whose car is a list of slot entries, each
; of the form
;    (v n instruction stale)
; where
;    v is the name of a variable or temporary,
;    n is #f or a slot number,
;    instruction is a possibly phantom store or setstk instruction
;       that stores v into slot n, and
;    stale is a list of stale slot entries, each of the form
;          (#t . n)
;       or (#f . -1)
;       where slot n had been allocated, initialized, and released
;       before the store or setstk instruction was generated.
; Slot entries are updated by side effect.
;
; Its cadr is the list of currently stale slots.
;
; Its caddr is the size of the stack frame, which can be
; increased but not decreased.  The cdr of the stack frame
; environment is shared with the save instruction that
; created the frame.  What a glorious crock!

; This stuff is private to the implementation of stack-frame
; environments.

(define cgframe:slots car)
(define cgframe:stale cadr)
(define cgframe:slot.name car)
(define cgframe:slot.offset cadr)
(define cgframe:slot.instruction caddr)
(define cgframe:slot.stale cadddr)

(define cgframe:slots-set! set-car!)
(define (cgframe:stale-set! frame stale)
  (set-car! (cdr frame) stale))

(define cgframe:slot.name-set! set-car!)

(define (cgframe:slot.offset-set! entry n)
  (let ((instruction (caddr entry)))
    (if (or (not (eq? #f (cadr entry)))
            (not (eq? $nop (car instruction))))
        (error "Compiler bug: cgframe" entry)
        (begin
         (set-car! (cdr entry) n)
         (set-car! instruction (cadr instruction))
         (set-cdr! instruction (cddr instruction))
         (if (eq? $setstk (car instruction))
             (set-car! (cdr instruction) n)
             (set-car! (cddr instruction) n))))))

; Reserves a slot offset that was unused where the instruction
; of the slot entry was generated, and returns that offset.

(define (cgframe:unused-slot frame entry)
  (let* ((stale (cgframe:slot.stale entry))
         (probe (assq #t stale)))
    (if probe
        (begin (set-car! probe #f)
               (cdr probe))
        (let* ((cell (cgframe-size-cell frame))
               (n (+ 1 (car cell))))
          (set-car! cell n)
          (if (zero? n)
              (cgframe:unused-slot frame entry)
              n)))))

; Public entry points.

; The runtime system requires slot 0 of a frame to contain
; a closure whose code pointer contains the return address
; of the frame.
; To prevent slot 0 from being used for some other purpose,
; we rely on a complex trick:  Slot 0 is initially stale.
; Gen-save! generates a store instruction for register 0,
; with slot 0 as the only stale slot for that instruction;
; then gen-save! clears the frame's set of stale slots, which
; prevents other store instructions from using slot 0.

(define (cgframe-initial)
  (list '()
        (list (cons #t 0))
        -1))

(define (cgframe-size-cell frame)
  (cddr frame))

(define (cgframe-size frame)
  (car (cgframe-size-cell frame)))

(define (cgframe-used! frame)
  (if (negative? (cgframe-size frame))
      (set-car! (cgframe-size-cell frame) 0)))

; Called only by gen-store!, gen-setstk!

(define (cgframe-bind! frame var instruction)
  (set-car! frame
            (cons (list var #f instruction (cgframe:stale frame))
                  (cgframe:slots frame))))

; Called only by gen-load!, gen-stack!

(define (cgframe-touch! frame var)
  (let ((entry (assq var (cgframe:slots frame))))
    (if entry
        (let ((n (cgframe:slot.offset entry)))
          (if (eq? #f n)
              (let ((n (cgframe:unused-slot frame entry)))
                (cgframe:slot.offset-set! entry n))))
        (error "Compiler bug: cgframe-touch!" frame var))))

(define (cgframe-rename! frame alist)
  (for-each (lambda (entry)
              (let ((probe (assq (cgframe:slot.name entry) alist)))
                (if probe
                    (cgframe:slot.name-set! entry (cdr probe)))))
            (cgframe:slots frame)))

(define (cgframe-release! frame var)
  (let* ((slots (cgframe:slots frame))
         (entry (assq var slots)))
    (if entry
        (begin (cgframe:slots-set! frame (remq entry slots))
               (let ((n (cgframe:slot.offset entry)))
                 (if (and (not (eq? #f n))
                          (not (zero? n)))
                     (cgframe:stale-set!
                      frame
                      (cons (cons #t n)
                            (cgframe:stale frame)))))))))

(define (cgframe-release-except! frame vars)
  (let loop ((slots (reverse (cgframe:slots frame)))
             (newslots '())
             (stale (cgframe:stale frame)))
    (if (null? slots)
        (begin (cgframe:slots-set! frame newslots)
               (cgframe:stale-set! frame stale))
        (let ((slot (car slots)))
          (if (memq (cgframe:slot.name slot) vars)
              (loop (cdr slots)
                    (cons slot newslots)
                    stale)
              (let ((n (cgframe:slot.offset slot)))
                (cond ((eq? n #f)
                       (loop (cdr slots)
                             newslots
                             stale))
                      ((zero? n)
                       (loop (cdr slots)
                             (cons slot newslots)
                             stale))
                      (else
                       (loop (cdr slots)
                             newslots
                             (cons (cons #t n) stale))))))))))

(define (cgframe-lookup frame var)
  (let ((entry (assq var (cgframe:slots frame))))
    (if entry
        (let ((n (cgframe:slot.offset entry)))
          (if (eq? #f n)
              (cgframe-touch! frame var))
          (list var 'frame (cgframe:slot.offset entry) '(object)))
        #f)))

(define (cgframe-spilled? frame var)
  (let ((entry (assq var (cgframe:slots frame))))
    (if entry
        (let ((n (cgframe:slot.offset entry)))
          (not (eq? #f n)))
        #f)))

; For a conditional expression, the then and else parts must be
; evaluated using separate copies of the frame environment,
; and those copies must be resolved at the join point.  The
; nature of the resolution depends upon whether the conditional
; expression is in a tail position.
;
; Critical invariant:
; Any store instructions that are generated within either arm of the
; conditional involve variables and temporaries that are local to the
; conditional.
;
; If the conditional expression is in a tail position, then a slot
; that is stale after the test can be allocated independently by the
; two arms of the conditional.  If the conditional expression is in a
; non-tail position, then the slot can be allocated independently
; provided it is not a candidate destination for any previous emitted
; store instruction.

(define (cgframe-copy frame)
  (cons (car frame)
        (cons (cadr frame)
              (cddr frame))))

(define (cgframe-update-stale! frame)
  (let* ((n (cgframe-size frame))
         (v (make-vector (+ 1 n) #t))
         (stale (cgframe:stale frame)))
    (for-each (lambda (x)
                (if (car x)
                    (let ((i (cdr x)))
                      (if (<= i n)
                          (vector-set! v i #f)))))
              stale)
    (for-each (lambda (slot)
                (let ((offset (cgframe:slot.offset slot)))
                  (if offset
                      (vector-set! v offset #f)
                      (for-each (lambda (stale)
                                  (if (car stale)
                                      (let ((i (cdr stale)))
                                        (if (< i n)
                                            (vector-set! v i #f)))))
                                (cgframe:slot.stale slot)))))
              (cgframe:slots frame))
    (do ((i n (- i 1))
         (stale (filter car stale)
                (if (vector-ref v i)
                    (cons (cons #t i) stale)
                    stale)))
        ((<= i 0)
         (cgframe:stale-set! frame stale)))))

(define (cgframe-join! frame1 frame2)
  (let* ((slots1 (cgframe:slots frame1))
         (slots2 (cgframe:slots frame2))
         (slots (intersection slots1 slots2))
         (deadslots (append (difference slots1 slots)
                            (difference slots2 slots)))
         (deadoffsets (make-set
                       (filter (lambda (x) (not (eq? x #f)))
                               (map cgframe:slot.offset deadslots))))
         (stale1 (cgframe:stale frame1))
         (stale2 (cgframe:stale frame2))
         (stale (intersection stale1 stale2))
         (stale (append (map (lambda (n) (cons #t n))
                             deadoffsets)
                        stale)))
    (cgframe:slots-set! frame1 slots)
    (cgframe:stale-set! frame1 stale)))

; Environments.
;
; Each identifier has one of the following kinds of entry.
;
;    (<name> register   <number>                (object))
;    (<name> frame      <slot>                  (object))
;    (<name> lexical    <rib>    <offset>       (object))
;    (<name> procedure  <rib>    <label>        (object))
;    (<name> integrable <arity>  <op>     <imm> (object))
;    (<name> global                             (object))
;
; Implementation.
;
; An environment is represented as a list of the form
;
;    ((<entry> ...)                          ; lexical rib
;     ...)
;
; where each <entry> has one of the forms
;
;    (<name> lexical <offset> (object))
;    (<name> procedure <rib> <label> (object))
;    (<name> integrable <arity> <op> <imm> (object))

(define entry.name car)
(define entry.kind cadr)
(define entry.rib caddr)
(define entry.offset cadddr)
(define entry.label cadddr)
(define entry.regnum caddr)
(define entry.slotnum caddr)
(define entry.arity caddr)
(define entry.op cadddr)
(define (entry.imm entry) (car (cddddr entry)))

(define (cgenv-initial integrable)
  (list (map (lambda (x)
               (list (car x)
                     'integrable
                     (cadr x)
                     (caddr x)
                     (cadddr x)
                     '(object)))
             integrable)))

(define (cgenv-lookup env id)
  (define (loop ribs m)
    (if (null? ribs)
        (cons id '(global (object)))
        (let ((x (assq id (car ribs))))
          (if x
              (case (cadr x)
                ((lexical) (cons id
                                 (cons (cadr x)
                                       (cons m (cddr x)))))
                ((procedure) (cons id
                                   (cons (cadr x)
                                         (cons m (cddr x)))))
                ((integrable) x)
                (else ???))
              (loop (cdr ribs) (+ m 1))))))
  (loop env 0))

(define (cgenv-extend env vars procs)
  (cons (do ((n 0 (+ n 1))
             (vars vars (cdr vars))
             (rib (map (lambda (id)
                         (list id 'procedure (make-label) '(object)))
                       procs)
                  (cons (list (car vars) 'lexical n '(object)) rib)))
            ((null? vars) rib))
        env))

(define (cgenv-bindprocs env procs)
  (cons (append (map (lambda (id)
                       (list id 'procedure (make-label) '(object)))
                     procs)
                (car env))
        (cdr env)))

(define (var-lookup var regs frame env)
  (or (cgreg-lookup regs var)
      (cgframe-lookup frame var)
      (cgenv-lookup env var)))

; For testing.

(define foo
  (lambda (x)
    (pretty-print
     (pass4 (pass2 (pass1 x)) $usual-integrable-procedures$))))

(define compile
  (lambda (x)
    (pass4 (pass2 (pass1 x)) $usual-integrable-procedures$)))

; Find the smallest number of registers such that
; adding more registers does not affect the code
; generated for x (from 4 to 32 registers).

(define (minregs x)
  (define (defregs R)
    (set! *nregs* R)
    (set! *lastreg* (- *nregs* 1))
    (set! *fullregs* (quotient *nregs* 2)))
  (defregs 32)
  (let ((code (assemble (compile x))))
    (define (binary-search m1 m2)
      (if (= (+ m1 1) m2)
          m2
          (let ((midpt (quotient (+ m1 m2) 2)))
            (defregs midpt)
            (if (equal? code (assemble (compile x)))
                (binary-search m1 midpt)
                (binary-search midpt m2)))))
    (defregs 4)
    (let ((newcode (assemble (compile x))))
      (if (equal? code newcode)
          4
          (binary-search 4 32)))))

; Minimums:
;  browse     10
;  triangle    5
;  traverse   10
;  destruct    6
;  puzzle      8,8,10,7
;  tak         6
;  fft        28   (changing the named lets to macros didn't matter)

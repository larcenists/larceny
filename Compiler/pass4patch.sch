; Date: Tue, 22 Jul 1997 15:31:18 -0500
; From: William D Clinger <will@ccs.neu.edu>
; To: Lars Thomas Hansen <lth@ccs.neu.edu>
; CC: will@ccs.neu.edu
; Subject: Re: Compiler bug
; 
; The following patches are sufficient to fix the particular
; example you gave, and I think they are sufficient to fix the
; general problem as well.  I'll let you test them.
; 
; These patches will generate slightly slower code, but
; correct is better than fast.  To fix the problem without
; losing any efficiency would require examination of the
; entire code generator, which I won't be doing for a few
; weeks.
; 
; Will
; 
; ----

(define (cg-move output src dst)
  (cond ((not dst)
         ; Temporary bug fix:
         ; Instead of returning src, move to result.
         (cg-move output src 'result))
        ((eqv? src dst)
         dst)
        ((eq? src 'result)
         (gen! output $setreg dst)
         dst)
        ((eq? dst 'result)
         (gen! output $reg src)
         dst)
        ((and (not (zero? src))
              (not (zero? dst)))
         (gen! output $movereg src dst))
        (else
         (gen! output $reg src)
         (gen! output $setreg dst)
         dst)))

(define (cg-variable output exp target regs frame env tail?)
  (define (return)
    (if tail?
        (begin (gen-pop! output frame)
               (gen! output $return)
               'result)
        (cg-move output 'result target)))
  (let* ((id (variable.name exp))
         (entry (var-lookup id regs frame env)))
    (case (entry.kind entry)
      ((global integrable)
       (gen! output $global id)
       (return))
      ((lexical)
       (gen! output $lexical (entry.rib entry) (entry.offset entry) id)
       (return))
      ((procedure) (error "Bug in cg-variable" exp))
      ((register)
       (let ((r (entry.regnum entry)))
         (if (and target (not (eqv? target r)))
             (begin (gen! output $reg (entry.regnum entry) id)
                    (return))
             ; was just r
             (cg-move output r target))))
      (else (error "Bug in cg-variable" exp)))))

; eof

; Auxlib/optimize-level.sch
;
; $Id: optimize-level.sch,v 1.1.1.1 1998/11/19 21:52:18 lth Exp $
;
; Set compiler switches to useful defaults and then define the system
; parameter 'optimize-level'.

(include-source-code #t)		; Debugging is good
(include-variable-names #t)		; Debugging is good
(generate-global-symbols #t)		; (say it again) Debugging is good
(issue-warnings #f)			; Most warnings are annoying

(define *optimize-level* 'unknown)	; Initialized below

(define (optimize-level . rest)
  (if (null? rest)
      *optimize-level*
      (case (car rest)
	((0) (benchmark-mode #f)
	     (catch-undefined-globals #t)
	     (fill-delay-slots #f)
	     (inline-assignment #f)
	     (inline-allocation #f)
	     (integrate-usual-procedures #f)
	     (local-optimizations #f)
	     (peephole-optimization #f)
	     (unsafe-code #f)
	     ; (lambda-lifting #f)      ; FIXME -- this switch doesn't exist
	     (set! *optimize-level* (car rest)))
	((1) (benchmark-mode #f)
	     (catch-undefined-globals #t)
	     (fill-delay-slots #t)
	     (inline-assignment #t)
	     (inline-allocation #t)
	     (integrate-usual-procedures #f)
	     (local-optimizations #t)
	     (peephole-optimization #t)
	     (unsafe-code #f)
	     ; (lambda-lifting #f)      ; FIXME -- this switch doesn't exist
	     (set! *optimize-level* (car rest)))
	((2) (benchmark-mode #t)
	     (catch-undefined-globals #t)
	     (fill-delay-slots #t)
	     (inline-assignment #t)
	     (inline-allocation #t)
	     (integrate-usual-procedures #t)
	     (local-optimizations #t)
	     (peephole-optimization #t)
	     (unsafe-code #f)
	     ; (lambda-lifting #t)      ; FIXME -- this switch doesn't exist
	     (set! *optimize-level* (car rest)))
	((3) (benchmark-mode #t)
	     (catch-undefined-globals #f)
	     (fill-delay-slots #t)
	     (inline-assignment #t)
	     (inline-allocation #t)
	     (integrate-usual-procedures #t)
	     (local-optimizations #t)
	     (peephole-optimization #t)
	     (unsafe-code #t)
	     ; (lambda-lifting #t)      ; FIXME -- this switch doesn't exist
	     (set! *optimize-level* (car rest)))
	(else
	 (error "Invalid optimization level " (car rest))))))

(optimize-level 2)

; eof

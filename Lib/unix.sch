; Lib/unix.sch
; Larceny library -- Some Unix primitives
;
; $Id: unix.sch,v 1.10 1997/08/22 21:05:14 lth Exp $

; Parameters for unix:open; the magic numbers are portable.

(define unix:open-read   #x01)        ; open for read
(define unix:open-write  #x02)        ; open for write
(define unix:open-append #x04)        ; position at end (writing)
(define unix:open-create #x08)        ; create if not existing (writing)
(define unix:open-trunc  #x10)        ; truncate if existing (writing)
(define unix:create-mode #o666)       ; default mode for new files

; Parameters for unix:access; the magic numbers are portable.

(define unix:access-exists  #x01)     ; path searchable, file exists
(define unix:access-read    #x02)     ; file readable
(define unix:access-write   #x04)     ; file writable
(define unix:access-execute #x08)     ; file executable

; Standard Unix file descriptors

(define unix:stdin  0)
(define unix:stdout 1)
(define unix:stderr 2)

; Syscalls
;
; FIXME: these values belong in a configuration file.
; NOTE: Values correspond to the table ordering in Rts/Sys/cglue.c.

(define syscall:open 0)
(define syscall:unlink 1)
(define syscall:close 2)
(define syscall:read 3)
(define syscall:write 4)
(define syscall:get-resource-usage 5)
(define syscall:dump-heap 6)
(define syscall:exit 7)
(define syscall:mtime 8)
(define syscall:access 9)
(define syscall:rename 10)
(define syscall:pollinput 11)
(define syscall:getenv 12)
(define syscall:gc 13)
(define syscall:flonum-log 14)
(define syscall:flonum-exp 15)
(define syscall:flonum-sin 16)
(define syscall:flonum-cos 17)
(define syscall:flonum-tan 18)
(define syscall:flonum-asin 19)
(define syscall:flonum-acos 20)
(define syscall:flonum-atan 21)
(define syscall:flonum-atan2 22)
(define syscall:flonum-sqrt 23)
(define syscall:stats-dump-on 24)
(define syscall:stats-dump-off 25)
(define syscall:iflush 26)
(define syscall:gcctl 27)
(define syscall:block-signals 28)
(define syscall:flonum-sinh 29)
(define syscall:flonum-cosh 30)
(define syscall:system 31)

; Syscall wrappers.

; Returns file descriptor; -1 on error.

(define (unix:open filename flags mode)
  (syscall syscall:open filename flags mode))

; Returns 0 on success; -1 on error.

(define (unix:close fd)
  (syscall syscall:close fd))

; Returns number of bytes actually read; 0 on eof; -1 on error.

(define (unix:read fd buffer nbytes)
  (syscall syscall:read fd buffer nbytes))

; Returns number of bytes actually written; -1 on error.

(define (unix:write fd buffer nbytes offset)
  (syscall syscall:write fd buffer nbytes offset))

; Returns 0 on success, -1 on error.

(define (unix:unlink filename)
  (syscall syscall:unlink filename))

; Doesn't return.

(define (unix:exit code)
  (syscall syscall:exit code))

; Returns #t (ready) or #f (not ready).

(define (unix:pollinput fd)
  (= (syscall syscall:pollinput fd) 1))

; Returns the exit status from wait(); see system(3).

(define (unix:system commandline)
  (syscall syscall:system commandline))

; Get resource usage data into given vector.
; Returns nothing.

(define (unix:get-resource-usage)
  (syscall syscall:get-resource-usage))

; Turn on and off GC statistics dumping to a file.

(define (unix:stats-dump-on filename)
  (if (string? filename)
      (syscall syscall:stats-dump-on filename)
      -1))

(define (unix:stats-dump-off)
  (syscall syscall:stats-dump-off))

; Dump a heap image to the given filename, with the given startup procedure.

(define (unix:dump-heap fn proc)
  (syscall syscall:dump-heap fn proc))

; Returns a vector of length 6: #(yr month day hr min sec), or #f on failure.

(define (unix:file-modification-time filename)
  (let ((v (make-vector 6)))
    (if (zero? (syscall syscall:mtime filename v))
	v
	#f)))

; Returns #t if file exists, #f if not.

(define (unix:access filename mode)
  (zero? (syscall syscall:access filename mode)))

; Rename the old filename to the new filename; return #t if it worked
; and #f if it didn't.

(define (unix:rename old new)
  (zero? (syscall syscall:rename old new)))

; Get the value of an environment variable, and return string or #f.

(define (unix:getenv str)
  (syscall syscall:getenv str))

; 'Gen' is the generation; 'type' is 0 (collect) or 1 (promote).

(define (unix:gc gen type)
  (syscall syscall:gc gen type))

; Instruction cache flushing.  The argument is a byte vector.

(define (unix:iflush bv)
  (syscall syscall:iflush bv))

; Transcendentals and square root are supported by callouts to C, for now.

(define (unix:flonum-log x)
  (syscall syscall:flonum-log x (make-raw-flonum)))

(define (unix:flonum-exp x)
  (syscall syscall:flonum-exp x (make-raw-flonum)))

(define (unix:flonum-sin x)
  (syscall syscall:flonum-sin x (make-raw-flonum)))

(define (unix:flonum-cos x)
  (syscall syscall:flonum-cos x (make-raw-flonum)))

(define (unix:flonum-tan x)
  (syscall syscall:flonum-tan x (make-raw-flonum)))

(define (unix:flonum-asin x)
  (syscall syscall:flonum-asin x (make-raw-flonum)))

(define (unix:flonum-acos x)
  (syscall syscall:flonum-acos x (make-raw-flonum)))

(define (unix:flonum-atan x)
  (syscall syscall:flonum-atan x (make-raw-flonum)))

(define (unix:flonum-atan2 x y)
  (syscall syscall:flonum-atan2 x y (make-raw-flonum)))

(define (unix:flonum-sqrt x)
  (syscall syscall:flonum-sqrt x (make-raw-flonum)))

(define (unix:flonum-sinh x)
  (syscall syscall:flonum-sinh x (make-raw-flonum)))

(define (unix:flonum-cosh x)
  (syscall syscall:flonum-cosh x (make-raw-flonum)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; High-level interface
;

; Support for the portable I/O system.
;
; In all cases, a filename is a string, a file descriptor is an integer, 
; 'mode' is a symbol, either input or output, and a buffer is a 
; bytevector-like structure.
;
; Rule: Opening a file which already exits for output will cause the old file
; to be silently truncated.

; Open the 'terminal' and return file descriptors for input and output
; on it.
;
; FIXME: must be more sophisticated and reopen the terminal after EOF.

(define (sys$open-terminal mode)
  (cond ((eq? mode 'input)  unix:stdin)
	((eq? mode 'output) unix:stdout)
	(else (error "sys$open-terminal: invalid mode: " mode)
	      #t)))

; FIXME: should actually close it when asked to do so.

(define (sys$close-terminal fd)
  (cond ((equal? fd unix:stdin) 0)
	((equal? fd unix:stdout) 0)
	(else (error "sys$close-terminal: invalid descriptor: " fd)
	      #t)))

(define (sys$open-file fn mode)
  (cond ((eq? mode 'input)
	 (unix:open fn unix:open-read 0))
	((eq? mode 'output)
	 (unix:open fn 
		    (+ unix:open-write unix:open-create unix:open-trunc)
		    unix:create-mode))
	(else
	 ???open-file)))

(define sys$get-resource-usage unix:get-resource-usage)
(define sys$close-file unix:close)
(define sys$delete-file unix:unlink)
(define sys$read-file unix:read)
(define (sys$write-file fd buf k) (unix:write fd buf k 0))
(define sys$write-file4 unix:write)
(define sys$rename-file unix:rename)
(define sys$file-modification-time unix:file-modification-time)
(define (sys$file-exists? fn) (unix:access fn unix:access-exists))
(define sys$char-ready? unix:pollinput)

(define sys$gc unix:gc)
(define sys$codevector-iflush unix:iflush)

(define (sys$gcctl heap rator rand)
  (if (not (and (fixnum? heap) (fixnum? rator) (fixnum? rand)))
      (error "sys$gcctl: bogus: " heap " " rator " " rand))
  (syscall syscall:gcctl heap rator rand)
  (unspecified))

; GC statistics dumping

(define (stats-dump-on fn)
  (if (< (unix:stats-dump-on fn) 0)
      (error "stats-dump-on: I/O error."))
  (unspecified))

(define (stats-dump-off)
  (if (< (unix:stats-dump-off) 0)
      (error "stats-dump-off: I/O error."))
  (unspecified))

(define sys$dump-heap unix:dump-heap)
(define sys$exit unix:exit)

; Get the value of an environment variable.

(define (getenv name)
  (cond ((symbol? name)
	 (unix:getenv (symbol->string name)))
	((string? name)
	 (unix:getenv name))
	(else
	 (error "getenv: not a valid name: " name)
	 #t)))

; Numbers

(define flonum:sin unix:flonum-sin)
(define flonum:cos unix:flonum-cos)
(define flonum:tan unix:flonum-tan)
(define flonum:asin unix:flonum-asin)
(define flonum:acos unix:flonum-acos)
(define flonum:atan unix:flonum-atan)
(define flonum:atan2 unix:flonum-atan2)
(define flonum:exp unix:flonum-exp)
(define flonum:log unix:flonum-log)
(define flonum:sqrt unix:flonum-sqrt)

; Subprocess

(define (system cmd)
  (if (not (string? cmd))
      (error "system: " cmd " is not a string.")
      (unix:system cmd)))

; System-dependent character values.

(define **newline** 10)


; RTS debugging utility function
;
; FIXME -- we're modifying a string constant.  The problem is that:
;  - can't use make-string, since it's written in Scheme and not
;    initialized yet.
;  - can't have a newline in a string constant, since we don't have
;    any syntax for that yet.

(define ($$debugmsg msg)
  (let ((nl " "))
    (string-set! nl 0 (integer->char **newline**))
    (sys$write-file4 1 msg (string-length msg) 0)
    (sys$write-file4 1 nl 1 0)))

(define $$trace $$debugmsg)

; For releases (and when we get tired of seeing all the startup msgs).

(define ($$trace msg)
  #f)

; Unix signal handling  --  prototype!
;
; We have a vector called pending-signals that counts the number of 
; signals of a particular type that has been received.  When 
; sys$get-pending-asynch-signal is called, it returns #f if no signals
; are pending or a pair (signum . count), where signum is the signal number
; and count is the number of signals received for that signal number.
;
; A signal handler written in C increments the counters.  The counters
; are fixnums.

(define pending-signals (make-vector 32 0))   ; counters

(define sys$get-pending-asynch-signal
  (let ((saved-i 0))    ; Round-robin search prevents starvation.

    (define (next i)
      (remainder (+ i 1) 32))

    (define (check i)
      (let ((v (vector-ref pending-signals i)))
	(if (> v 0)
	    (begin (vector-set! pending-signals i 0)
		   (cons i v))
	    #f)))

    (define (search k i)
      (cond ((= k 0) #f)
	    ((check i))
	    (else (search (- k 1) (next i)))))

    (lambda ()
      (syscall syscall:block-signals 1)
      (let ((r (search 32 saved-i)))
	(if r (set! saved-i (next (car r))))
	(syscall syscall:block-signals 0)
	r))))


; eof

; Lib/unix.sch
; Larceny library -- Some Unix primitives
;
; $Id$

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
(define syscall:C-ffi-apply 32)
(define syscall:C-ffi-dlopen 33)
(define syscall:C-ffi-dlsym 34)
(define syscall:make-nonrelocatable 35)
(define syscall:object->address 36)
(define syscall:ffi-getaddr 37)
(define syscall:sro 38)
(define syscall:sys-feature 39)
(define syscall:peek-bytes 40)
(define syscall:poke-bytes 41)

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

(define (unix:sro ptr hdr limit)
  (if (not (and (fixnum? ptr)
		(fixnum? hdr)
		(fixnum? limit)))
      (error "Bogus parameters to unix:sro " (list ptr hdr limit)))
  (syscall syscall:sro ptr hdr limit))

; Returns a resource usage vector.

(define (unix:get-resource-usage)
  (syscall syscall:get-resource-usage))

; Turn on and off GC statistics dumping to a file.

(define (unix:stats-dump-on filename)
  (syscall syscall:stats-dump-on filename))

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
; As a general rule, these procedures should check their argument to
; the extent necessary for the low-level procedures not to crash
; the system.

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
; The first time through, we use file descriptors inherited from the parent
; process.  Later, we open /dev/tty.

(define (sys$open-terminal mode firsttime?)
  (if (not (or (eq? mode 'input) (eq? mode 'output)))
      (begin (error "sys$open-terminal: invalid mode: " mode)
	     #t))
  (if firsttime?
      (case mode
	((input) unix:stdin)
	((output) unix:stdout))
      (case mode
	((input) (unix:open "/dev/tty" unix:open-read 0))
	((output) (unix:open "/dev/tty" unix:open-write unix:create-mode)))))

(define (sys$close-terminal fd)
  (sys$close-file fd))

(define (sys$open-file fn mode)
  (cond ((not (string? fn))
	 (error "sys$open-file: invalid filename " fn)
	 #t)
	((eq? mode 'input)
	 (unix:open fn unix:open-read 0))
	((eq? mode 'output)
	 (unix:open fn 
		    (+ unix:open-write unix:open-create unix:open-trunc)
		    unix:create-mode))
	(else
	 ???open-file)))

(define sys$get-resource-usage unix:get-resource-usage)

(define (sys$close-file fd)
  (if (not (fixnum? fd))
      (begin (error "sys$close-file: invalid file descriptor " fd)
	     #t)
      (unix:close fd)))

(define (sys$delete-file fn)
  (if (not (string? fn))
      (begin (error "sys$delete-file: invalid filename " fn)
	     #t)
      (unix:unlink fn)))

(define (sys$read-file fd buffer nbytes)
  (cond ((not (fixnum? fd))
	 (error "sys$read-file: invalid descriptor " fd) #t)
	((not (bytevector-like? buffer))
	 (error "sys$read-file: invalid buffer " buffer) #t)
	((not (and (fixnum? nbytes) (>= nbytes 0)))
	 (error "sys$read-file: invalid byte count " nbytes) #t)
	(else
	 (unix:read fd buffer nbytes))))

(define (sys$write-file fd buf k)
  (sys$write-file4 fd buf k 0))

(define (sys$write-file4 fd buf k offset)
  (cond ((not (fixnum? fd))
	 (error "sys$write-file4: invalid descriptor " fd) #t)
	((not (bytevector-like? buf))
	 (error "sys$write-file4: invalid buffer " buf) #t)
	((not (and (fixnum? k) (>= k 0)))
	 (error "sys$write-file4: invalid byte count " k) #t)
	((not (and (>= offset 0)
		   (<= (+ offset k) (bytevector-like-length buf))))
	 (error "sys$write-file4: invalid byte count or offset "
		k "/" offset) #t)
	(else
	 (unix:write fd buf k offset))))

(define (sys$rename-file old new)
  (cond ((not (string? old))
	 (error "sys$write-file4: bad file name " old) #t)
	((not (string? new))
	 (error "sys$write-file4: bad file name " new) #t)
	(else
	 (unix:rename old new))))

(define (sys$file-modification-time fn)
  (cond ((not (string? fn))
	 (error "sys$file-modification-time: bad file name " fn) #t)
	(else
	 (unix:file-modification-time fn))))

(define (sys$file-exists? fn)
  (cond ((not (string? fn))
	 (error "sys$file-exists?: bad file name " fn) #t)
	(else
	 (unix:access fn unix:access-exists))))

(define (sys$char-ready? fd)
  (cond ((not (fixnum? fd))
	 (error "sys$char-ready?: bad descriptor " fd) #t)
	(else
	 (unix:pollinput fd))))

(define (sys$gc gen type)
  (cond ((not (fixnum? gen))
	 (error "sys$gc: bad generation " gen) #t)
	((not (fixnum? type))
	 (error "sys$gc: bad type " type) #t)
	(else
	 (unix:gc gen type))))

(define (sys$codevector-iflush bv)
  (cond ((not (bytevector-like? bv))
	 (error "sys$codevector-iflush: not a bytevector-like: " bv) #t)
	(else 
	 (unix:iflush bv))))

(define (sys$gcctl heap rator rand)
  (if (not (and (fixnum? heap) (fixnum? rator) (fixnum? rand)))
      (error "sys$gcctl: bogus: " heap " " rator " " rand))
  (syscall syscall:gcctl heap rator rand)
  (unspecified))

; GC statistics dumping

(define (stats-dump-on fn)
  (cond ((not (string? fn))
	 (error "stats-dump-on: invalid filename " fn))
	((< (unix:stats-dump-on fn) 0)
	 (error "stats-dump-on: I/O error.")))
  (unspecified))

(define (stats-dump-off)
  (if (< (unix:stats-dump-off) 0)
      (error "stats-dump-off: I/O error."))
  (unspecified))

(define (sys$dump-heap fn proc)
  (cond ((not (string? fn))
	 (error "sys$dump-heap: bad file name " fn) #t)
	((not (procedure? proc))
	 (error "sys$dump-heap: not a procedure " proc) #t)
	(else
	 (unix:dump-heap fn proc))))

(define (sys$exit code)
  (cond ((not (fixnum? code))
	 (error "sys$exit: bad code " code)
	 (unix:exit 1))
	(else
	 (unix:exit code))))

(define feature-vector-length 5)
(define feature$larceny-major 0)
(define feature$larceny-minor 1)
(define feature$os-major      2)
(define feature$os-minor      3)
(define feature$gc-info       4)
(define feature$gen-info      5)

(define (sys$system-feature name)

  (define (sysfeature v)
    (syscall syscall:sys-feature v))

  (let ((v (make-vector feature-vector-length 0)))
    (case name
      ((larceny-major)
       (vector-set! v 0 feature$larceny-major)
       (sysfeature v)
       (vector-ref v 0))
      ((larceny-minor) 
       (vector-set! v 0 feature$larceny-minor)
       (sysfeature v)
       (vector-ref v 0))
      ((arch-name) "SPARC")
      ((endian) 'big)
      ((os-name)   "SunOS")
      ((os-major)
       (vector-set! v 0 feature$os-major)
       (sysfeature v)
       (vector-ref v 0))
      ((os-minor)
       (vector-set! v 0 feature$os-minor)
       (sysfeature v)
       (vector-ref v 0))
      ((gc-tech)
       (vector-set! v 0 feature$gc-info)
       (sysfeature v)
       (let* ((gc-type (vector-ref v 0))
	      (gc-gens (vector-ref v 1))
	      (g       (make-vector gc-gens #f)))
	 (do ((i 0 (+ i 1)))
	     ((= i gc-gens))
	   (vector-set! v 0 feature$gen-info)
	   (vector-set! v 1 (+ i 1))
	   (sysfeature v) 
	   (let* ((type (case (vector-ref v 0)
			  ((0) 'nursery)
			  ((1) 'two-space)
			  ((2) 'two-space-np-old)
			  ((3) 'two-space-np-young)
			  ((4) 'static)
			  ((5) 'conservative-mark-sweep)
			  (else 'unknown)))
		  (size (vector-ref v 1))
		  (params (case type
			    ((two-space-np-old two-space-np-young)
			     (list (cons 'k (vector-ref v 2))
				   (cons 'j (vector-ref v 3))))
			    ((two-space nursery static)
			     (if (zero? (vector-ref v 2))
				 'fixed
				 'expandable))
			    (else
			     #f))))
	     (vector-set! g i (vector i type size params))))
	 (cons (case gc-type
		 ((0) 'stop+copy)
		 ((1) 'generational)
		 ((2) 'conservative)
		 (else 'unknown))
	       g)))
      (else 
       (error "sys$system-feature: " name " is not a system feature name")))))

; Get the value of an environment variable.

(define (getenv name)
  (cond ((symbol? name)
	 (unix:getenv (symbol->string name)))
	((string? name)
	 (unix:getenv name))
	(else
	 (error "getenv: not a valid name: " name)
	 #t)))

; Numbers -- not error checked yet.  (FIXME?)

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

; SRO

(define sro unix:sro)

; Subprocess

(define (system cmd)
  (cond ((not (string? cmd))
	 (error "system: " cmd " is not a string.") #t)
	(else
	 (unix:system cmd))))

; System-dependent character values.

(define **newline** 10)
(define **space** 32)
(define **tab** 9)
(define **carriage-return** 13)
(define **linefeed** 10)
(define **form-feed** 12)
(define **backspace** 8)

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


; Foreign-function interface.

(define (sys$C-ffi-apply trampoline arg-encoding ret-encoding actuals)
  (syscall syscall:C-ffi-apply trampoline arg-encoding ret-encoding actuals))

(define (sys$C-ffi-dlopen path)
  (cond ((not (bytevector? path))	; 0-terminated bytevector
	 (error "sys$C-ffi-dlopen: bad path.") #t)
	(else
	 (syscall syscall:C-ffi-dlopen path))))

(define (sys$C-ffi-dlsym handle sym)
  (cond ((not (and (integer? handle) (exact? handle)))
	 (error "sys$C-ffi-dlsym: bad handle " handle) #t)
	((not (bytevector? sym))
	 (error "sys$C-ffi-dlsym: bad symbol " sym) #t)
	(else
	 (syscall syscall:C-ffi-dlsym handle sym))))

(define (peek-bytes addr bv count)
  (if (and (bytevector? bv)
	   (fixnum? count)
	   (<= count (bytevector-length bv))
	   (or (fixnum? addr)
	       (and (bignum? addr)
		    (<= 0 addr 4294967295))))
      (syscall syscall:peek-bytes addr bv count)
      (error "peek-bytes: invalid arguments " addr ", " bv ", " count)))

(define (poke-bytes addr bv count)
  (if (and (bytevector? bv)
	   (fixnum? count)
	   (<= count (bytevector-length bv))
	   (or (fixnum? addr)
	       (and (bignum? addr)
		    (<= 0 addr 4294967295))))
      (syscall syscall:poke-bytes addr bv count))
      (error "poke-bytes: invalid arguments " addr ", " bv ", " count))

; eof

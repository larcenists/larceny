; Larceny library -- Some Unix primitives
;
; $Id$
;
; History
;   July 15, 1995 / lth
;     Added some more syscalls: mtime, access, rename, pollinput.
;
;   June 27-July 1, 1995 / lth
;     Cleaned up and put into service.
;
;   July 19, 1994 / lth
;     Created

; Various UNIX I/O parameters. The values are taken from header files
; for SunOS 4.1.1; at some point we need to find a scheme for generating
; these values automatically (unless the values are POSIX-mandated).

; sys/fcntlcom.h

(define unix:O_RDONLY 0)             ; mode for opening a file for read
(define unix:O_WRONLY 1)             ; ditto for write
(define unix:O_RDWR   2)             ; ditto for both
(define unix:O_CREAT  #x200)         ; create file if !existing (output)
(define unix:O_TRUNC  #x400)         ; truncate if existing (output)
(define unix:create-mode #o666)      ; default mode for new files

; unistd.h

(define unix:R_OK 4)                 ; access for reading
(define unix:W_OK 2)                 ; access for writing
(define unix:X_OK 1)                 ; access for execution
(define unix:F_OK 0)                 ; path searchable, file exists

; Standard Unix file descriptors

(define unix:stdin 0)
(define unix:stdout 1)
(define unix:stderr 2)

; Syscalls
;
; They belong in a configuration file!!
; Values correspond to the table ordering in Rts/Sys/cglue.c.

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

; Wrappers

(define (unix:open filename flags mode)
  (syscall syscall:open filename flags mode))

(define (unix:close fd)
  (syscall syscall:close fd))

(define (unix:read fd buffer nbytes)
  (syscall syscall:read fd buffer nbytes))

(define (unix:write fd buffer nbytes)
  (syscall syscall:write fd buffer nbytes))

(define (unix:unlink filename)
  (syscall syscall:unlink filename))

(define (unix:exit)
  (syscall syscall:exit))

; Get resource usage data into given vector

(define (unix:get-resource-usage vec)
  (syscall syscall:get-resource-usage vec))

; Dump image to given filename, and with given startup proc.

(define (unix:dump-heap fn proc)
  (syscall syscall:dump-heap fn proc))

; Returns a vector of length 6, containing yr month day hr min sec.
; Return the vector, or #f if the call failed.

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

; Poll for input on the given file descriptor

(define (unix:pollinput fd)
  (= (syscall syscall:pollinput fd) 1))

; Get an environment variable, or #f

(define (unix:getenv str)
  (syscall syscall:getenv str))

; Garbage collection. Type is a numeric code defined in globals.cfg.

(define (unix:gc type)
  (syscall syscall:gc type))


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
; Rule: Opening a file which already exits for mail will cause the old file
; to be silently truncated.

; Open the 'terminal' and return file descriptors for input and output
; on it.

(define (sys$open-terminal mode)
  (cond ((eq? mode 'input)  unix:stdin)
	((eq? mode 'output) unix:stdout)
	(else ???open-terminal)))

(define (sys$open-file fn mode)
  (cond ((eq? mode 'input)
	 (unix:open fn unix:O_RDONLY 0))
	((eq? mode 'output)
	 (unix:open fn 
		    (+ unix:O_WRONLY unix:O_CREAT unix:O_TRUNC)
		    unix:create-mode))
	(else
	 ???open-file)))

(define sys$close-file unix:close)
(define sys$delete-file unix:unlink)
(define sys$read-file unix:read)
(define sys$write-file unix:write)
(define sys$rename-file unix:rename)
(define sys$file-modification-time unix:file-modification-time)
(define (sys$file-exists? fn) (unix:access fn unix:F_OK))
(define sys$char-ready? unix:pollinput)

(define sys$gc unix:gc)

; Dump a heap.

(define (dumpheap filename proc)
  (cond ((not (string? filename))
	 (error "Bad filename argument to dumpheap: " filename))
	((not (procedure? proc))
	 (error "Bad procedure argument to dumpheap: " proc))
	(else
	 (display "Dumping heap...") (newline)
	 (unix:dump-heap filename proc)
	 (display "Done.")
	 (newline))))

; Clean up and exit.

(define (exit)
  (close-open-files)
  (unix:exit))

; Get the value of an environment variable.

(define (getenv name)
  (cond ((symbol? name)
	 (unix:getenv (symbol->string name)))
	((string? name)
	 (unix:getenv name))
	(else
	 (error "getenv: not a valid name: " name))))


; eof

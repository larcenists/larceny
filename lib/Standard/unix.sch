; Unix functionality
; 2004-01-11 / lth
;
; Might split this file into several services, in particular, factor
; PROCESS out into a process-related package.
;
; WITH-WORKING-DIRECTORY is less complex than it used to be and of
; less utility; CURRENT-DIRECTORY is available generally now.

(require 'ffi)
(require 'string)
(require 'experimental/unix)
(require 'experimental/unix-descriptor)

; Does PATH denote a directory?

(define (directory-name? path)
  (let* ((s (unix/make-stat_t))         ; FIXME: make-stat_t etc. not defined
         (r (unix/stat path s)))
    (not (zero? (logand (stat_t-mode-ref s) unix/S_IFDIR)))))


; WITH-WORKING-DIRECTORY: Execute THUNK in the given DIR, and honor
; changes to the working directory inside THUNK.
;
; CD and PWD are defined in the extended image, should perhaps move
; them into this file?

(define (with-working-directory dir thunk)
  (parameterize ((current-directory dir))
    (thunk)))


; TEMPORARY-FILE-NAME: Generate a safe unique temporary file name.
;
; Note that at most TMP_MAX unique names are generated before tmpnam
; starts reusing them; on my current system that value is 26^3=17576 but
; it could be smaller, I suppose.  Applications that make serious use of
; temporary-file-name should be careful to keep a log of file names
; still in use, and check the value returned from this function against
; that log.

(define temporary-file-name
  (let ((unix:tmpnam (foreign-procedure "tmpnam" '(string) 'string)))
    (lambda ()
      (call-without-interrupts          ; tmpnam is not reentrant
        (lambda ()
          (unix:tmpnam #f))))))

; PROCESS: Create a subprocess and set up communication with it.
;
; Returns a list (input-port output-port process-id), where data written
; to output-port are available on standard input in the subprocess and
; data written on standard output in the subprocess are available on
; input-port.
;
; Compatible with Chez Scheme.  A modern version migh return multiple values.

(define (process command)
  (let-values (((r1 stdin-in stdin-out) (unix/pipe))
               ((r2 stdout-in stdout-out) (unix/pipe)))
    (let ((pid (unix/fork)))
      (case pid
        ((-1)
         (error "Failed to fork a subprocess."))
        ((0)
         (unix/close stdout-in)
         (unix/close stdin-out)
         (unix/dup2 stdin-in 0)
         (unix/dup2 stdout-out 1)
         (unix/execl "/bin/sh" "sh" "-c" command)
         ; FIXME: signal an error here, but can't use ERROR, I think.
         (unix/exit 1))
        (else 
         (unix/close stdin-in)
         (unix/close stdout-out)
         (list (open-input-descriptor stdout-in)
               (open-output-descriptor stdin-out)
               pid))))))

; eof

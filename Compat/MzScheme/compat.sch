;; Why not just require?
;; Well... that doesn't work on certain versions of DrScheme.
(namespace-require/copy '(lib "list.ss"))
(namespace-require/copy '(lib "etc.ss"))
(namespace-require/copy '(lib "process.ss"))
(namespace-require/copy '(prefix mz: mzscheme))

(define ($$trace x) #t)
(define host-system 'mzscheme)

;; Initialization

(define (compat:initialize)
  (let ((hostdir (nbuild-parameter 'compatibility)))
    (load (string-append hostdir "logops.ss"))
    (if (eq? 'little (nbuild-parameter 'endianness))
        (begin (load (string-append hostdir "bytevec-el.ss"))
               (load (string-append hostdir "misc2bytevector-el.ss")))
        (begin (load (string-append hostdir "bytevec.ss"))
               (load (string-append hostdir "misc2bytevector.ss"))))
    (print-vector-length #f)
    #t))

(define (compat:load filename)
  (define (loadit fn)
    (if (nbuild-parameter 'verbose-load?)
	(begin (display fn)
	       (newline)))
    (load fn))
  (loadit filename))

(define (call-with-error-control thunk1 thunk2) 
  (with-handlers [(values (lambda _ (thunk2)))]
    (thunk1)))

(define (call-with-error-handler handler thunk)
  (with-handlers [(values handler)]
    (thunk)))

(define (call-without-interrupts thunk)
  (thunk))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Larceny-specific objects

(define *undefined-expression* (letrec ((x x)) x))
(define *unspecified-expression* (void))
(define *eof-object* (read (open-input-string "")))

(define (unspecified) *unspecified-expression*)
(define (undefined) *undefined-expression*)
(define (eof-object) *eof-object*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; Common non-standard operations

(define some? ormap)
(define every? andmap)

(define (find this? list)
  (cond ((null? list) #f)
	((this? (car list)) (car list))
	(else (find this? (cdr list)))))

(define (remove-duplicates list same?)
  (cond ((null? list) list)
	((find (lambda (x)
		 (same? x (car list)))
	       (cdr list))
	 (remove-duplicates (cdr list) same?))
	(else
	 (cons (car list)
	       (remove-duplicates (cdr list) same?)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; A well-defined sorting procedure
(define compat:sort quicksort)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Well-defined character codes.
; Returns the UCS-2 code for a character.

(define compat:char->integer char->integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Error handling.

(define error
  (lambda (msg . irritants)
    (let [(err (open-output-string))]
      (display msg err)
      (for-each (lambda (x) (display " " err) (display x err)) irritants)
      (mz:error 'error (get-output-string err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Symbol generation
; Chez Scheme's gensym is incompatible with the use of gensym in Twobit.

(define gensym
  (let ((n 0))
    (lambda (x)
      (set! n (+ n 1))
      (string->uninterned-symbol (format "~a~a" x n)))))

(define (symbol-hash sym)
  (string-hash (symbol->string sym)))

(define (string-hash string)
  (define (loop s i h)
    (if (< i 0)
	h
	(loop s
	      (- i 1)
	      (logand 65535 (+ (char->integer (string-ref s i)) h h h)))))
  (let ((n (string-length string)))
    (loop string (- n 1) n)))

(define (an-arbitrary-number)
  (abs (current-milliseconds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input and output

(define (write-lop x p)
  (write x p)
  (newline p)
  (newline p))

; Does not use magic syntax for flonums and compnums, but produces 
; valid data anyway.

(define write-fasl-datum write)

(define (twobit-format port fmt . args)
  (cond ((port? port)
         (display (apply format fmt args) port))
        (port
         (display (apply format fmt args)))
        (else
         (apply format fmt args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; UNIX interface to support 'make'.

(define file-modification-time
  file-or-directory-modify-seconds)

(define (compat:file-newer? a b)
  (>= (file-modification-time a) (file-modification-time b)))

;; We need delete-file to not raise error if file doesn't exist
(define (delete-file file)
  (if (file-exists? file)
      (mz:delete-file file)))

;; Note that under MzScheme, open-*-file defaults to Binary mode, not Text.

;(define (open-output-file file)
;  (mz:open-output-file file 'truncate/replace))
;
;(define (with-output-to-file file proc)
;  (parameterize [(current-output-port (open-output-file file))]
;    (begin0 (proc)
;            (close-output-port (current-output-port)))))
;(define (call-with-output-file file proc)
;  (let [(out (open-output-file file))]
;    (begin0 (proc out)
;            (close-output-port out))))

(define open-binary-output-file open-output-file)
(define open-binary-input-file open-input-file)

(define call-with-binary-output-file call-with-output-file)
(define call-with-binary-input-file call-with-input-file)
(define with-binary-output-to-file with-output-to-file)
(define with-binary-input-from-file with-input-from-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Misc

(define (cerror . irritants)
  (error 'error (apply string-append (map (lambda (x) (format "~s" x)) irritants))))

(define (environment-syntax-environment env) env)

(define (vector-copy v)
  (build-vector (vector-length v)
                (lambda (i) (vector-ref v i))))

(define (make-parameter name v)
  (mz:make-parameter v))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= -536870912 x 536870911)))
(define (flonum? x)
  (and (real? x) (inexact? x)))

(define system system/exit-code)

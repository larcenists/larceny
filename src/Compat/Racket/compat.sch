; Compatibility library for hosting Larceny's build under modern Racket
; (v8/v9, Chez Scheme backend), running in plt-r5rs --no-prim.
;
; See README in this directory.

(#%require (only racket/base
                 version regexp-match void
                 open-input-string open-output-string get-output-string
                 file-or-directory-modify-seconds current-directory
                 path->string read-case-sensitive
                 bitwise-ior bitwise-and bitwise-xor bitwise-not
                 arithmetic-shift file-exists? port? format
                 string->uninterned-symbol getenv system-type sub1
                 with-handlers exn-message exn? raise
                 read-byte write-byte write-bytes write-string
                 make-bytes bytes-ref bytes-set! bytes-length bytes-copy
                 bytes? bytes-fill!
                 real->floating-point-bytes floating-point-bytes->real
                 current-milliseconds current-inexact-milliseconds
                 flush-output error-display-handler exit
                 current-command-line-arguments
                 let-values define-values call-with-values
                 print-mpair-curly-braces print-vector-length
                 delete-file copy-file rename-file-or-directory
                 current-error-port
                 error-escape-handler
                 exact-integer?))
(#%require (only racket/system system/exit-code))
(#%require (prefix rkt: racket/base))
(#%require (prefix pretty: racket/pretty))

(#%require (file "reader.rkt"))

(print-mpair-curly-braces #f)

;; Larceny's sources use R7RS lexical syntax that Racket's reader does
;; not accept, so we read with our own reader and evaluate each form.

(define read
  (lambda rest
    (larceny-read (if (null? rest) (current-input-port) (car rest)))))

(define eval
  (let ((r5rs-eval eval))
    (lambda (expr . rest)
      (r5rs-eval expr (if (null? rest) (interaction-environment) (car rest))))))

; As with Racket's own load, relative module paths in the file (such as
; the #%require of reader.rkt in this file, when it is loaded again) are
; relative to the file's directory.

(define (load filename . rest)
  (let-values (((dir name must-be-dir?)
                (rkt:split-path (rkt:path->complete-path filename))))
    (rkt:parameterize ((rkt:current-load-relative-directory
                        (if (rkt:path? dir) dir #f)))
      (call-with-input-file filename
        (lambda (p)
          (let loop ()
            (let ((x (larceny-read p)))
              (if (not (eof-object? x))
                  (begin (eval x)
                         (loop))))))))))

;; Larceny's make-parameter takes a name as its first argument.

(define (make-parameter name v . rest)
  (if (null? rest)
      (rkt:make-parameter v)
      (rkt:make-parameter v (car rest))))

;; Larceny's parameterize accepts arbitrary procedures that behave like
;; parameters (0 arguments = get, 1 argument = set), not only Racket
;; parameter objects.

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((PARAM EXP) ...) BODY ...)
     (parameterize "help" () ((PARAM EXP) ...) () BODY ...))

    ((parameterize "help" (NAMES ...) ((P E) R ...) (PE ...)       BODY ...)
     (parameterize "help" ((FRESH-1 FRESH-2) NAMES ...) (R ...) (PE ... (P E)) BODY ...))

    ((parameterize "help" ((ORIG RHS) ...) () ((PARAM EXPR) ...) BODY ...)
     (let ((ORIG (PARAM)) ...)
       (let ((RHS EXPR) ...)
         (dynamic-wind
             (lambda () (PARAM RHS) ...)
             (lambda () BODY   ...)
             (lambda () (PARAM ORIG) ...)))))))
(read-case-sensitive #t)

(define ($$trace x) #t)
(define host-system 'racket)

;; When *exit-on-error* is set, make our error handler die loudly

(if *exit-on-error*
    (error-escape-handler
     (lambda ()
       (exit 113))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Initialization

(define (compat:initialize)
  (let ((hostdir (nbuild-parameter 'compatibility)))
    (compat:load (string-append hostdir "logops.ss"))
    (compat:load (string-append hostdir "bytevec.ss"))
    (compat:load (string-append hostdir "misc2bytevector.ss"))
    (compat:load (string-append hostdir "compat2.sch"))
    #t))

(define (compat:load filename)
  (if (nbuild-parameter 'verbose-load?)
      (begin (display filename)
             (newline)))
  (load filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Error handling

(define (call-with-error-control thunk1 thunk2)
  (with-handlers
   ((exn? (lambda (exn)
            (display "Error: ")
            (display (exn-message exn))
            (newline)
            (thunk2))))
   (thunk1)))

(define (call-with-error-handler handler thunk)
  (with-handlers ((exn? (lambda (exn) (handler (exn-message exn)))))
    (thunk)))

(define (call-without-interrupts thunk)
  (thunk))

(define error
  (lambda (msg . irritants)
    (let ((err (open-output-string)))
      (display msg err)
      (for-each (lambda (x) (display " " err) (write x err)) irritants)
      (rkt:error 'error (get-output-string err)))))

(define (cerror . irritants)
  (error (apply string-append
                (map (lambda (x) (format "~a" x)) irritants))))

(define (reset) (error "RESET"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Larceny-specific objects

(#%require (prefix rkt: racket/undefined))
(define *undefined-expression* rkt:undefined)
(define *unspecified-expression* (void))
(define *eof-object* (read (open-input-string "")))

(define (unspecified) *unspecified-expression*)
(define (undefined) *undefined-expression*)

; Larceny's assert procedure, used by the native back ends.

(define (assert x . rest)
  (if (not x)
      (error "Assertion failed")))
(define (eof-object) *eof-object*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Common non-standard operations

(define (ormap f l)
  (cond ((null? l) #f) (else (or (f (car l)) (ormap f (cdr l))))))
(define (andmap f l)
  (cond ((null? l) #t) (else (and (f (car l)) (andmap f (cdr l))))))

(define some? ormap)
(define every? andmap)

(define (find this? list)
  (cond ((null? list) #f)
        ((this? (car list)) (car list))
        (else (find this? (cdr list)))))

(define (filter p? l)
  (let loop ((l l))
    (cond ((null? l) l)
          ((p? (car l)) (cons (car l) (loop (cdr l))))
          (else (loop (cdr l))))))

(define (remove-duplicates list same?)
  (cond ((null? list) list)
        ((find (lambda (x)
                 (same? x (car list)))
               (cdr list))
         (remove-duplicates (cdr list) same?))
        (else
         (cons (car list)
               (remove-duplicates (cdr list) same?)))))

(define (append! . args)

  (define (loop rest tail)
    (cond ((null? rest)
           tail)
          ((null? (car rest))
           (loop (cdr rest) tail))
          (else
           (loop (cdr rest)
                 (begin (set-cdr! (last-pair (car rest)) tail)
                        (car rest))))))

  (if (null? args)
      '()
      (let ((a (reverse args)))
        (loop (cdr a) (car a)))))

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (reverse! l)
  (let loop ((l l) (prev '()))
    (if (null? l)
        prev
        (let ((next (cdr l)))
          (set-cdr! l prev)
          (loop next l)))))

(define (vector-copy v)
  (let* ((n (vector-length v))
         (w (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((= i n) w)
      (vector-set! w i (vector-ref v i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; A well-defined (stable merge) sorting procedure

(define (compat:sort l less?)

  (define (merge a b)
    (cond ((null? a) b)
          ((null? b) a)
          ((less? (car b) (car a))
           (cons (car b) (merge a (cdr b))))
          (else
           (cons (car a) (merge (cdr a) b)))))

  (define (sort l n)
    (if (<= n 1)
        (if (= n 0) '() (list (car l)))
        (let ((k (quotient n 2)))
          (merge (sort l k)
                 (sort (list-tail l k) (- n k))))))

  (if (vector? l)
      (list->vector (sort (vector->list l) (vector-length l)))
      (sort l (length l))))

(define sort compat:sort)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Well-defined character codes.

(define compat:char->integer char->integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Symbol generation

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
              (bitwise-and 65535 (+ (char->integer (string-ref s i)) h h h)))))
  (let ((n (string-length string)))
    (loop string (- n 1) n)))

(define (an-arbitrary-number)
  (abs (current-milliseconds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input and output

; LOP and FASL files must be readable by any host Scheme, notably by
; Larceny, so they are written in Larceny's syntax rather than with
; Racket's write (which writes bytevectors as #"..." and the unspecified
; value as #<void>, among other things).

(define (write-lop x p)
  (larceny-syntax-write x p)
  (newline p)
  (newline p))

(define write-fasl-datum
  (lambda (x . rest)
    (larceny-syntax-write x (if (null? rest) (current-output-port) (car rest)))))

; The code-features procedures of Lib/Common/code-features.sch, on the
; lcf structure that reader.rkt reads the #lap(...) and #lop(...)
; markers into.

(define (list->code-features kind clauses)
  (if (not (memq kind '(lap lop)))
      (error "list->code-features: bad kind " kind))
  (lcf kind clauses))

(define (make-code-features kind assumes guarantees)
  (list->code-features kind
                       (list (cons 'assumes assumes)
                             (cons 'guarantees guarantees))))

(define code-features? lcf?)
(define code-features-kind lcf-kind)
(define code-features-clauses lcf-clauses)

(define (code-features-clause cf name)
  (let ((probe (assq name (code-features-clauses cf))))
    (if probe (cdr probe) '())))

(define (code-features-assumes cf) (code-features-clause cf 'assumes))
(define (code-features-guarantees cf) (code-features-clause cf 'guarantees))

(define (write-code-features cf port)
  (larceny-syntax-write cf port))

(define (larceny-syntax-write x port)

  (define (put s) (write-string s port))

  (define (hex n) (number->string n 16))

  (define (plain-symbol? s)
    (let ((n (string-length s)))
      (and (> n 0)
           (not (string->number s))
           (not (string=? s "."))
           (not (char-numeric? (string-ref s 0)))
           (not (char=? (string-ref s 0) #\#))
           (let loop ((i 0))
             (or (= i n)
                 (let ((c (string-ref s i)))
                   (and (or (char-alphabetic? c)
                            (char-numeric? c)
                            (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\>
                                      #\? #\^ #\_ #\~ #\+ #\- #\. #\@)))
                        (< (char->integer c) 128)
                        (loop (+ i 1)))))))))

  ; Characters of a string or |symbol|, escaping delim.
  (define (put-escaped s delim)
    (do ((i 0 (+ i 1)))
        ((= i (string-length s)))
      (let* ((c (string-ref s i))
             (k (char->integer c)))
        (cond ((or (char=? c delim) (char=? c #\\))
               (put (string #\\ c)))
              ((and (<= 32 k) (< k 127))
               (put (string c)))
              (else
               (put (string-append "\\x" (hex k) ";")))))))

  (define (w x)
    (cond ((pair? x)
           (put "(")
           (w (car x))
           (let loop ((x (cdr x)))
             (cond ((null? x))
                   ((pair? x) (put " ") (w (car x)) (loop (cdr x)))
                   (else (put " . ") (w x))))
           (put ")"))
          ((null? x) (put "()"))
          ((eq? x #t) (put "#t"))
          ((eq? x #f) (put "#f"))
          ((symbol? x)
           (let ((s (symbol->string x)))
             (if (plain-symbol? s)
                 (put s)
                 (begin (put "|") (put-escaped s #\|) (put "|")))))
          ((string? x)
           (put "\"") (put-escaped x #\") (put "\""))
          ((number? x) (put (number->string x)))
          ((char? x)
           (let ((k (char->integer x)))
             (if (and (< 32 k) (< k 127))
                 (put (string #\# #\\ x))
                 (put (string-append "#\\x" (hex k))))))
          ((vector? x)
           (put "#(")
           (do ((i 0 (+ i 1)))
               ((= i (vector-length x)))
             (if (> i 0) (put " "))
             (w (vector-ref x i)))
           (put ")"))
          ((bytes? x)
           (put "#vu8(")
           (do ((i 0 (+ i 1)))
               ((= i (bytes-length x)))
             (if (> i 0) (put " "))
             (put (number->string (bytes-ref x i))))
           (put ")"))
          ((lcf? x)
           (put (if (eq? (lcf-kind x) 'lap) "#lap(" "#lop("))
           (let loop ((clauses (lcf-clauses x)) (first? #t))
             (if (pair? clauses)
                 (begin (if (not first?) (put " "))
                        (w (car clauses))
                        (loop (cdr clauses) #f))))
           (put ")"))
          ((eq? x *unspecified-expression*) (put "#!unspecified"))
          ((eq? x *undefined-expression*) (put "#!undefined"))
          ((eof-object? x) (put "#!eof"))
          (else
           (error "larceny-syntax-write: cannot write " x))))

  (w x))

(define (twobit-format port fmt . args)
  (cond ((port? port)
         (display (apply format fmt args) port))
        (port
         (display (apply format fmt args)))
        (else
         (apply format fmt args))))

(define flush-output-port flush-output)

(define (pretty-print x . rest)
  (apply pretty:pretty-print x rest))

(define (writeln . args)
  (for-each display args)
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; UNIX interface to support 'make'.

(define (file-modification-time f)
  (file-or-directory-modify-seconds f))

(define (compat:file-newer? a b)
  (>= (file-modification-time a) (file-modification-time b)))

;; We need delete-file to not raise error if file doesn't exist

(define delete-file
  (let ((rkt-delete-file delete-file))
    (lambda (file)
      (if (file-exists? file)
          (rkt-delete-file file)))))

(define (rename-file old new)
  (rename-file-or-directory old new #t))

(define open-raw-latin-1-output-file open-output-file)
(define open-raw-latin-1-input-file open-input-file)
(define call-with-raw-latin-1-output-file call-with-output-file)
(define call-with-raw-latin-1-input-file call-with-input-file)
(define with-output-to-raw-latin-1-file with-output-to-file)
(define with-input-from-raw-latin-1-file with-input-from-file)

(define open-binary-input-file open-raw-latin-1-input-file)
(define open-binary-output-file open-raw-latin-1-output-file)
(define call-with-binary-input-file call-with-raw-latin-1-input-file)
(define call-with-binary-output-file call-with-raw-latin-1-output-file)

(define system system/exit-code)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Reader parameters.  (Twobit expects these to exist even though they
; have no effect on the host reader.)

(define (err-on-non-boolean name)
  (lambda (x)
    (if (boolean? x)
        x
        (error name "Passed non boolean"))))

(define recognize-javadot-symbols?
  (make-parameter 'recognize-javadot-symbols? #f
                  (err-on-non-boolean 'recognize-javadot-symbols?)))
(define recognize-keywords?
  (make-parameter 'recognize-keywords? #f
                  (err-on-non-boolean 'recognize-keywords?)))
(define case-sensitive?
  (make-parameter 'case-sensitive? #t (err-on-non-boolean 'case-sensitive?)))
(define javadot-symbol? (lambda (x) #f))
(define (javadot-symbol->symbol! x) x)
(define (symbol->javadot-symbol! x) x)

(define compat:read-case-sensitive? read-case-sensitive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Misc

(define (environment-syntax-environment env) env)

;; The fixnum? predicate describes the *target's* fixnums, which have
;; two tag bits: 30 bits on 32-bit targets, 62 bits on 64-bit targets.

(define (fixnum? x)
  (and (exact-integer? x)
       (let ((limit (expt 2 (- (nbuild-parameter 'word-size) 3))))
         (and (<= (- limit) x) (< x limit)))))

(define (flonum? x)
  (and (real? x) (inexact? x)))

(define *root-directory* (path->string (current-directory)))

(define (system-features)
  (list (cons 'os-name (case (system-type)
                         ((macosx)  "MacOS X")
                         ((windows) "Win32")
                         (else      "Linux")))))

(define alist->hash-table 'not-yet-defined)

;; Tables keyed by object identity (eq?), for the compiler:
;;   (compat:make-eq-table)
;;   (compat:eq-table-ref table key default)
;;   (compat:eq-table-set! table key value)

(define (compat:make-eq-table) (rkt:make-hasheq))
(define (compat:eq-table-ref table key default) (rkt:hash-ref table key default))
(define (compat:eq-table-set! table key value) (rkt:hash-set! table key value))

; eof

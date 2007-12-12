(library (rnrs io ports (6))

  (export

   &i/o make-i/o-error i/o-error?
   &i/o-read make-i/o-read-error i/o-read-error?
   &i/o-write make-i/o-write-error i/o-write-error?
   &i/o-invalid-position make-i/o-invalid-position-error
   i/o-invalid-position-error? i/o-error-position
   &i/o-filename make-i/o-filename-error i/o-filename-error?
   i/o-error-filename
   &i/o-file-protection make-i/o-file-protection-error
   i/o-file-protection-error?
   &i/o-file-is-read-only make-i/o-file-is-read-only-error
   i/o-file-is-read-only-error?
   &i/o-file-already-exists make-i/o-file-already-exists-error
   i/o-file-already-exists-error?
   &i/o-file-does-not-exist make-i/o-file-does-not-exist-error
   i/o-file-does-not-exist-error?
   &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port

   file-options                           ; deprecated syntax
   no-create no-fail no-truncate          ; Larceny hack

   buffer-mode                            ; deprecated syntax
   buffer-mode?                           ; deprecated procedure

   latin-1-codec utf-8-codec utf-16-codec

   eol-style                              ; deprecated syntax
   native-eol-style

   &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
   &i/o-encoding make-i/o-encoding-error i/o-encoding-error?
   i/o-encoding-error-char

   error-handling-mode                    ; deprecated syntax

   make-transcoder
   native-transcoder
   transcoder-codec transcoder-eol-style transcoder-error-handling-mode

   bytevector->string string->bytevector

   eof-object eof-object?

   port? port-transcoder textual-port? binary-port? transcoded-port
   port-has-port-position? port-position
   port-has-set-port-position!? set-port-position!
   close-port call-with-port

   input-port? port-eof?
   open-file-input-port open-bytevector-input-port open-string-input-port
   standard-input-port current-input-port
   make-custom-binary-input-port make-custom-textual-input-port

   get-u8 lookahead-u8 get-bytevector-n get-bytevector-n!
   get-bytevector-some                    ; deprecated procedure
   get-bytevector-all

   get-char lookahead-char
   get-string-n get-string-n! get-string-all get-line get-datum

   output-port? flush-output-port output-port-buffer-mode
   open-file-output-port
   open-bytevector-output-port            ; deprecated procedure
   open-string-output-port                ; deprecated procedure
   call-with-bytevector-output-port
   call-with-string-output-port
   standard-output-port current-output-port current-error-port
   make-custom-binary-output-port make-custom-textual-output-port

   put-u8 put-bytevector

   put-char put-string put-datum

   open-file-input/output-port
   make-custom-binary-input/output-port
   make-custom-textual-input/output-port
   )

  (import
   (core primitives)
   (for (only (core primitives) ...) expand)
   (for (core syntax-rules) expand)
   (rnrs base)
   (larceny deprecated) ; [Larceny]
   (primitives

    &i/o make-i/o-error i/o-error?
    &i/o-read make-i/o-read-error i/o-read-error?
    &i/o-write make-i/o-write-error i/o-write-error?
    &i/o-invalid-position make-i/o-invalid-position-error
    i/o-invalid-position-error? i/o-error-position
    &i/o-filename make-i/o-filename-error i/o-filename-error?
    i/o-error-filename
    &i/o-file-protection make-i/o-file-protection-error
    i/o-file-protection-error?
    &i/o-file-is-read-only make-i/o-file-is-read-only-error
    i/o-file-is-read-only-error?
    &i/o-file-already-exists make-i/o-file-already-exists-error
    i/o-file-already-exists-error?
    &i/o-file-does-not-exist make-i/o-file-does-not-exist-error
    i/o-file-does-not-exist-error?
    &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port

    file-options
    no-create no-fail no-truncate                                ; Larceny hack

    buffer-mode?

    latin-1-codec utf-8-codec utf-16-codec

    native-eol-style

    &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
    &i/o-encoding make-i/o-encoding-error i/o-encoding-error?
    i/o-encoding-error-char

    make-transcoder
    native-transcoder
    transcoder-codec transcoder-eol-style transcoder-error-handling-mode

    bytevector->string string->bytevector

    eof-object eof-object?

    port? port-transcoder textual-port? binary-port? transcoded-port
    port-has-port-position? port-position
    port-has-set-port-position!? set-port-position!
    close-port call-with-port

    input-port? port-eof?
    open-file-input-port open-bytevector-input-port open-string-input-port
    standard-input-port current-input-port
    make-custom-binary-input-port make-custom-textual-input-port

    get-u8 lookahead-u8 get-bytevector-n get-bytevector-n!
    get-bytevector-some                    ; deprecated procedure
    get-bytevector-all

    get-char lookahead-char
    get-string-n get-string-n! get-string-all get-line get-datum

    output-port? flush-output-port output-port-buffer-mode
    open-file-output-port
    open-bytevector-output-port            ; deprecated procedure
    open-string-output-port                ; deprecated procedure
    call-with-bytevector-output-port
    call-with-string-output-port
    standard-output-port current-output-port current-error-port
    make-custom-binary-output-port make-custom-textual-output-port

    put-u8 put-bytevector

    put-char put-string put-datum

    open-file-input/output-port
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    ))

  ; [Larceny]
  ; Larceny accepts any symbol as a file option,
  ; but ignores all but a few options.
  ;
  ; This could be implemented as a macro, but it's easier
  ; for Larceny to implement file-options as a procedure.

  ;(define-syntax file-options
  ;  (syntax-rules ()
  ;   ((_ opt ...)
  ;    (make-file-options 'opt ...))))

  ; [Larceny]
  ; In Larceny, *every* symbol describes a buffer mode.
  ; See Lib/Common/portio.sch for semantics.
  ;
  ; The three buffer modes allowed by the current draft R6RS
  ; do not include Larceny's traditional discretionary-flush
  ; mode for interactive ports.  Beginning in Larceny v0.94,
  ; the preferred name of this buffer mode is datum.
  ;
  ; As the current draft R6RS is written, however, the
  ; buffer-mode syntax accepts only three symbols:
  ;
  ;     none, line, block
  ;
  ; Programmers should therefore get into the habit of
  ; specifying buffer modes using Scheme's traditional
  ; quote syntax.
  ;
  ; FIXME:  Syntax checking should be done at macro expansion time,
  ; although this particular syntax check is so stupid it really
  ; shouldn't be done at all.

  (define-syntax buffer-mode
    (syntax-rules ()
     ((_ x)
      (begin
       (issue-warning-deprecated 'buffer-mode)
       (let ((mode (quote x)))
         (if (memq mode '(none line block))
             mode
             (assertion-violation 'buffer-mode
                                  "Larceny-specific buffer mode"
                                  mode)))))))

  ; [Larceny]
  ; In Larceny, *every* symbol describes an eol-style.
  ; See Lib/Common/portio.sch for semantics.

  (define-syntax eol-style
    (syntax-rules ()
     ((_ x)
      (begin
       (issue-warning-deprecated 'eol-style)
       (quote x)))))

  ; [Larceny]
  ; In Larceny, *every* symbol describes an error handling mode.
  ; See Lib/Common/portio.sch for semantics.

  (define-syntax error-handling-mode
    (syntax-rules ()
     ((_ x)
      (begin
       (issue-warning-deprecated 'error-handling-mode)
       (quote x)))))

  )


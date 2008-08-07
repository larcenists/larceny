; Copyright 2008 William D Clinger
;
; $Id$
;
; Larceny messages and localization.
;
; <msgid>
;
;     is a symbol used to identify a message string.  By convention,
;     a <msgid> begins with "msg:", as in msg:notchar.
;
; (errmsg <msgid>)
;
;     returns the language-dependent string associated with <msgid>,
;     or the string extracted from <msgid> itself if there is no string
;     associated with <msgid> in the database.
;
; (errmsg <msgid> <language>)
;
;     returns the string associated with <msgid> in the specified
;     language; if none is found, returns the English string or
;     extracts a string from <msgid> itself.
;
; (define-message <msgid> <string>)
; (define-message <msgid> <string> <language>)
;
;     associates <msgid> with <string> in the database, defaulting
;     to English.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; In Larceny, the preferred ways to raise a non-continuable exception are
;
; (error <who> (errmsg '<msgid>) <irritant> ...)
; (assertion-violation <who> (errmsg '<msgid>) <irritant> ...)
; (raise-r6rs-exception <condition> <who> (errmsg '<msgid>) <irritants>)
;
; where
; <condition> is a base condition,
; <who> is a symbol, string, or #f,
; <msgid> is a symbol that identifies a string in the data base below,
; <irritant> is a Scheme value that had something to do with the exception, and
; <irritants> is a list of <irritant>s.
;
; To raise a continuable exception, call raise-continuable directly.

($$trace "errmsg")

(define messages:english '())
(define messages:non-english '())

(define (errmsg msgid . rest)
  (if (null? rest)
      (let ((probe (assq msgid messages:english)))
        (if probe
            (errmsg:uncompress (cdr probe))
            (let* ((msg (symbol->string msgid))
                   (n (string-length msg))
                   (usual-prefix "msg")
                   (nprefix (string-length usual-prefix)))
              (if (and (>= n nprefix)
                       (string=? usual-prefix (substring msg 0 nprefix)))
                  (substring msg nprefix n)
                  msg))))
      (let* ((probe1 (assq (car rest) messages:non-english))
             (probe (and probe1 (assq msgid probe1))))
        (if probe
            (errmsg:uncompress (cdr probe))
            (errmsg msgid)))))

; (define-message <msgid> <string>)
; (define-message (<msgid> <language>) <string>)

(define (define-message msgid msg . rest)
  (define (display-warning msg0 language)
    (display "WARNING: redefining message for ")
    (write msgid)
    (newline)
    (display "Old message:")
    (newline)
    (write msg0)
    (newline)
    (display "New message:")
    (newline)
    (write msg)
    (newline))
  (if (null? rest)
      (let ((probe (assq msgid messages:english)))
        (if probe
            (let ((msg0 (errmsg:uncompress (cdr probe))))
              (cond ((string=? msg0 msg) #t)
                    (else
                     (display-warning msg0 'english)
                     (set-cdr! probe (errmsg:compress msg))
                     #f)))
            (begin (set! messages:english
                         (cons (cons msgid (errmsg:compress msg))
                               messages:english))
                   #t)))
      (let* ((probe1 (assq (car rest) messages:non-english))
             (probe (and probe1 (assq msgid probe1))))
        (if probe
            (let ((msg0 (cdr probe)))
              (cond ((string=? msg0 msg) #t)
                    (else
                     (display-warning msg0 (car rest))
                     (set-cdr! probe (errmsg:compress msg))
                     #f)))
            (let ((other-languages
                   (filter (lambda (x) (not (eq? x probe1)))
                           messages:non-english))
                  (this-language
                   (cons (cons msgid (errmsg:compress msg))
                         (or probe1 '()))))
              (set! messages:non-english
                    (cons this-language other-languages))
              #t)))))

(define (errmsg:compress s)
  (string->utf8 s))

(define (errmsg:uncompress bv)
  (utf8->string bv))


(for-each (lambda (x) (apply define-message x)) '(

(msg:assert                     "assertion failure")

(msg:notbytevector              "not a bytevector")
(msg:notchar                    "not a char")
(msg:notexactintegers           "not an exact integer")
(msg:notfixnum                  "not a fixnum")
(msg:nothashtable               "not a hashtable")
(msg:notindex                   "not an index")
(msg:notlist                    "not a (proper) list")
(msg:notnumber                  "not a number")
(msg:notport                    "not a port")
(msg:notproc                    "not a procedure")
(msg:notreal                    "not a real number")
(msg:notrtd                     "not a record type descriptor")
(msg:notrecordoftype            "not a record of the correct type")
(msg:notrectnum                 "not a rectnum")
(msg:notstring                  "not a string")
(msg:notsymbol                  "not a symbol")
(msg:notnaturalnumber           "not an exact nonnegative integer")

(msg:illegalarg1                "illegal first argument")
(msg:illegalarg2                "illegal second argument")
(msg:illegalarg3                "illegal third argument")
(msg:illegalarg                 "illegal argument")
(msg:illegalargs                "illegal argument(s)")
(msg:illegalhash                "illegal hash value")
(msg:illegalop                  "illegal operation")
(msg:rangeerror                 "range error")
(msg:toomanyargs                "too many arguments")
(msg:wna                        "wrong number of arguments")
(msg:wna:caselambda             "wrong number of arguments to case-lambda")
(msg:wna:constructor
 "wrong number of arguments to record constructor")
(msg:wna:integrable
 "wrong number of arguments to integrable procedure")

(msg:baddef                     "malformed definition")
(msg:badexpr                    "unrecognized expression")
(msg:badlet-syntax              "malformed let-syntax")
(msg:badformals                 "malformed parameter list")

; Bugs in Larceny

(msg:internalerror              "internal error")

(msg:twobitbugintra
 "compiler bug detected during intraprocedural optimization")
(msg:twobitbugcgvar
 "compiler bug detected in cg-variable")

(msg:sassy:baddata              "sassy: bad data items")
(msg:sassy:badinclude           "sassy: bad include")
(msg:sassy:badtext              "sassy: bad text item")

(msg:printing
 "error during printing; reverting to the ur-printer")

; Errors detected by subsystems

(msg:zerodivide                 "zero divisor")
(msg:fixnumshift                "fixnum shift count out of range")
(msg:fixnumrange                "result out of fixnum range")
(fixnum-range:idiotic-error
 "the R6RS requires the third argument to be less than (fixnum-width)")

(msg:incompatiblesets           "incompatible sets")

(msg:openerror                  "unable to open file")
(msg:readerror                  "read error on port")
(msg:writeerror                 "permanent write error on port")
(msg:writeclosed                "write attempted on closed port")
(msg:weirdtranscoder            "weird transcoder")
(msg:decoding                   "decoding error")
(msg:encoding                   "encoding error")
(msg:nottextualinput            "not a textual input port")
(msg:notoutput                  "not an output port")
(msg:i/oport:nosetportposition  "input/output port without set-port-position!")

(msg:keyboardinterrupt          "keyboard interrupt")

))

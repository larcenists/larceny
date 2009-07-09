; SRFI-28: Basic format strings
; lth@acm.org / 2004-01-10
;
; $Id$
;
; Larceny already has FORMAT built in, and a fancier one to boot.
;
; The built-in one accepts a port, #t, or #f for the first argument,
; so this one does too, while accepting a string as SRFI-28 requires.

(library (srfi :28 basic-format-strings larceny)

  (export larceny:format)

  (import (rnrs base)
          (primitives format))

  (define larceny:format format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (srfi :28 basic-format-strings)

  (export format)

  (import (rnrs base)
          (srfi :6 basic-string-ports)
          (srfi :28 basic-format-strings larceny))

  (define format
    (lambda (fst . args)
      (if (string? fst)
          (let ((s (open-output-string)))
            (apply larceny:format s fst args)
            (get-output-string s))
          (apply larceny:format fst args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (srfi :28)
  (export format)
  (import (srfi :28 basic-format-strings)))

; eof

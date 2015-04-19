; SRFI-28: Basic format strings
; lth@acm.org / 2004-01-10
;
; $Id$
;

(define-library (srfi 28 basic-format-strings larceny)

  (export larceny:format)

  (import (srfi :28 basic-format-strings larceny)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-library (srfi 28 basic-format-strings)

  (export format)

  (import (srfi :28 basic-format-strings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-library (srfi 28)
  (export format)
  (import (srfi 28 basic-format-strings)))

; eof

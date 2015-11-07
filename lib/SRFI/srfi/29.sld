;;; SRFI-29: Localization
;;; Reference implementation
;;;
;;; $Id$
;;;

(define-library (srfi 29 localization)

  (export current-language current-country current-locale-details
          declare-bundle!
          store-bundle store-bundle!
          load-bundle!
          localized-template
          format)

  (import (srfi :29 localization)))


(define-library (srfi 29)

  (export current-language current-country current-locale-details
          declare-bundle!
          store-bundle store-bundle!
          load-bundle!
          localized-template
          format)

  (import (srfi 29 localization)))

; eof

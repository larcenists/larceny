;;; SRFI 31: REC
;;; Reference implementation.
;;;
;;; $Id$
;;;
;;; Copyright (C) Dr. Mirko Luedde (2002). All Rights Reserved. 
;;;
;;; This document and translations of it may be copied and furnished to
;;; others, and derivative works that comment on or otherwise explain it
;;; or assist in its implementation may be prepared, copied, published and
;;; distributed, in whole or in part, without restriction of any kind,
;;; provided that the above copyright notice and this paragraph are
;;; included on all such copies and derivative works. However, this
;;; document itself may not be modified in any way, such as by removing
;;; the copyright notice or references to the Scheme Request For
;;; Implementation process or editors, except as needed for the purpose of
;;; developing SRFIs in which case the procedures for copyrights defined
;;; in the SRFI process must be followed, or as required to translate it
;;; into languages other than English.
;;;
;;; The limited permissions granted above are perpetual and will not be
;;; revoked by the authors or their successors or assigns.
;;;
;;; This document and the information contained herein is provided on an
;;; "AS IS" basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL
;;; WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY
;;; WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE ANY
;;; RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS FOR A
;;; PARTICULAR PURPOSE.

(library (srfi :31 rec)

  (export rec)

  (import (rnrs base))

  (define-syntax rec
    (syntax-rules ()
      ((rec (NAME . VARIABLES) . BODY)
       (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME))
      ((rec NAME EXPRESSION)
       (letrec ( (NAME EXPRESSION) ) NAME))))

  )


(library (srfi :31)
  (export rec)
  (import (srfi :31 rec)))

; eof


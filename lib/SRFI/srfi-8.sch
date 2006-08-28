;;; SRFI 8: RECEIVE: Binding to multiple values
;;; Reference implementation
;;;
;;; $Id$
;;;
;;; Taken from http://srfi.schemers.org/srfi-8/srfi-8.html
;;; on 09 November 2003.
;;;
;;; Copyright (C) John David Stone (1999). All Rights Reserved. 
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
;;; "AS IS" basis and THE AUTHORS AND THE SRFI EDITORS DISCLAIM ALL
;;; WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY
;;; WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE ANY
;;; RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS FOR A
;;; PARTICULAR PURPOSE.

(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
                       (lambda formals body ...)))))

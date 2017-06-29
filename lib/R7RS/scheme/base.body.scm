;;; Definitions for things in (scheme base) that aren't in (rnrs)
;;; or some other R6RS or SRFI library.

;;; NOTE that Twobit may open-code calls to MAP and FOR-EACH and thus
;;; ignore the definitions in this file.  The versions of MAP and
;;; FOR-EACH specified by the R6RS are not compliant with SRFI-1 and
;;; the R7RS: the R6RS requires map and for-each to raise exceptions
;;; on lists of unequal length.
;;;
;;; As of Larceny v0.99, this incompatibility is resolved as follows:
;;;
;;;     open-coded calls to map and for-each perform a closed call
;;;         if their list arguments have different lengths
;;;
;;;     in Larceny's r5rs and r7rs execution modes, map and for-each
;;;         use SRFI 1 and R7RS semantics
;;;
;;;     in Larceny's r6rs execution mode, map and for-each enforce
;;;         the R6RS requirement unless the program has imported
;;;         the (srfi 1) or (scheme base) library, which imply
;;;         SRFI 1 and R7RS semantics
;;;
;;;     the (srfi 1) and (scheme base) libraries announce their
;;;         use by calling a larceny:use-r7rs-semantics! procedure

(larceny:use-r7rs-semantics!)

(define bytevector-copy! r7rs:bytevector-copy!)

(define features larceny:features)

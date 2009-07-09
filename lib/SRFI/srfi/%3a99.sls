; SRFI 99: ERR5RS Records
;
; $Id$
;
; See <http://srfi.schemers.org/srfi-97/srfi-97.html> for the full document.
;
; (srfi :99), (srfi :99 records), and (srfi :99 records syntactic)
; conflict with (rnrs records syntactic) and (srfi :9 records).
; On the other hand, (srfi :99) extends (srfi :9 records).

(library (srfi :99 records procedural)
  (export
   make-rtd rtd? rtd-constructor rtd-predicate rtd-accessor rtd-mutator)
  (import (err5rs records procedural)))

(library (srfi :99 records inspection)
  (export
   record? record-rtd rtd-name rtd-parent
   rtd-field-names rtd-all-field-names rtd-field-mutable?)
  (import (err5rs records inspection)))

(library (srfi :99 records syntactic)
  (export define-record-type)
  (import (err5rs records syntactic)))

(library (srfi :99 records)
  (export
   make-rtd rtd? rtd-constructor rtd-predicate rtd-accessor rtd-mutator
   record? record-rtd rtd-name rtd-parent
   rtd-field-names rtd-all-field-names rtd-field-mutable?
   define-record-type)
  (import (srfi :99 records procedural)
          (srfi :99 records inspection)
          (srfi :99 records syntactic)))

(library (srfi :99)
  (export
   make-rtd rtd? rtd-constructor rtd-predicate rtd-accessor rtd-mutator
   record? record-rtd rtd-name rtd-parent
   rtd-field-names rtd-all-field-names rtd-field-mutable?
   define-record-type)
  (import (srfi :99 records)))

; eof

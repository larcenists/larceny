; SRFI 67: Compare Procedures
;
; $Id$

(define-library (srfi 67 compare-procedures)

  (export boolean-compare
          char-compare char-compare-ci
          string-compare string-compare-ci
          symbol-compare
          integer-compare rational-compare real-compare
          complex-compare number-compare
          vector-compare vector-compare-as-list
          list-compare list-compare-as-vector
          pair-compare-car pair-compare-cdr pair-compare
          default-compare
          refine-compare select-compare cond-compare
          if3 if=? if<? if>? if<=? if>=? if-not=?
          =? <? >? <=? >=? not=?
          </<? </<=? <=/<? <=/<=? >/>? >/>=? >=/>? >=/>=?
          chain=? chain<? chain>? chain<=? chain>=?
          pairwise-not=?
          min-compare max-compare kth-largest
          compare-by< compare-by>
          compare-by<= compare-by>=
          compare-by=/< compare-by=/>
          debug-compare)

  (import (srfi :67 compare-procedures)))


(define-library (srfi 67)

  (export boolean-compare
          char-compare char-compare-ci
          string-compare string-compare-ci
          symbol-compare
          integer-compare rational-compare real-compare
          complex-compare number-compare
          vector-compare vector-compare-as-list
          list-compare list-compare-as-vector
          pair-compare-car pair-compare-cdr pair-compare
          default-compare
          refine-compare select-compare cond-compare
          if3 if=? if<? if>? if<=? if>=? if-not=?
          =? <? >? <=? >=? not=?
          </<? </<=? <=/<? <=/<=? >/>? >/>=? >=/>? >=/>=?
          chain=? chain<? chain>? chain<=? chain>=?
          pairwise-not=?
          min-compare max-compare kth-largest
          compare-by< compare-by>
          compare-by<= compare-by>=
          compare-by=/< compare-by=/>
          debug-compare)

  (import (srfi 67 compare-procedures)))

; eof

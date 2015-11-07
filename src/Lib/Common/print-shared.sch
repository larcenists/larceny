;;; R7RS and SRFI 38: External Representation of Data With Shared Structure
;;;
;;; $Id$
;;;
;;; See <http://srfi.schemers.org/srfi-38/srfi-38.html>
;;; and <http://trac.sacrideo.us/wg/raw-attachment/wiki/WikiStart/r7rs.pdf>
;;; for specification and syntax.

($$trace "print-shared")

;;; Copyright (C) Ray Dillinger 2003. All Rights Reserved. 
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

;;; A printer that shows all sharing of substructures.  Uses the Common
;;; Lisp print-circle notation: #n# refers to a previous substructure
;;; labeled with #n=.   Takes O(n^2) time.

;;; Code attributed to Al* Petrofsky, modified by Dillinger.  
;;;
;;; Modified December 2008 by Will Clinger to use R6RS-style hashtables
;;; and to recognize R6RS data structures.  Now runs in O(n) time if
;;; the hashtable accesses are O(1).
;;;
;;; Modified January 2015 by Will Clinger:
;;;     changed the procedure name to print-with-shared-structure
;;;     made the port argument mandatory
;;;     added a third argument that will be used to write simple structure
;;; 
;;; Also added a predicate that determines whether an object contains
;;; circular structure.

(define (print-with-shared-structure obj outport write)

  (define (lookup key state)
    (hashtable-ref state key #f))

  (define (present? key state)
    (hashtable-contains? state key))

  (define (updated-state key val state)
    (hashtable-set! state key val)
    state)

  ;; An object is interesting if it might have a mutable state.
  ;; An interesting object is especially interesting if it has
  ;; a standard external representation (according to SRFI 38)
  ;; that might contain the object itself.  The interesting
  ;; objects described by the R5RS or R6RS are:
  ;;
  ;;     pairs             (especially interesting)
  ;;     vectors           (especially interesting)
  ;;     strings
  ;;     bytevectors
  ;;     records
  ;;     ports
  ;;     hashtables
  ;;
  ;; We treat zero-length vectors, strings, and bytevectors
  ;; as uninteresting because they don't have a mutable state
  ;; and the reference implementation for SRFI 38 also treated
  ;; them as uninteresting.

  (define (interesting? obj)
    (or (pair? obj)
        (and (vector? obj) (not (zero? (vector-length obj))))
        (and (string? obj) (not (zero? (string-length obj))))
        (bytevector? obj)
        (record? obj)
        (port? obj)
        (hashtable? obj)))

  ;; The state has an entry for each interesting part of OBJ.  The
  ;; associated value will be:
  ;;  -- a number if the part has been given one,
  ;;  -- #t if the part will need to be assigned a number but has not been yet,
  ;;  -- #f if the part will not need a number.
  ;; The state also associates a symbol (counter) with the most
  ;; recently assigned number.
  ;; Returns a state with new entries for any parts that had
  ;; numbers assigned.

  (define (write-obj obj state outport)

    (define (write-interesting state)
      (cond ((pair? obj)
             (display "(" outport)
             (let write-cdr ((obj (cdr obj))
                             (state (write-obj (car obj) state outport)))
               (cond ((and (pair? obj)
                           (not (lookup obj state)))
                      (display " " outport)
                      (write-cdr (cdr obj)
                                 (write-obj (car obj) state outport)))
                     ((null? obj)
                      (display ")" outport)
                      state)
                     (else
                      (display " . " outport)
                      (let ((state (write-obj obj state outport)))
                        (display ")" outport)
                        state)))))
            ((vector? obj)
             (display "#(" outport)
             (let ((len (vector-length obj)))
               (let write-vec ((i 1)
                               (state (write-obj (vector-ref obj 0)
                                                 state outport)))
                 (cond ((= i len) (display ")" outport) state)
                       (else (display " " outport)
                             (write-vec (+ i 1)
                                        (write-obj (vector-ref obj i)
                                                   state outport)))))))
            ;; else it's a string or something
            (else (write obj outport) state)))

    (cond ((interesting? obj)
           (let ((val (lookup obj state)))
             (cond ((not val)
                    (write-interesting state))
                   ((number? val) 
                    (begin (display "#" outport)
                           (write val outport)
                           (display "#" outport)
                           state))
                   (else
                    (let* ((n (+ 1 (lookup 'counter state)))
                           (state (updated-state 'counter n state)))
                      (begin (display "#" outport)
                             (write n outport) 
                             (display "=" outport))
                      (write-interesting (updated-state obj n state)))))))
          (else (write obj outport) state)))

  ;; Scan computes the initial value of the state, which maps each
  ;; interesting part of the object to #t if it occurs multiple times,
  ;; #f if only once.

  (define (scan obj state)
    (cond ((not (interesting? obj)) state)
          ((present? obj state)
           (updated-state obj #t state))
          (else
           (let ((state (updated-state obj #f state)))
             (cond ((pair? obj) (scan (car obj) (scan (cdr obj) state)))
                   ((vector? obj)
                    (let ((len (vector-length obj)))
                      (do ((i 0 (+ 1 i))
                           (state state (scan (vector-ref obj i) state)))
                          ((= i len) state))))
                   (else state))))))

  (let* ((state (make-eq-hashtable))
         (state (scan obj state))
         (state (updated-state 'counter 0 state)))
    (write-obj obj state outport)
    ;; We don't want to return the big state that write-obj just returned.
    (if #f #f)))

;;; An R7RS-conforming write-shared procedure.

(define write-shared
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (print-with-shared-structure x p write-simple))))

;;; An R7RS-conforming write procedure.

(define write
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (if (object-is-circular? x)
          (print-with-shared-structure x p write-simple)
          (write-simple x p)))))

;;; An R7RS-conforming display procedure.

(define display
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (if (object-is-circular? x)
          (print-with-shared-structure x p display-simple)
          (display-simple x p)))))

; eof

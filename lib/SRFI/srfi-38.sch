;;; SRFI 38
;;; External Representation for Data With Shared Structure
;;;
;;; $Id$

;;; Copyright (C) Ray Dillinger 2003. All Rights Reserved. 

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

;;; The limited permissions granted above are perpetual and will not be
;;; revoked by the authors or their successors or assigns.

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
;;; and to recognize R6RS data structures.

(define (write-with-shared-structure obj . optional-port)

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
         (state (updated-state 'counter 0 state))
         (outport (if (eq? '() optional-port)
                      (current-output-port)
                      (car optional-port))))
    (write-obj obj state outport)
    ;; We don't want to return the big state that write-obj just returned.
    (if #f #f)))


(define (read-with-shared-structure . optional-port)
  (define port
    (if (null? optional-port) (current-input-port) (car optional-port)))

  (define (read-char*) (read-char port))
  (define (peek-char*) (peek-char port))

  (define (looking-at? c)
    (eqv? c (peek-char*)))

  (define (delimiter? c)
    (case c
      ((#\( #\) #\" #\;) #t)
      (else (or (eof-object? c)
                (char-whitespace? c)))))

  (define (not-delimiter? c) (not (delimiter? c)))

  (define (eat-intertoken-space)
    (define c (peek-char*))
    (cond ((eof-object? c))
          ((char-whitespace? c) (read-char*) (eat-intertoken-space))
          ((char=? c #\;)
           (do ((c (read-char*) (read-char*)))
               ((or (eof-object? c) (char=? c #\newline))))
           (eat-intertoken-space))))

  (define (read-string)
    (read-char*)
    (let read-it ((chars '()))
      (let ((c (read-char*)))
        (if (eof-object? c)
            (error "EOF inside a string")
            (case c
              ((#\") (list->string (reverse chars)))
              ((#\\) (read-it (cons (read-char*) chars)))
              (else (read-it (cons c chars))))))))

  ;; reads chars that match PRED and returns them as a string.
  (define (read-some-chars pred)
    (let iter ((chars '()))
      (let ((c (peek-char*)))
        (if (or (eof-object? c) (not (pred c)))
            (list->string (reverse chars))
            (iter (cons (read-char*) chars))))))

  ;; reads a character after the #\ has been read.
  (define (read-character)
    (let ((c (peek-char*)))
      (cond ((eof-object? c) (error "EOF inside a character"))
            ((char-alphabetic? c)
             (let ((name (read-some-chars char-alphabetic?)))
               (cond ((= 1 (string-length name)) (string-ref name 0))
                     ((string-ci=? name "space") #\space)
                     ((string-ci=? name "newline") #\newline)
                     (else (error "Unknown named character: " name)))))
            (else (read-char*)))))

  (define (read-number first-char)
    (let ((str (string-append (string first-char)
                              (read-some-chars not-delimiter?))))
      (or (string->number str)
          (error "Malformed number: " str))))

  (define char-standard-case
    (if (char=? #\a (string-ref (symbol->string 'a) 0))
        char-downcase
        char-upcase))

  (define (string-standard-case str)
    (let* ((len (string-length str))
           (new (make-string len)))
      (do ((i 0 (+ i 1)))
          ((= i len) new)
        (string-set! new i (char-standard-case (string-ref str i))))))

  (define (read-identifier)
    (string->symbol (string-standard-case (read-some-chars not-delimiter?))))

  (define (read-part-spec)
    (let ((n (string->number (read-some-chars char-numeric?))))
      (let ((c (read-char*)))
        (case c
          ((#\=) (cons 'decl n))
          ((#\#) (cons 'use n))
          (else (error "Malformed shared part specifier"))))))

  ;; Tokens: strings, characters, numbers, booleans, and
  ;; identifiers/symbols are represented as themselves.
  ;; Single-character tokens are represented as (CHAR), the
  ;; two-character tokens #( and ,@ become (#\#) and (#\@).
  ;; #NN= and #NN# become (decl . NN) and (use . NN).
  (define (read-optional-token)
    (eat-intertoken-space)
    (let ((c (peek-char*)))
      (case c
        ((#\( #\) #\' #\`) (read-char*) (list c))
        ((#\,)
         (read-char*)
         (if (looking-at? #\@)
             (begin (read-char*) '(#\@))
             '(#\,)))
        ((#\") (read-string))
        ((#\.)
         (read-char*)
         (cond ((delimiter? (peek-char*)) '(#\.))
               ((not (looking-at? #\.)) (read-number #\.))
               ((begin (read-char*) (looking-at? #\.)) (read-char*) '...)
               (else (error "Malformed token starting with \"..\""))))
        ((#\+) (read-char*) (if (delimiter? (peek-char*)) '+ (read-number c)))
        ((#\-) (read-char*) (if (delimiter? (peek-char*)) '- (read-number c)))
        ((#\#)
         (read-char*)
         (let ((c (peek-char*)))
           (case c
             ((#\() (read-char*) '(#\#))
             ((#\\) (read-char*) (read-character))
             ((#\t #\T) (read-char*) #t)
             ((#\f #\F) (read-char*) #f)
             (else (cond ((eof-object? c) (error "EOF inside a # token"))
                         ((char-numeric? c) (read-part-spec))
                         (else (read-number #\#)))))))
        (else (cond ((eof-object? c) c)
                    ((char-numeric? c) (read-char*) (read-number c))
                    (else (read-identifier)))))))

  (define (read-token)
    (let ((tok (read-optional-token)))
      (if (eof-object? tok)
          (error "EOF where token was required")
          tok)))

  ;; Parts-alist maps the number of each part to a thunk that returns the part.
  (define parts-alist '())

  (define (add-part-to-alist! n thunk)
    (set! parts-alist (cons (cons n thunk) parts-alist)))

  ;; Read-object returns a datum that may contain some thunks, which
  ;; need to be replaced with their return values.
  (define (read-object)
    (finish-reading-object (read-token)))

  ;; Like read-object, but may return EOF.
  (define (read-optional-object)
    (finish-reading-object (read-optional-token)))

  (define (finish-reading-object first-token)
    (if (not (pair? first-token))
        first-token
        (if (char? (car first-token))
            (case (car first-token)
              ((#\() (read-list-tail))
              ((#\#) (list->vector (read-list-tail)))
              ((#\. #\)) (error (string-append "Unexpected \"" first-token "\"")))
              (else
               (list (caadr (assv (car first-token)
                                  '((#\' 'x) (#\, ,x) (#\` `x) (#\@ ,@x))))
                     (read-object))))
            ;; We need to specially handle chains of declarations in
            ;; order to allow #1=#2=x and #1=(#2=#1#) and not to allow
            ;; #1=#2=#1# nor #1=#2=#1=x.
            (let ((starting-alist parts-alist))
              (let read-decls ((token first-token))
                (if (and (pair? token) (symbol? (car token)))
                    (let ((n (cdr token)))
                      (case (car token)
                        ((use)
                         ;; To use a part, it must have been
                         ;; declared before this chain started.
                         (cond ((assv n starting-alist) => cdr)
                               (else (error "Use of undeclared part " n))))
                        ((decl)
                         (if (assv n parts-alist)
                             (error "Double declaration of part " n))
                         ;; Letrec enables us to make deferred
                         ;; references to an object before it exists.
                         (letrec ((obj (begin
                                         (add-part-to-alist! n (lambda () obj))
                                         (read-decls (read-token)))))
                           obj))))
                    (finish-reading-object token)))))))

  (define (read-list-tail)
    (let ((token (read-token)))
      (if (not (pair? token))
          (cons token (read-list-tail))
          (case (car token)
            ((#\)) '())
            ((#\.) (let* ((obj (read-object))
                          (tok (read-token)))
                     (if (and (pair? tok) (char=? #\) (car tok)))
                         obj
                         (error "Extra junk after a dot"))))
            (else (let ((obj (finish-reading-object token)))
                    (cons obj (read-list-tail))))))))

  ;; Unthunk.
  ;; To deference a part that was declared using another part,
  ;; e.g. #2=#1#, may require multiple dethunkings.  We were careful
  ;; in finish-reading-object to ensure that this won't loop forever:
  (define (unthunk thunk)
    (let ((x (thunk)))
      (if (procedure? x) (unthunk x) x)))

  (let ((obj (read-optional-object)))
    (let fill-in-parts ((obj obj))
      (cond ((pair? obj)
             (if (procedure? (car obj))
                 (set-car! obj (unthunk (car obj)))
                 (fill-in-parts (car obj)))
             (if (procedure? (cdr obj))
                 (set-cdr! obj (unthunk (cdr obj)))
                 (fill-in-parts (cdr obj))))
            ((vector? obj)
             (let ((len (vector-length obj)))
               (do ((i 0 (+ i 1)))
                   ((= i len))
                 (let ((elt (vector-ref obj i)))
                   (if (procedure? elt)
                       (vector-set! obj i (unthunk elt))
                       (fill-in-parts elt))))))))
    obj))

(define write/ss write-with-shared-structure)
(define read/ss read-with-shared-structure)

; -*- mode: scheme; indent-tabs-mode: nil -*-
;
; Globber.  Somewhat tested.
; 2002-03-18 / lth
;
; $Id$

; NB: pnkfelix says its buggy; (glob "[a]z" "ay") shouldn't be #t.

; This is the GLOB(7) algorithm as commonly used on Unix systems.
; The exact behavior of the algorithm is specified by a standard
; (namely, POSIX 1003.2, 3.13).  The following procedure implements
; the subset of that standard that corresponds with the basic GLOB
; algorithm.  It does not have any POSIX extensions: locale
; sensitivity, character classes, collating symbols, or equivalence
; classes.  Here is a summary of the behavior:
;
;       \c      matches the character c
;       c       matches the character c
;       *       matches zero or more characters except '/'
;       ?       matches any character except '/'
;       [...]   matches any character in the set except '/'
;       
;               Inside a set, a leading '!' denotes complementation
;               (the set matches any character not in the set).  
;                       
;               A ']' can occur in the set if it comes first (possibly
;               following '!').  A range a-b in the set denotes a
;               character range; the set contains all characters a 
;               through b inclusive.  A '-' can occur in the set if it
;               comes first (possibly following '!') or if it comes
;               last.  '\', '*', '[', and '?' have no special meaning
;               inside a set, and '/' may not be in the set.
;
;  Unlike the standard globber, this one does not treat a leading '.'
;  specially.

(define (glob pat str)
  (let ((plen (string-length pat))
        (slen (string-length str)))

    ; If (= p plen) is met then the pattern is syntactically malformed,
    ; return #f for this always.

    (define (charset-matcher p s complement?)
      (let ((c (string-ref str s)))
        (if (= p plen)
            #f
            (let charset-loop ((p p) (first-time? #t) (success? #f))
              (let* ((c1 (string-ref pat p))
                     (p  (+ p 1)))
                (cond ((and (char=? c1 #\]) (not first-time?))
                       (not (eq? complement? success?)))
                      ((and (char=? c1 #\-) first-time?)
                       (if (char=? c1 c)
                           (not complement?)
                           (charset-loop p #f complement?)))
                      ((= p plen)
                       #f)
                      ((char=? (string-ref pat p) #\-)
                       (let ((p (+ p 1)))
                         (if (= p plen)
                             #f
                             (let* ((c2 (string-ref pat p))
                                    (p  (+ p 1)))
                               (cond ((char=? c2 #\])
                                      (charset-loop (- p 2)
                                                    #f
                                                    (char=? c1 c)))
                                     ((= p plen)
                                      #f)
                                     (else
                                      (charset-loop p 
                                                    #f 
                                                    (and (char<=? c1 c) 
                                                         (char<=? c c2)))))))))
                      ((char=? c1 c)
                       (charset-loop p #f #t))
                      (else
                       (charset-loop p #f success?))))))))

    (define (match p s)
      (if (= p plen)
          (= s slen)
          (let* ((c (string-ref pat p))
                 (p (+ p 1)))
            (case c
              ((#\*)
               (if (= p plen)
                   (let loop ((s s))
                     (cond ((= s slen)                      #t)
                           ((char=? (string-ref str s) #\/) #f)
                           (else                            (loop (+ s 1)))))
                   (let loop ((s s))
                     (cond ((match p s)                     #t)
                           ((= s slen)                      #f)
                           ((char=? (string-ref str s) #\/) #f)
                           (else                            (loop (+ s 1)))))))

              ((#\?)
               (if (or (= s slen) (char=? (string-ref str s) #\/))
                   #f
                   (match p (+ s 1))))

              ((#\[)
               (cond ((or (= p plen) (= s slen))
                      #f)
                     ((char=? (string-ref str s) #\/)
                      #f)
                     ((char=? (string-ref pat p) #\!)
                      (charset-matcher (+ p 1) s #t))
                     (else
                      (charset-matcher p s #f))))

              ((#\\) 
               (cond ((or (= p plen) (= s slen))
                      #f)
                     ((char=? (string-ref pat p) (string-ref str s))
                      (match (+ p 1) (+ s 1)))
                     (else
                      #f)))

              (else
               (if (or (= s slen) (not (char=? c (string-ref str s))))
                   #f
                   (match p (+ s 1))))))))

    (match 0 0)))

; eof

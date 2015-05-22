;;; This file defines the five (rnrs unicode) procedures that
;;; are not already provided by (scheme char):
;;;
;;;     char-general-category
;;;     string-normalize-nfd
;;;     string-normalize-nfkd
;;;     string-normalize-nfc
;;;     string-normalize-nfkc

;;; For the moment, these four are defined as stubs.

(define (string-normalize-nfd s)
  (error "string-normalize-nfd is not yet implemented"))

(define (string-normalize-nfkd s)
  (error "string-normalize-nfkd is not yet implemented"))

(define (string-normalize-nfc s)
  (error "string-normalize-nfc is not yet implemented"))

(define (string-normalize-nfkc s)
  (error "string-normalize-nfkc is not yet implemented"))

;;; FIXME: This should go away once (rnrs bytevectors) has been
;;; implemented.

(define (u8-list->bytevector bytes)
  (apply bytevector bytes))

;;; FIXME: This should go away once (rnrs base) has been
;;; implemented.

(define (div x y)
  (quotient x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Help procedures (not part of R6RS)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given an exact integer key and a vector of exact integers
; in strictly increasing order, returns the largest i such
; that element i of the vector is less than or equal to key,
; or -1 if key is less than every element of the vector.

(define (binary-search-of-vector key vec)

  ; Loop invariants:
  ; 0 <= i < j <= (vector-length vec)
  ; vec[i] <= key
  ; if j < (vector-length vec), then key < vec[j]

  (define (loop i j)
    (let ((mid (div (+ i j) 2)))
      (cond ((= i mid)
             mid)
            ((<= (vector-ref vec mid) key)
             (loop mid j))
            (else
             (loop i mid)))))

  (let ((hi (vector-length vec)))
    (if (or (= hi 0) (< key (vector-ref vec 0)))
        -1
        (loop 0 hi))))

;;; What follows was extracted from the reference implementation
;;; of (rnrs unicode) as upgraded for Unicode 7.0.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Copyright 2006 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright and permission notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;

; Given a character, returns its Unicode general category.
; The tables used to implement this procedure occupy about 12869 bytes.
; About a third of those bytes could be saved by splitting the large
; vector into a bytevector for the 16-bit scalar values and using a
; general vector only for the scalar values greater than 65535.

(define (char-general-category c)
  (let ((n (char->integer c)))
    (vector-ref
     vector-of-general-category-symbols
     (if (< n (bytevector-length
               general-category-indices-for-common-characters))
         (bytevector-u8-ref general-category-indices-for-common-characters n)
         (bytevector-u8-ref general-category-indices-for-all-characters
                            (binary-search-of-vector
                             n
                             vector-of-code-points-with-same-category))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Unicode general properties.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The symbols that represent Unicode general properties.
; There are 30 of these.
; This table occupies about 128 bytes, not counting
; the space occupied by the symbols themselves.

(define vector-of-general-category-symbols
  '#(
     ; Letter: uppercase, lowercase, titlecase, modifier, other
     Lu Ll Lt Lm Lo

     ; Mark: nonspacing, spacing combining, enclosing
     Mn Mc Me

     ; Number: decimal digit, letter, other
     Nd Nl No

     ; Punctuation: connector, dash, open, close,
     ;     initial quote, final quote, other
     Pc Pd Ps Pe Pi Pf Po

     ; Symbol: math, currency, modifier, other
     Sm Sc Sk So

     ; Separator: space, line, paragraph
     Zs Zl Zp

     ; Other: control, format, surrogate, private use, not assigned
     Cc Cf Cs Co Cn))

; Given a symbol that appears in the vector above,
; returns its index within the vector.
; Used only for initialization, so it needn't be fast.

(define (general-category-symbol->index sym)
  (let ((n (vector-length vector-of-general-category-symbols)))
    (do ((i 0 (+ i 1)))
        ((or (= i n)
             (eq? sym (vector-ref vector-of-general-category-symbols i)))
         (if (= i n)
             (error "Unrecognized Unicode general category" sym)
             i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The following tables were generated from
; UnicodeData.txt, CaseFolding.txt,
; SpecialCasing.txt, PropList.txt,
; WordBreakProperty.txt, and CompositionExclusions.txt.
; Use parseUCD.sch to regenerate these tables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following array of bytes, together with the vector below it,
; implements an indirect mapping from all Unicode scalar values to
; indices into vector-of-general-category-symbols.
;
; This table contains 3499 entries.

(define general-category-indices-for-all-characters
  (u8-list->bytevector
   (map
    general-category-symbol->index
    '(
      Cc Zs Po Sc Po Ps Pe Po Sm Po Pd Po Nd Po Sm Po 
      Lu Ps Po Pe Sk Pc Sk Ll Ps Sm Pe Sm Cc Zs Po Sc 
      So Po Sk So Lo Pi Sm Cf So Sk So Sm No Sk Ll Po 
      Sk No Lo Pf No Po Lu Sm Lu Ll Sm Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lo Lu Ll Lo 
      Lu Lt Ll Lu Lt Ll Lu Lt Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Lt Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lo Ll Lm Sk 
      Lm Sk Lm Sk Lm Sk Lm Sk Mn Lu Ll Lu Ll Lm Sk Lu 
      Ll Cn Lm Ll Po Lu Cn Sk Lu Po Lu Cn Lu Cn Lu Ll 
      Lu Cn Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Sm Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll So Mn Me Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Cn Lu Cn Lm Po Cn Ll Cn Po Pd Cn So Sc Cn 
      Mn Pd Mn Po Mn Po Mn Po Mn Cn Lo Cn Lo Po Cn Cf 
      Sm Po Sc Po So Mn Po Cf Cn Po Lo Lm Lo Mn Nd Po 
      Lo Mn Lo Po Lo Mn Cf So Mn Lm Mn So Mn Lo Nd Lo 
      So Lo Po Cn Cf Lo Mn Lo Mn Cn Lo Mn Lo Cn Nd Lo 
      Mn Lm So Po Lm Cn Lo Mn Lm Mn Lm Mn Lm Mn Cn Po 
      Cn Lo Mn Cn Po Cn Lo Cn Mn Mc Lo Mn Mc Mn Lo Mc 
      Mn Mc Mn Mc Lo Mn Lo Mn Po Nd Po Lm Lo Mn Mc Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Lo Mc Mn 
      Cn Mc Cn Mc Mn Lo Cn Mc Cn Lo Cn Lo Mn Cn Nd Lo 
      Sc No So Sc Cn Mn Mc Cn Lo Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Mn Cn Mc Mn Cn Mn Cn Mn Cn Mn 
      Cn Lo Cn Lo Cn Nd Mn Lo Mn Cn Mn Mc Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Lo Mc Mn Cn Mn Mc 
      Cn Mc Mn Cn Lo Cn Lo Mn Cn Nd Po Sc Cn Mn Mc Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Lo Mc Mn 
      Mc Mn Cn Mc Cn Mc Mn Cn Mn Mc Cn Lo Cn Lo Mn Cn 
      Nd So Lo No Cn Mn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Mc Mn Mc Cn Mc Cn 
      Mc Mn Cn Lo Cn Mc Cn Nd No So Sc So Cn Mn Mc Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Mn Mc Cn Mn Cn Mn Cn 
      Mn Cn Lo Cn Lo Mn Cn Nd Cn No So Cn Mn Mc Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Lo Mc Mn Mc Cn Mn 
      Mc Cn Mc Mn Cn Mc Cn Lo Cn Lo Mn Cn Nd Cn Lo Cn 
      Mn Mc Cn Lo Cn Lo Cn Lo Cn Lo Mc Mn Cn Mc Cn Mc 
      Mn Lo Cn Mc Cn Lo Mn Cn Nd No Cn So Lo Cn Mc Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Cn Mc Mn Cn Mn 
      Cn Mc Cn Nd Cn Mc Po Cn Lo Mn Lo Mn Cn Sc Lo Lm 
      Mn Po Nd Po Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Mn Lo Mn Cn 
      Mn Lo Cn Lo Cn Lm Cn Mn Cn Nd Cn Lo Cn Lo So Po 
      So Po So Mn So Nd No So Mn So Mn So Mn Ps Pe Ps 
      Pe Mc Lo Cn Lo Cn Mn Mc Mn Po Mn Lo Mn Cn Mn Cn 
      So Mn So Cn So Po So Po Cn Lo Mc Mn Mc Mn Mc Mn 
      Mc Mn Lo Nd Po Lo Mc Mn Lo Mn Lo Mc Lo Mc Lo Mn 
      Lo Mn Mc Mn Mc Mn Lo Mc Nd Mc Mn So Lu Cn Lu Cn 
      Lu Cn Lo Po Lm Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Mn Po No Cn Lo So Cn Lo Cn Pd Lo 
      Po Lo Zs Lo Ps Pe Cn Lo Po Nl Lo Cn Lo Cn Lo Mn 
      Cn Lo Mn Po Cn Lo Mn Cn Lo Cn Lo Cn Mn Cn Lo Mn 
      Mc Mn Mc Mn Mc Mn Po Lm Po Sc Lo Mn Cn Nd Cn No 
      Cn Po Pd Po Mn Cf Cn Nd Cn Lo Lm Lo Cn Lo Mn Lo 
      Cn Lo Cn Lo Cn Mn Mc Mn Mc Cn Mc Mn Mc Mn Cn So 
      Cn Po Nd Lo Cn Lo Cn Lo Cn Mc Lo Mc Cn Nd No Cn 
      So Lo Mn Mc Mn Cn Po Lo Mc Mn Mc Mn Cn Mn Mc Mn 
      Mc Mn Mc Mn Cn Mn Nd Cn Nd Cn Po Lm Po Cn Mn Me 
      Cn Mn Mc Lo Mn Mc Mn Mc Mn Mc Mn Mc Lo Cn Nd Po 
      So Mn So Cn Mn Mc Lo Mc Mn Mc Mn Mc Mn Lo Nd Lo 
      Mn Mc Mn Mc Mn Mc Mn Mc Cn Po Lo Mc Mn Mc Mn Cn 
      Po Nd Cn Lo Nd Lo Lm Po Cn Po Cn Mn Po Mn Mc Mn 
      Lo Mn Lo Mc Mn Lo Cn Mn Cn Ll Lm Ll Lm Ll Lm Mn 
      Cn Mn Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Cn Lu Cn Ll 
      Lu Ll Lu Ll Cn Lu Cn Ll Cn Lu Cn Lu Cn Lu Cn Lu 
      Ll Lu Ll Cn Ll Lt Ll Lt Ll Lt Ll Cn Ll Lu Lt Sk 
      Ll Sk Ll Cn Ll Lu Lt Sk Ll Cn Ll Lu Cn Sk Ll Lu 
      Sk Cn Ll Cn Ll Lu Lt Sk Cn Zs Cf Pd Po Pi Pf Ps 
      Pi Pf Ps Pi Po Zl Zp Cf Zs Po Pi Pf Po Pc Po Sm 
      Ps Pe Po Sm Po Pc Po Zs Cf Cn Cf No Lm Cn No Sm 
      Ps Pe Lm No Sm Ps Pe Cn Lm Cn Sc Cn Mn Me Mn Me 
      Mn Cn So Lu So Lu So Ll Lu Ll Lu Ll So Lu So Sm 
      Lu So Lu So Lu So Lu So Lu So Ll Lu Ll Lo Ll So 
      Ll Lu Sm Lu Ll So Sm So Ll So No Nl Lu Ll Nl No 
      Cn Sm So Sm So Sm So Sm So Sm So Sm So Sm So Sm 
      So Sm So Sm So Ps Pe Ps Pe So Sm So Ps Pe So Sm 
      So Sm So Sm So Cn So Cn So Cn No So No So Sm So 
      Sm So Sm So Sm So Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe 
      Ps Pe Ps Pe No So Sm Ps Pe Sm Ps Pe Ps Pe Ps Pe 
      Ps Pe Ps Pe Sm So Sm Ps Pe Ps Pe Ps Pe Ps Pe Ps 
      Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Sm Ps Pe 
      Ps Pe Sm Ps Pe Sm So Sm So Sm So Cn So Cn So Cn 
      So Cn So Cn Lu Cn Ll Cn Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lm Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll So Lu Ll 
      Lu Ll Mn Lu Ll Cn Po No Po Ll Cn Ll Cn Ll Cn Lo 
      Cn Lm Po Cn Mn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Mn Po Pi Pf Pi Pf Po Pi Pf 
      Po Pi Pf Po Pd Po Pd Po Pi Pf Po Pi Pf Ps Pe Ps 
      Pe Ps Pe Ps Pe Po Lm Po Pd Po Pd Po Ps Cn So Cn 
      So Cn So Cn So Cn Zs Po So Lm Lo Nl Ps Pe Ps Pe 
      Ps Pe Ps Pe Ps Pe So Ps Pe Ps Pe Ps Pe Ps Pe Pd 
      Ps Pe So Nl Mn Mc Pd Lm So Nl Lm Lo Po So Cn Lo 
      Cn Mn Sk Lm Lo Pd Lo Po Lm Lo Cn Lo Cn Lo Cn So 
      No So Lo Cn So Cn Lo So Cn No So No So No So No 
      So No So Cn So Lo Cn So Lo Cn Lo Lm Lo Cn So Cn 
      Lo Lm Po Lo Lm Po Lo Nd Lo Cn Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lo Mn Me Po Mn Po Lm Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lm Cn Mn Lo Nl 
      Mn Po Cn Sk Lm Sk Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lm Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lm Sk Lu Ll Lu Ll Cn Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Cn Lu Cn Lo Lm Ll Lo Mn Lo Mn Lo Mn Lo Mc 
      Mn Mc So Cn No So Sc So Cn Lo Po Cn Mc Lo Mc Mn 
      Cn Po Nd Cn Mn Lo Po Lo Cn Nd Lo Mn Po Lo Mn Mc 
      Cn Po Lo Cn Mn Mc Lo Mn Mc Mn Mc Mn Mc Po Cn Lm 
      Nd Cn Po Lo Mn Lm Lo Nd Lo Cn Lo Mn Mc Mn Mc Mn 
      Cn Lo Mn Lo Mn Mc Cn Nd Cn Po Lo Lm Lo So Lo Mc 
      Mn Mc Lo Mn Lo Mn Lo Mn Lo Mn Lo Mn Lo Cn Lo Lm 
      Po Lo Mc Mn Mc Po Lo Lm Mc Mn Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Ll Sk Lm Cn Ll Cn Lo Mc Mn Mc Mn 
      Mc Po Mc Mn Cn Nd Cn Lo Cn Lo Cn Lo Cn Cs Co Lo 
      Cn Lo Cn Ll Cn Ll Cn Lo Mn Lo Sm Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Sk Cn Lo Pe Ps Cn Lo Cn Lo Cn 
      Lo Sc So Cn Mn Po Ps Pe Po Cn Mn Cn Po Pd Pc Ps 
      Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Po 
      Ps Pe Po Pc Po Cn Po Pd Ps Pe Ps Pe Ps Pe Po Sm 
      Pd Sm Cn Po Sc Po Cn Lo Cn Lo Cn Cf Cn Po Sc Po 
      Ps Pe Po Sm Po Pd Po Nd Po Sm Po Lu Ps Po Pe Sk 
      Pc Sk Ll Ps Sm Pe Sm Ps Pe Po Ps Pe Po Lo Lm Lo 
      Lm Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Sc Sm Sk So Sc 
      Cn So Sm So Cn Cf So Cn Lo Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Po Cn No Cn So Nl No So No So 
      Cn So Cn So Cn So Mn Cn Lo Cn Lo Cn Mn No Cn Lo 
      No Cn Lo Nl Lo Nl Cn Lo Mn Cn Lo Cn Po Lo Cn Lo 
      Po Nl Cn Lu Ll Lo Cn Nd Cn Lo Cn Lo Cn Po Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Po No Lo So No Lo Cn No Cn Lo No Cn Po Lo Cn 
      Po Cn Lo Cn Lo Cn Lo Mn Cn Mn Cn Mn Lo Cn Lo Cn 
      Lo Cn Mn Cn Mn No Cn Po Cn Lo No Po Lo No Cn Lo 
      So Lo Mn Cn No Po Cn Lo Cn Po Lo Cn No Lo Cn No 
      Lo Cn Po Cn No Cn Lo Cn No Cn Mc Mn Mc Lo Mn Po 
      Cn No Nd Cn Mn Mc Lo Mc Mn Mc Mn Po Cf Po Cn Lo 
      Cn Nd Cn Mn Lo Mn Mc Mn Cn Nd Po Cn Lo Mn Po Lo 
      Cn Mn Mc Lo Mc Mn Mc Lo Po Cn Po Cn Nd Lo Cn No 
      Cn Lo Cn Lo Mc Mn Mc Mn Mc Mn Po Cn Lo Mn Mc Mn 
      Cn Nd Cn Mn Mc Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Mn Lo Mc Mn Mc Cn Mc Cn Mc Cn Mc Cn Lo Mc 
      Cn Mn Cn Mn Cn Lo Mc Mn Mc Mn Mc Mn Mc Mn Lo Po 
      Lo Cn Nd Cn Lo Mc Mn Cn Mc Mn Mc Mn Po Cn Lo Mc 
      Mn Mc Mn Mc Mn Po Lo Cn Nd Cn Lo Mn Mc Mn Mc Mn 
      Mc Mn Cn Nd Cn Lu Ll Nd No Cn Lo Cn Lo Cn Lo Cn 
      Nl Cn Po Cn Lo Cn Lo Cn Lo Cn Nd Cn Po Cn Lo Cn 
      Mn Po Cn Lo Mn Po So Lm Po So Cn Nd Cn No Cn Lo 
      Cn Lo Cn Lo Cn Lo Mc Cn Mn Lm Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn So Mn Po Cf Cn So Cn So Cn So Mc 
      Mn So Mc Cf Mn So Mn So Mn So Cn So Mn So Cn So 
      Cn No Cn Lu Ll Lu Ll Cn Ll Lu Ll Lu Cn Lu Cn Lu 
      Cn Lu Cn Lu Cn Lu Ll Cn Ll Cn Ll Cn Ll Lu Ll Lu 
      Cn Lu Cn Lu Cn Lu Cn Ll Lu Cn Lu Cn Lu Cn Lu Cn 
      Lu Cn Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Cn 
      Lu Sm Ll Sm Ll Lu Sm Ll Sm Ll Lu Sm Ll Sm Ll Lu 
      Sm Ll Sm Ll Lu Sm Ll Sm Ll Lu Ll Cn Nd Cn Lo Cn 
      No Mn Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Sm Cn So Cn So Cn So Cn So Cn So 
      Cn So Cn No Cn So Cn So Cn So Cn So Cn So Cn So 
      Cn So Cn So Cn So Cn So Cn So Cn So Cn So Cn So 
      Cn So Cn So Cn So Cn So Cn So Cn So Cn So Cn So 
      Cn So Cn So Cn So Cn So Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Cf Cn Cf Cn Mn Cn Co Cn Co Cn ))))

; The following vector of exact integers represents the
; Unicode scalar values whose Unicode general category
; is different from the Unicode scalar value immediately
; less than it.
;
; This table contains 3499 entries.

(define vector-of-code-points-with-same-category
  '#(
     #x0 #x20 #x21 #x24 #x25 #x28 #x29 #x2a 
     #x2b #x2c #x2d #x2e #x30 #x3a #x3c #x3f 
     #x41 #x5b #x5c #x5d #x5e #x5f #x60 #x61 
     #x7b #x7c #x7d #x7e #x7f #xa0 #xa1 #xa2 
     #xa6 #xa7 #xa8 #xa9 #xaa #xab #xac #xad 
     #xae #xaf #xb0 #xb1 #xb2 #xb4 #xb5 #xb6 
     #xb8 #xb9 #xba #xbb #xbc #xbf #xc0 #xd7 
     #xd8 #xdf #xf7 #xf8 #x100 #x101 #x102 #x103 
     #x104 #x105 #x106 #x107 #x108 #x109 #x10a #x10b 
     #x10c #x10d #x10e #x10f #x110 #x111 #x112 #x113 
     #x114 #x115 #x116 #x117 #x118 #x119 #x11a #x11b 
     #x11c #x11d #x11e #x11f #x120 #x121 #x122 #x123 
     #x124 #x125 #x126 #x127 #x128 #x129 #x12a #x12b 
     #x12c #x12d #x12e #x12f #x130 #x131 #x132 #x133 
     #x134 #x135 #x136 #x137 #x139 #x13a #x13b #x13c 
     #x13d #x13e #x13f #x140 #x141 #x142 #x143 #x144 
     #x145 #x146 #x147 #x148 #x14a #x14b #x14c #x14d 
     #x14e #x14f #x150 #x151 #x152 #x153 #x154 #x155 
     #x156 #x157 #x158 #x159 #x15a #x15b #x15c #x15d 
     #x15e #x15f #x160 #x161 #x162 #x163 #x164 #x165 
     #x166 #x167 #x168 #x169 #x16a #x16b #x16c #x16d 
     #x16e #x16f #x170 #x171 #x172 #x173 #x174 #x175 
     #x176 #x177 #x178 #x17a #x17b #x17c #x17d #x17e 
     #x181 #x183 #x184 #x185 #x186 #x188 #x189 #x18c 
     #x18e #x192 #x193 #x195 #x196 #x199 #x19c #x19e 
     #x19f #x1a1 #x1a2 #x1a3 #x1a4 #x1a5 #x1a6 #x1a8 
     #x1a9 #x1aa #x1ac #x1ad #x1ae #x1b0 #x1b1 #x1b4 
     #x1b5 #x1b6 #x1b7 #x1b9 #x1bb #x1bc #x1bd #x1c0 
     #x1c4 #x1c5 #x1c6 #x1c7 #x1c8 #x1c9 #x1ca #x1cb 
     #x1cc #x1cd #x1ce #x1cf #x1d0 #x1d1 #x1d2 #x1d3 
     #x1d4 #x1d5 #x1d6 #x1d7 #x1d8 #x1d9 #x1da #x1db 
     #x1dc #x1de #x1df #x1e0 #x1e1 #x1e2 #x1e3 #x1e4 
     #x1e5 #x1e6 #x1e7 #x1e8 #x1e9 #x1ea #x1eb #x1ec 
     #x1ed #x1ee #x1ef #x1f1 #x1f2 #x1f3 #x1f4 #x1f5 
     #x1f6 #x1f9 #x1fa #x1fb #x1fc #x1fd #x1fe #x1ff 
     #x200 #x201 #x202 #x203 #x204 #x205 #x206 #x207 
     #x208 #x209 #x20a #x20b #x20c #x20d #x20e #x20f 
     #x210 #x211 #x212 #x213 #x214 #x215 #x216 #x217 
     #x218 #x219 #x21a #x21b #x21c #x21d #x21e #x21f 
     #x220 #x221 #x222 #x223 #x224 #x225 #x226 #x227 
     #x228 #x229 #x22a #x22b #x22c #x22d #x22e #x22f 
     #x230 #x231 #x232 #x233 #x23a #x23c #x23d #x23f 
     #x241 #x242 #x243 #x247 #x248 #x249 #x24a #x24b 
     #x24c #x24d #x24e #x24f #x294 #x295 #x2b0 #x2c2 
     #x2c6 #x2d2 #x2e0 #x2e5 #x2ec #x2ed #x2ee #x2ef 
     #x300 #x370 #x371 #x372 #x373 #x374 #x375 #x376 
     #x377 #x378 #x37a #x37b #x37e #x37f #x380 #x384 
     #x386 #x387 #x388 #x38b #x38c #x38d #x38e #x390 
     #x391 #x3a2 #x3a3 #x3ac #x3cf #x3d0 #x3d2 #x3d5 
     #x3d8 #x3d9 #x3da #x3db #x3dc #x3dd #x3de #x3df 
     #x3e0 #x3e1 #x3e2 #x3e3 #x3e4 #x3e5 #x3e6 #x3e7 
     #x3e8 #x3e9 #x3ea #x3eb #x3ec #x3ed #x3ee #x3ef 
     #x3f4 #x3f5 #x3f6 #x3f7 #x3f8 #x3f9 #x3fb #x3fd 
     #x430 #x460 #x461 #x462 #x463 #x464 #x465 #x466 
     #x467 #x468 #x469 #x46a #x46b #x46c #x46d #x46e 
     #x46f #x470 #x471 #x472 #x473 #x474 #x475 #x476 
     #x477 #x478 #x479 #x47a #x47b #x47c #x47d #x47e 
     #x47f #x480 #x481 #x482 #x483 #x488 #x48a #x48b 
     #x48c #x48d #x48e #x48f #x490 #x491 #x492 #x493 
     #x494 #x495 #x496 #x497 #x498 #x499 #x49a #x49b 
     #x49c #x49d #x49e #x49f #x4a0 #x4a1 #x4a2 #x4a3 
     #x4a4 #x4a5 #x4a6 #x4a7 #x4a8 #x4a9 #x4aa #x4ab 
     #x4ac #x4ad #x4ae #x4af #x4b0 #x4b1 #x4b2 #x4b3 
     #x4b4 #x4b5 #x4b6 #x4b7 #x4b8 #x4b9 #x4ba #x4bb 
     #x4bc #x4bd #x4be #x4bf #x4c0 #x4c2 #x4c3 #x4c4 
     #x4c5 #x4c6 #x4c7 #x4c8 #x4c9 #x4ca #x4cb #x4cc 
     #x4cd #x4ce #x4d0 #x4d1 #x4d2 #x4d3 #x4d4 #x4d5 
     #x4d6 #x4d7 #x4d8 #x4d9 #x4da #x4db #x4dc #x4dd 
     #x4de #x4df #x4e0 #x4e1 #x4e2 #x4e3 #x4e4 #x4e5 
     #x4e6 #x4e7 #x4e8 #x4e9 #x4ea #x4eb #x4ec #x4ed 
     #x4ee #x4ef #x4f0 #x4f1 #x4f2 #x4f3 #x4f4 #x4f5 
     #x4f6 #x4f7 #x4f8 #x4f9 #x4fa #x4fb #x4fc #x4fd 
     #x4fe #x4ff #x500 #x501 #x502 #x503 #x504 #x505 
     #x506 #x507 #x508 #x509 #x50a #x50b #x50c #x50d 
     #x50e #x50f #x510 #x511 #x512 #x513 #x514 #x515 
     #x516 #x517 #x518 #x519 #x51a #x51b #x51c #x51d 
     #x51e #x51f #x520 #x521 #x522 #x523 #x524 #x525 
     #x526 #x527 #x528 #x529 #x52a #x52b #x52c #x52d 
     #x52e #x52f #x530 #x531 #x557 #x559 #x55a #x560 
     #x561 #x588 #x589 #x58a #x58b #x58d #x58f #x590 
     #x591 #x5be #x5bf #x5c0 #x5c1 #x5c3 #x5c4 #x5c6 
     #x5c7 #x5c8 #x5d0 #x5eb #x5f0 #x5f3 #x5f5 #x600 
     #x606 #x609 #x60b #x60c #x60e #x610 #x61b #x61c 
     #x61d #x61e #x620 #x640 #x641 #x64b #x660 #x66a 
     #x66e #x670 #x671 #x6d4 #x6d5 #x6d6 #x6dd #x6de 
     #x6df #x6e5 #x6e7 #x6e9 #x6ea #x6ee #x6f0 #x6fa 
     #x6fd #x6ff #x700 #x70e #x70f #x710 #x711 #x712 
     #x730 #x74b #x74d #x7a6 #x7b1 #x7b2 #x7c0 #x7ca 
     #x7eb #x7f4 #x7f6 #x7f7 #x7fa #x7fb #x800 #x816 
     #x81a #x81b #x824 #x825 #x828 #x829 #x82e #x830 
     #x83f #x840 #x859 #x85c #x85e #x85f #x8a0 #x8b3 
     #x8e4 #x903 #x904 #x93a #x93b #x93c #x93d #x93e 
     #x941 #x949 #x94d #x94e #x950 #x951 #x958 #x962 
     #x964 #x966 #x970 #x971 #x972 #x981 #x982 #x984 
     #x985 #x98d #x98f #x991 #x993 #x9a9 #x9aa #x9b1 
     #x9b2 #x9b3 #x9b6 #x9ba #x9bc #x9bd #x9be #x9c1 
     #x9c5 #x9c7 #x9c9 #x9cb #x9cd #x9ce #x9cf #x9d7 
     #x9d8 #x9dc #x9de #x9df #x9e2 #x9e4 #x9e6 #x9f0 
     #x9f2 #x9f4 #x9fa #x9fb #x9fc #xa01 #xa03 #xa04 
     #xa05 #xa0b #xa0f #xa11 #xa13 #xa29 #xa2a #xa31 
     #xa32 #xa34 #xa35 #xa37 #xa38 #xa3a #xa3c #xa3d 
     #xa3e #xa41 #xa43 #xa47 #xa49 #xa4b #xa4e #xa51 
     #xa52 #xa59 #xa5d #xa5e #xa5f #xa66 #xa70 #xa72 
     #xa75 #xa76 #xa81 #xa83 #xa84 #xa85 #xa8e #xa8f 
     #xa92 #xa93 #xaa9 #xaaa #xab1 #xab2 #xab4 #xab5 
     #xaba #xabc #xabd #xabe #xac1 #xac6 #xac7 #xac9 
     #xaca #xacb #xacd #xace #xad0 #xad1 #xae0 #xae2 
     #xae4 #xae6 #xaf0 #xaf1 #xaf2 #xb01 #xb02 #xb04 
     #xb05 #xb0d #xb0f #xb11 #xb13 #xb29 #xb2a #xb31 
     #xb32 #xb34 #xb35 #xb3a #xb3c #xb3d #xb3e #xb3f 
     #xb40 #xb41 #xb45 #xb47 #xb49 #xb4b #xb4d #xb4e 
     #xb56 #xb57 #xb58 #xb5c #xb5e #xb5f #xb62 #xb64 
     #xb66 #xb70 #xb71 #xb72 #xb78 #xb82 #xb83 #xb84 
     #xb85 #xb8b #xb8e #xb91 #xb92 #xb96 #xb99 #xb9b 
     #xb9c #xb9d #xb9e #xba0 #xba3 #xba5 #xba8 #xbab 
     #xbae #xbba #xbbe #xbc0 #xbc1 #xbc3 #xbc6 #xbc9 
     #xbca #xbcd #xbce #xbd0 #xbd1 #xbd7 #xbd8 #xbe6 
     #xbf0 #xbf3 #xbf9 #xbfa #xbfb #xc00 #xc01 #xc04 
     #xc05 #xc0d #xc0e #xc11 #xc12 #xc29 #xc2a #xc3a 
     #xc3d #xc3e #xc41 #xc45 #xc46 #xc49 #xc4a #xc4e 
     #xc55 #xc57 #xc58 #xc5a #xc60 #xc62 #xc64 #xc66 
     #xc70 #xc78 #xc7f #xc80 #xc81 #xc82 #xc84 #xc85 
     #xc8d #xc8e #xc91 #xc92 #xca9 #xcaa #xcb4 #xcb5 
     #xcba #xcbc #xcbd #xcbe #xcbf #xcc0 #xcc5 #xcc6 
     #xcc7 #xcc9 #xcca #xccc #xcce #xcd5 #xcd7 #xcde 
     #xcdf #xce0 #xce2 #xce4 #xce6 #xcf0 #xcf1 #xcf3 
     #xd01 #xd02 #xd04 #xd05 #xd0d #xd0e #xd11 #xd12 
     #xd3b #xd3d #xd3e #xd41 #xd45 #xd46 #xd49 #xd4a 
     #xd4d #xd4e #xd4f #xd57 #xd58 #xd60 #xd62 #xd64 
     #xd66 #xd70 #xd76 #xd79 #xd7a #xd80 #xd82 #xd84 
     #xd85 #xd97 #xd9a #xdb2 #xdb3 #xdbc #xdbd #xdbe 
     #xdc0 #xdc7 #xdca #xdcb #xdcf #xdd2 #xdd5 #xdd6 
     #xdd7 #xdd8 #xde0 #xde6 #xdf0 #xdf2 #xdf4 #xdf5 
     #xe01 #xe31 #xe32 #xe34 #xe3b #xe3f #xe40 #xe46 
     #xe47 #xe4f #xe50 #xe5a #xe5c #xe81 #xe83 #xe84 
     #xe85 #xe87 #xe89 #xe8a #xe8b #xe8d #xe8e #xe94 
     #xe98 #xe99 #xea0 #xea1 #xea4 #xea5 #xea6 #xea7 
     #xea8 #xeaa #xeac #xead #xeb1 #xeb2 #xeb4 #xeba 
     #xebb #xebd #xebe #xec0 #xec5 #xec6 #xec7 #xec8 
     #xece #xed0 #xeda #xedc #xee0 #xf00 #xf01 #xf04 
     #xf13 #xf14 #xf15 #xf18 #xf1a #xf20 #xf2a #xf34 
     #xf35 #xf36 #xf37 #xf38 #xf39 #xf3a #xf3b #xf3c 
     #xf3d #xf3e #xf40 #xf48 #xf49 #xf6d #xf71 #xf7f 
     #xf80 #xf85 #xf86 #xf88 #xf8d #xf98 #xf99 #xfbd 
     #xfbe #xfc6 #xfc7 #xfcd #xfce #xfd0 #xfd5 #xfd9 
     #xfdb #x1000 #x102b #x102d #x1031 #x1032 #x1038 #x1039 
     #x103b #x103d #x103f #x1040 #x104a #x1050 #x1056 #x1058 
     #x105a #x105e #x1061 #x1062 #x1065 #x1067 #x106e #x1071 
     #x1075 #x1082 #x1083 #x1085 #x1087 #x108d #x108e #x108f 
     #x1090 #x109a #x109d #x109e #x10a0 #x10c6 #x10c7 #x10c8 
     #x10cd #x10ce #x10d0 #x10fb #x10fc #x10fd #x1249 #x124a 
     #x124e #x1250 #x1257 #x1258 #x1259 #x125a #x125e #x1260 
     #x1289 #x128a #x128e #x1290 #x12b1 #x12b2 #x12b6 #x12b8 
     #x12bf #x12c0 #x12c1 #x12c2 #x12c6 #x12c8 #x12d7 #x12d8 
     #x1311 #x1312 #x1316 #x1318 #x135b #x135d #x1360 #x1369 
     #x137d #x1380 #x1390 #x139a #x13a0 #x13f5 #x1400 #x1401 
     #x166d #x166f #x1680 #x1681 #x169b #x169c #x169d #x16a0 
     #x16eb #x16ee #x16f1 #x16f9 #x1700 #x170d #x170e #x1712 
     #x1715 #x1720 #x1732 #x1735 #x1737 #x1740 #x1752 #x1754 
     #x1760 #x176d #x176e #x1771 #x1772 #x1774 #x1780 #x17b4 
     #x17b6 #x17b7 #x17be #x17c6 #x17c7 #x17c9 #x17d4 #x17d7 
     #x17d8 #x17db #x17dc #x17dd #x17de #x17e0 #x17ea #x17f0 
     #x17fa #x1800 #x1806 #x1807 #x180b #x180e #x180f #x1810 
     #x181a #x1820 #x1843 #x1844 #x1878 #x1880 #x18a9 #x18aa 
     #x18ab #x18b0 #x18f6 #x1900 #x191f #x1920 #x1923 #x1927 
     #x1929 #x192c #x1930 #x1932 #x1933 #x1939 #x193c #x1940 
     #x1941 #x1944 #x1946 #x1950 #x196e #x1970 #x1975 #x1980 
     #x19ac #x19b0 #x19c1 #x19c8 #x19ca #x19d0 #x19da #x19db 
     #x19de #x1a00 #x1a17 #x1a19 #x1a1b #x1a1c #x1a1e #x1a20 
     #x1a55 #x1a56 #x1a57 #x1a58 #x1a5f #x1a60 #x1a61 #x1a62 
     #x1a63 #x1a65 #x1a6d #x1a73 #x1a7d #x1a7f #x1a80 #x1a8a 
     #x1a90 #x1a9a #x1aa0 #x1aa7 #x1aa8 #x1aae #x1ab0 #x1abe 
     #x1abf #x1b00 #x1b04 #x1b05 #x1b34 #x1b35 #x1b36 #x1b3b 
     #x1b3c #x1b3d #x1b42 #x1b43 #x1b45 #x1b4c #x1b50 #x1b5a 
     #x1b61 #x1b6b #x1b74 #x1b7d #x1b80 #x1b82 #x1b83 #x1ba1 
     #x1ba2 #x1ba6 #x1ba8 #x1baa #x1bab #x1bae #x1bb0 #x1bba 
     #x1be6 #x1be7 #x1be8 #x1bea #x1bed #x1bee #x1bef #x1bf2 
     #x1bf4 #x1bfc #x1c00 #x1c24 #x1c2c #x1c34 #x1c36 #x1c38 
     #x1c3b #x1c40 #x1c4a #x1c4d #x1c50 #x1c5a #x1c78 #x1c7e 
     #x1c80 #x1cc0 #x1cc8 #x1cd0 #x1cd3 #x1cd4 #x1ce1 #x1ce2 
     #x1ce9 #x1ced #x1cee #x1cf2 #x1cf4 #x1cf5 #x1cf7 #x1cf8 
     #x1cfa #x1d00 #x1d2c #x1d6b #x1d78 #x1d79 #x1d9b #x1dc0 
     #x1df6 #x1dfc #x1e00 #x1e01 #x1e02 #x1e03 #x1e04 #x1e05 
     #x1e06 #x1e07 #x1e08 #x1e09 #x1e0a #x1e0b #x1e0c #x1e0d 
     #x1e0e #x1e0f #x1e10 #x1e11 #x1e12 #x1e13 #x1e14 #x1e15 
     #x1e16 #x1e17 #x1e18 #x1e19 #x1e1a #x1e1b #x1e1c #x1e1d 
     #x1e1e #x1e1f #x1e20 #x1e21 #x1e22 #x1e23 #x1e24 #x1e25 
     #x1e26 #x1e27 #x1e28 #x1e29 #x1e2a #x1e2b #x1e2c #x1e2d 
     #x1e2e #x1e2f #x1e30 #x1e31 #x1e32 #x1e33 #x1e34 #x1e35 
     #x1e36 #x1e37 #x1e38 #x1e39 #x1e3a #x1e3b #x1e3c #x1e3d 
     #x1e3e #x1e3f #x1e40 #x1e41 #x1e42 #x1e43 #x1e44 #x1e45 
     #x1e46 #x1e47 #x1e48 #x1e49 #x1e4a #x1e4b #x1e4c #x1e4d 
     #x1e4e #x1e4f #x1e50 #x1e51 #x1e52 #x1e53 #x1e54 #x1e55 
     #x1e56 #x1e57 #x1e58 #x1e59 #x1e5a #x1e5b #x1e5c #x1e5d 
     #x1e5e #x1e5f #x1e60 #x1e61 #x1e62 #x1e63 #x1e64 #x1e65 
     #x1e66 #x1e67 #x1e68 #x1e69 #x1e6a #x1e6b #x1e6c #x1e6d 
     #x1e6e #x1e6f #x1e70 #x1e71 #x1e72 #x1e73 #x1e74 #x1e75 
     #x1e76 #x1e77 #x1e78 #x1e79 #x1e7a #x1e7b #x1e7c #x1e7d 
     #x1e7e #x1e7f #x1e80 #x1e81 #x1e82 #x1e83 #x1e84 #x1e85 
     #x1e86 #x1e87 #x1e88 #x1e89 #x1e8a #x1e8b #x1e8c #x1e8d 
     #x1e8e #x1e8f #x1e90 #x1e91 #x1e92 #x1e93 #x1e94 #x1e95 
     #x1e9e #x1e9f #x1ea0 #x1ea1 #x1ea2 #x1ea3 #x1ea4 #x1ea5 
     #x1ea6 #x1ea7 #x1ea8 #x1ea9 #x1eaa #x1eab #x1eac #x1ead 
     #x1eae #x1eaf #x1eb0 #x1eb1 #x1eb2 #x1eb3 #x1eb4 #x1eb5 
     #x1eb6 #x1eb7 #x1eb8 #x1eb9 #x1eba #x1ebb #x1ebc #x1ebd 
     #x1ebe #x1ebf #x1ec0 #x1ec1 #x1ec2 #x1ec3 #x1ec4 #x1ec5 
     #x1ec6 #x1ec7 #x1ec8 #x1ec9 #x1eca #x1ecb #x1ecc #x1ecd 
     #x1ece #x1ecf #x1ed0 #x1ed1 #x1ed2 #x1ed3 #x1ed4 #x1ed5 
     #x1ed6 #x1ed7 #x1ed8 #x1ed9 #x1eda #x1edb #x1edc #x1edd 
     #x1ede #x1edf #x1ee0 #x1ee1 #x1ee2 #x1ee3 #x1ee4 #x1ee5 
     #x1ee6 #x1ee7 #x1ee8 #x1ee9 #x1eea #x1eeb #x1eec #x1eed 
     #x1eee #x1eef #x1ef0 #x1ef1 #x1ef2 #x1ef3 #x1ef4 #x1ef5 
     #x1ef6 #x1ef7 #x1ef8 #x1ef9 #x1efa #x1efb #x1efc #x1efd 
     #x1efe #x1eff #x1f08 #x1f10 #x1f16 #x1f18 #x1f1e #x1f20 
     #x1f28 #x1f30 #x1f38 #x1f40 #x1f46 #x1f48 #x1f4e #x1f50 
     #x1f58 #x1f59 #x1f5a #x1f5b #x1f5c #x1f5d #x1f5e #x1f5f 
     #x1f60 #x1f68 #x1f70 #x1f7e #x1f80 #x1f88 #x1f90 #x1f98 
     #x1fa0 #x1fa8 #x1fb0 #x1fb5 #x1fb6 #x1fb8 #x1fbc #x1fbd 
     #x1fbe #x1fbf #x1fc2 #x1fc5 #x1fc6 #x1fc8 #x1fcc #x1fcd 
     #x1fd0 #x1fd4 #x1fd6 #x1fd8 #x1fdc #x1fdd #x1fe0 #x1fe8 
     #x1fed #x1ff0 #x1ff2 #x1ff5 #x1ff6 #x1ff8 #x1ffc #x1ffd 
     #x1fff #x2000 #x200b #x2010 #x2016 #x2018 #x2019 #x201a 
     #x201b #x201d #x201e #x201f #x2020 #x2028 #x2029 #x202a 
     #x202f #x2030 #x2039 #x203a #x203b #x203f #x2041 #x2044 
     #x2045 #x2046 #x2047 #x2052 #x2053 #x2054 #x2055 #x205f 
     #x2060 #x2065 #x2066 #x2070 #x2071 #x2072 #x2074 #x207a 
     #x207d #x207e #x207f #x2080 #x208a #x208d #x208e #x208f 
     #x2090 #x209d #x20a0 #x20be #x20d0 #x20dd #x20e1 #x20e2 
     #x20e5 #x20f1 #x2100 #x2102 #x2103 #x2107 #x2108 #x210a 
     #x210b #x210e #x2110 #x2113 #x2114 #x2115 #x2116 #x2118 
     #x2119 #x211e #x2124 #x2125 #x2126 #x2127 #x2128 #x2129 
     #x212a #x212e #x212f #x2130 #x2134 #x2135 #x2139 #x213a 
     #x213c #x213e #x2140 #x2145 #x2146 #x214a #x214b #x214c 
     #x214e #x214f #x2150 #x2160 #x2183 #x2184 #x2185 #x2189 
     #x218a #x2190 #x2195 #x219a #x219c #x21a0 #x21a1 #x21a3 
     #x21a4 #x21a6 #x21a7 #x21ae #x21af #x21ce #x21d0 #x21d2 
     #x21d3 #x21d4 #x21d5 #x21f4 #x2300 #x2308 #x2309 #x230a 
     #x230b #x230c #x2320 #x2322 #x2329 #x232a #x232b #x237c 
     #x237d #x239b #x23b4 #x23dc #x23e2 #x23fb #x2400 #x2427 
     #x2440 #x244b #x2460 #x249c #x24ea #x2500 #x25b7 #x25b8 
     #x25c1 #x25c2 #x25f8 #x2600 #x266f #x2670 #x2768 #x2769 
     #x276a #x276b #x276c #x276d #x276e #x276f #x2770 #x2771 
     #x2772 #x2773 #x2774 #x2775 #x2776 #x2794 #x27c0 #x27c5 
     #x27c6 #x27c7 #x27e6 #x27e7 #x27e8 #x27e9 #x27ea #x27eb 
     #x27ec #x27ed #x27ee #x27ef #x27f0 #x2800 #x2900 #x2983 
     #x2984 #x2985 #x2986 #x2987 #x2988 #x2989 #x298a #x298b 
     #x298c #x298d #x298e #x298f #x2990 #x2991 #x2992 #x2993 
     #x2994 #x2995 #x2996 #x2997 #x2998 #x2999 #x29d8 #x29d9 
     #x29da #x29db #x29dc #x29fc #x29fd #x29fe #x2b00 #x2b30 
     #x2b45 #x2b47 #x2b4d #x2b74 #x2b76 #x2b96 #x2b98 #x2bba 
     #x2bbd #x2bc9 #x2bca #x2bd2 #x2c00 #x2c2f #x2c30 #x2c5f 
     #x2c60 #x2c61 #x2c62 #x2c65 #x2c67 #x2c68 #x2c69 #x2c6a 
     #x2c6b #x2c6c #x2c6d #x2c71 #x2c72 #x2c73 #x2c75 #x2c76 
     #x2c7c #x2c7e #x2c81 #x2c82 #x2c83 #x2c84 #x2c85 #x2c86 
     #x2c87 #x2c88 #x2c89 #x2c8a #x2c8b #x2c8c #x2c8d #x2c8e 
     #x2c8f #x2c90 #x2c91 #x2c92 #x2c93 #x2c94 #x2c95 #x2c96 
     #x2c97 #x2c98 #x2c99 #x2c9a #x2c9b #x2c9c #x2c9d #x2c9e 
     #x2c9f #x2ca0 #x2ca1 #x2ca2 #x2ca3 #x2ca4 #x2ca5 #x2ca6 
     #x2ca7 #x2ca8 #x2ca9 #x2caa #x2cab #x2cac #x2cad #x2cae 
     #x2caf #x2cb0 #x2cb1 #x2cb2 #x2cb3 #x2cb4 #x2cb5 #x2cb6 
     #x2cb7 #x2cb8 #x2cb9 #x2cba #x2cbb #x2cbc #x2cbd #x2cbe 
     #x2cbf #x2cc0 #x2cc1 #x2cc2 #x2cc3 #x2cc4 #x2cc5 #x2cc6 
     #x2cc7 #x2cc8 #x2cc9 #x2cca #x2ccb #x2ccc #x2ccd #x2cce 
     #x2ccf #x2cd0 #x2cd1 #x2cd2 #x2cd3 #x2cd4 #x2cd5 #x2cd6 
     #x2cd7 #x2cd8 #x2cd9 #x2cda #x2cdb #x2cdc #x2cdd #x2cde 
     #x2cdf #x2ce0 #x2ce1 #x2ce2 #x2ce3 #x2ce5 #x2ceb #x2cec 
     #x2ced #x2cee #x2cef #x2cf2 #x2cf3 #x2cf4 #x2cf9 #x2cfd 
     #x2cfe #x2d00 #x2d26 #x2d27 #x2d28 #x2d2d #x2d2e #x2d30 
     #x2d68 #x2d6f #x2d70 #x2d71 #x2d7f #x2d80 #x2d97 #x2da0 
     #x2da7 #x2da8 #x2daf #x2db0 #x2db7 #x2db8 #x2dbf #x2dc0 
     #x2dc7 #x2dc8 #x2dcf #x2dd0 #x2dd7 #x2dd8 #x2ddf #x2de0 
     #x2e00 #x2e02 #x2e03 #x2e04 #x2e05 #x2e06 #x2e09 #x2e0a 
     #x2e0b #x2e0c #x2e0d #x2e0e #x2e17 #x2e18 #x2e1a #x2e1b 
     #x2e1c #x2e1d #x2e1e #x2e20 #x2e21 #x2e22 #x2e23 #x2e24 
     #x2e25 #x2e26 #x2e27 #x2e28 #x2e29 #x2e2a #x2e2f #x2e30 
     #x2e3a #x2e3c #x2e40 #x2e41 #x2e42 #x2e43 #x2e80 #x2e9a 
     #x2e9b #x2ef4 #x2f00 #x2fd6 #x2ff0 #x2ffc #x3000 #x3001 
     #x3004 #x3005 #x3006 #x3007 #x3008 #x3009 #x300a #x300b 
     #x300c #x300d #x300e #x300f #x3010 #x3011 #x3012 #x3014 
     #x3015 #x3016 #x3017 #x3018 #x3019 #x301a #x301b #x301c 
     #x301d #x301e #x3020 #x3021 #x302a #x302e #x3030 #x3031 
     #x3036 #x3038 #x303b #x303c #x303d #x303e #x3040 #x3041 
     #x3097 #x3099 #x309b #x309d #x309f #x30a0 #x30a1 #x30fb 
     #x30fc #x30ff #x3100 #x3105 #x312e #x3131 #x318f #x3190 
     #x3192 #x3196 #x31a0 #x31bb #x31c0 #x31e4 #x31f0 #x3200 
     #x321f #x3220 #x322a #x3248 #x3250 #x3251 #x3260 #x3280 
     #x328a #x32b1 #x32c0 #x32ff #x3300 #x3400 #x4db6 #x4dc0 
     #x4e00 #x9fcd #xa000 #xa015 #xa016 #xa48d #xa490 #xa4c7 
     #xa4d0 #xa4f8 #xa4fe #xa500 #xa60c #xa60d #xa610 #xa620 
     #xa62a #xa62c #xa640 #xa641 #xa642 #xa643 #xa644 #xa645 
     #xa646 #xa647 #xa648 #xa649 #xa64a #xa64b #xa64c #xa64d 
     #xa64e #xa64f #xa650 #xa651 #xa652 #xa653 #xa654 #xa655 
     #xa656 #xa657 #xa658 #xa659 #xa65a #xa65b #xa65c #xa65d 
     #xa65e #xa65f #xa660 #xa661 #xa662 #xa663 #xa664 #xa665 
     #xa666 #xa667 #xa668 #xa669 #xa66a #xa66b #xa66c #xa66d 
     #xa66e #xa66f #xa670 #xa673 #xa674 #xa67e #xa67f #xa680 
     #xa681 #xa682 #xa683 #xa684 #xa685 #xa686 #xa687 #xa688 
     #xa689 #xa68a #xa68b #xa68c #xa68d #xa68e #xa68f #xa690 
     #xa691 #xa692 #xa693 #xa694 #xa695 #xa696 #xa697 #xa698 
     #xa699 #xa69a #xa69b #xa69c #xa69e #xa69f #xa6a0 #xa6e6 
     #xa6f0 #xa6f2 #xa6f8 #xa700 #xa717 #xa720 #xa722 #xa723 
     #xa724 #xa725 #xa726 #xa727 #xa728 #xa729 #xa72a #xa72b 
     #xa72c #xa72d #xa72e #xa72f #xa732 #xa733 #xa734 #xa735 
     #xa736 #xa737 #xa738 #xa739 #xa73a #xa73b #xa73c #xa73d 
     #xa73e #xa73f #xa740 #xa741 #xa742 #xa743 #xa744 #xa745 
     #xa746 #xa747 #xa748 #xa749 #xa74a #xa74b #xa74c #xa74d 
     #xa74e #xa74f #xa750 #xa751 #xa752 #xa753 #xa754 #xa755 
     #xa756 #xa757 #xa758 #xa759 #xa75a #xa75b #xa75c #xa75d 
     #xa75e #xa75f #xa760 #xa761 #xa762 #xa763 #xa764 #xa765 
     #xa766 #xa767 #xa768 #xa769 #xa76a #xa76b #xa76c #xa76d 
     #xa76e #xa76f #xa770 #xa771 #xa779 #xa77a #xa77b #xa77c 
     #xa77d #xa77f #xa780 #xa781 #xa782 #xa783 #xa784 #xa785 
     #xa786 #xa787 #xa788 #xa789 #xa78b #xa78c #xa78d #xa78e 
     #xa78f #xa790 #xa791 #xa792 #xa793 #xa796 #xa797 #xa798 
     #xa799 #xa79a #xa79b #xa79c #xa79d #xa79e #xa79f #xa7a0 
     #xa7a1 #xa7a2 #xa7a3 #xa7a4 #xa7a5 #xa7a6 #xa7a7 #xa7a8 
     #xa7a9 #xa7aa #xa7ae #xa7b0 #xa7b2 #xa7f7 #xa7f8 #xa7fa 
     #xa7fb #xa802 #xa803 #xa806 #xa807 #xa80b #xa80c #xa823 
     #xa825 #xa827 #xa828 #xa82c #xa830 #xa836 #xa838 #xa839 
     #xa83a #xa840 #xa874 #xa878 #xa880 #xa882 #xa8b4 #xa8c4 
     #xa8c5 #xa8ce #xa8d0 #xa8da #xa8e0 #xa8f2 #xa8f8 #xa8fb 
     #xa8fc #xa900 #xa90a #xa926 #xa92e #xa930 #xa947 #xa952 
     #xa954 #xa95f #xa960 #xa97d #xa980 #xa983 #xa984 #xa9b3 
     #xa9b4 #xa9b6 #xa9ba #xa9bc #xa9bd #xa9c1 #xa9ce #xa9cf 
     #xa9d0 #xa9da #xa9de #xa9e0 #xa9e5 #xa9e6 #xa9e7 #xa9f0 
     #xa9fa #xa9ff #xaa00 #xaa29 #xaa2f #xaa31 #xaa33 #xaa35 
     #xaa37 #xaa40 #xaa43 #xaa44 #xaa4c #xaa4d #xaa4e #xaa50 
     #xaa5a #xaa5c #xaa60 #xaa70 #xaa71 #xaa77 #xaa7a #xaa7b 
     #xaa7c #xaa7d #xaa7e #xaab0 #xaab1 #xaab2 #xaab5 #xaab7 
     #xaab9 #xaabe #xaac0 #xaac1 #xaac2 #xaac3 #xaadb #xaadd 
     #xaade #xaae0 #xaaeb #xaaec #xaaee #xaaf0 #xaaf2 #xaaf3 
     #xaaf5 #xaaf6 #xaaf7 #xab01 #xab07 #xab09 #xab0f #xab11 
     #xab17 #xab20 #xab27 #xab28 #xab2f #xab30 #xab5b #xab5c 
     #xab60 #xab64 #xab66 #xabc0 #xabe3 #xabe5 #xabe6 #xabe8 
     #xabe9 #xabeb #xabec #xabed #xabee #xabf0 #xabfa #xac00 
     #xd7a4 #xd7b0 #xd7c7 #xd7cb #xd7fc #xd800 #xe000 #xf900 
     #xfa6e #xfa70 #xfada #xfb00 #xfb07 #xfb13 #xfb18 #xfb1d 
     #xfb1e #xfb1f #xfb29 #xfb2a #xfb37 #xfb38 #xfb3d #xfb3e 
     #xfb3f #xfb40 #xfb42 #xfb43 #xfb45 #xfb46 #xfbb2 #xfbc2 
     #xfbd3 #xfd3e #xfd3f #xfd40 #xfd50 #xfd90 #xfd92 #xfdc8 
     #xfdf0 #xfdfc #xfdfd #xfdfe #xfe00 #xfe10 #xfe17 #xfe18 
     #xfe19 #xfe1a #xfe20 #xfe2e #xfe30 #xfe31 #xfe33 #xfe35 
     #xfe36 #xfe37 #xfe38 #xfe39 #xfe3a #xfe3b #xfe3c #xfe3d 
     #xfe3e #xfe3f #xfe40 #xfe41 #xfe42 #xfe43 #xfe44 #xfe45 
     #xfe47 #xfe48 #xfe49 #xfe4d #xfe50 #xfe53 #xfe54 #xfe58 
     #xfe59 #xfe5a #xfe5b #xfe5c #xfe5d #xfe5e #xfe5f #xfe62 
     #xfe63 #xfe64 #xfe67 #xfe68 #xfe69 #xfe6a #xfe6c #xfe70 
     #xfe75 #xfe76 #xfefd #xfeff #xff00 #xff01 #xff04 #xff05 
     #xff08 #xff09 #xff0a #xff0b #xff0c #xff0d #xff0e #xff10 
     #xff1a #xff1c #xff1f #xff21 #xff3b #xff3c #xff3d #xff3e 
     #xff3f #xff40 #xff41 #xff5b #xff5c #xff5d #xff5e #xff5f 
     #xff60 #xff61 #xff62 #xff63 #xff64 #xff66 #xff70 #xff71 
     #xff9e #xffa0 #xffbf #xffc2 #xffc8 #xffca #xffd0 #xffd2 
     #xffd8 #xffda #xffdd #xffe0 #xffe2 #xffe3 #xffe4 #xffe5 
     #xffe7 #xffe8 #xffe9 #xffed #xffef #xfff9 #xfffc #xfffe 
     #x10000 #x1000c #x1000d #x10027 #x10028 #x1003b #x1003c #x1003e 
     #x1003f #x1004e #x10050 #x1005e #x10080 #x100fb #x10100 #x10103 
     #x10107 #x10134 #x10137 #x10140 #x10175 #x10179 #x1018a #x1018c 
     #x1018d #x10190 #x1019c #x101a0 #x101a1 #x101d0 #x101fd #x101fe 
     #x10280 #x1029d #x102a0 #x102d1 #x102e0 #x102e1 #x102fc #x10300 
     #x10320 #x10324 #x10330 #x10341 #x10342 #x1034a #x1034b #x10350 
     #x10376 #x1037b #x10380 #x1039e #x1039f #x103a0 #x103c4 #x103c8 
     #x103d0 #x103d1 #x103d6 #x10400 #x10428 #x10450 #x1049e #x104a0 
     #x104aa #x10500 #x10528 #x10530 #x10564 #x1056f #x10570 #x10600 
     #x10737 #x10740 #x10756 #x10760 #x10768 #x10800 #x10806 #x10808 
     #x10809 #x1080a #x10836 #x10837 #x10839 #x1083c #x1083d #x1083f 
     #x10856 #x10857 #x10858 #x10860 #x10877 #x10879 #x10880 #x1089f 
     #x108a7 #x108b0 #x10900 #x10916 #x1091c #x1091f #x10920 #x1093a 
     #x1093f #x10940 #x10980 #x109b8 #x109be #x109c0 #x10a00 #x10a01 
     #x10a04 #x10a05 #x10a07 #x10a0c #x10a10 #x10a14 #x10a15 #x10a18 
     #x10a19 #x10a34 #x10a38 #x10a3b #x10a3f #x10a40 #x10a48 #x10a50 
     #x10a59 #x10a60 #x10a7d #x10a7f #x10a80 #x10a9d #x10aa0 #x10ac0 
     #x10ac8 #x10ac9 #x10ae5 #x10ae7 #x10aeb #x10af0 #x10af7 #x10b00 
     #x10b36 #x10b39 #x10b40 #x10b56 #x10b58 #x10b60 #x10b73 #x10b78 
     #x10b80 #x10b92 #x10b99 #x10b9d #x10ba9 #x10bb0 #x10c00 #x10c49 
     #x10e60 #x10e7f #x11000 #x11001 #x11002 #x11003 #x11038 #x11047 
     #x1104e #x11052 #x11066 #x11070 #x1107f #x11082 #x11083 #x110b0 
     #x110b3 #x110b7 #x110b9 #x110bb #x110bd #x110be #x110c2 #x110d0 
     #x110e9 #x110f0 #x110fa #x11100 #x11103 #x11127 #x1112c #x1112d 
     #x11135 #x11136 #x11140 #x11144 #x11150 #x11173 #x11174 #x11176 
     #x11177 #x11180 #x11182 #x11183 #x111b3 #x111b6 #x111bf #x111c1 
     #x111c5 #x111c9 #x111cd #x111ce #x111d0 #x111da #x111db #x111e1 
     #x111f5 #x11200 #x11212 #x11213 #x1122c #x1122f #x11232 #x11234 
     #x11235 #x11236 #x11238 #x1123e #x112b0 #x112df #x112e0 #x112e3 
     #x112eb #x112f0 #x112fa #x11301 #x11302 #x11304 #x11305 #x1130d 
     #x1130f #x11311 #x11313 #x11329 #x1132a #x11331 #x11332 #x11334 
     #x11335 #x1133a #x1133c #x1133d #x1133e #x11340 #x11341 #x11345 
     #x11347 #x11349 #x1134b #x1134e #x11357 #x11358 #x1135d #x11362 
     #x11364 #x11366 #x1136d #x11370 #x11375 #x11480 #x114b0 #x114b3 
     #x114b9 #x114ba #x114bb #x114bf #x114c1 #x114c2 #x114c4 #x114c6 
     #x114c7 #x114c8 #x114d0 #x114da #x11580 #x115af #x115b2 #x115b6 
     #x115b8 #x115bc #x115be #x115bf #x115c1 #x115ca #x11600 #x11630 
     #x11633 #x1163b #x1163d #x1163e #x1163f #x11641 #x11644 #x11645 
     #x11650 #x1165a #x11680 #x116ab #x116ac #x116ad #x116ae #x116b0 
     #x116b6 #x116b7 #x116b8 #x116c0 #x116ca #x118a0 #x118c0 #x118e0 
     #x118ea #x118f3 #x118ff #x11900 #x11ac0 #x11af9 #x12000 #x12399 
     #x12400 #x1246f #x12470 #x12475 #x13000 #x1342f #x16800 #x16a39 
     #x16a40 #x16a5f #x16a60 #x16a6a #x16a6e #x16a70 #x16ad0 #x16aee 
     #x16af0 #x16af5 #x16af6 #x16b00 #x16b30 #x16b37 #x16b3c #x16b40 
     #x16b44 #x16b45 #x16b46 #x16b50 #x16b5a #x16b5b #x16b62 #x16b63 
     #x16b78 #x16b7d #x16b90 #x16f00 #x16f45 #x16f50 #x16f51 #x16f7f 
     #x16f8f #x16f93 #x16fa0 #x1b000 #x1b002 #x1bc00 #x1bc6b #x1bc70 
     #x1bc7d #x1bc80 #x1bc89 #x1bc90 #x1bc9a #x1bc9c #x1bc9d #x1bc9f 
     #x1bca0 #x1bca4 #x1d000 #x1d0f6 #x1d100 #x1d127 #x1d129 #x1d165 
     #x1d167 #x1d16a #x1d16d #x1d173 #x1d17b #x1d183 #x1d185 #x1d18c 
     #x1d1aa #x1d1ae #x1d1de #x1d200 #x1d242 #x1d245 #x1d246 #x1d300 
     #x1d357 #x1d360 #x1d372 #x1d400 #x1d41a #x1d434 #x1d44e #x1d455 
     #x1d456 #x1d468 #x1d482 #x1d49c #x1d49d #x1d49e #x1d4a0 #x1d4a2 
     #x1d4a3 #x1d4a5 #x1d4a7 #x1d4a9 #x1d4ad #x1d4ae #x1d4b6 #x1d4ba 
     #x1d4bb #x1d4bc #x1d4bd #x1d4c4 #x1d4c5 #x1d4d0 #x1d4ea #x1d504 
     #x1d506 #x1d507 #x1d50b #x1d50d #x1d515 #x1d516 #x1d51d #x1d51e 
     #x1d538 #x1d53a #x1d53b #x1d53f #x1d540 #x1d545 #x1d546 #x1d547 
     #x1d54a #x1d551 #x1d552 #x1d56c #x1d586 #x1d5a0 #x1d5ba #x1d5d4 
     #x1d5ee #x1d608 #x1d622 #x1d63c #x1d656 #x1d670 #x1d68a #x1d6a6 
     #x1d6a8 #x1d6c1 #x1d6c2 #x1d6db #x1d6dc #x1d6e2 #x1d6fb #x1d6fc 
     #x1d715 #x1d716 #x1d71c #x1d735 #x1d736 #x1d74f #x1d750 #x1d756 
     #x1d76f #x1d770 #x1d789 #x1d78a #x1d790 #x1d7a9 #x1d7aa #x1d7c3 
     #x1d7c4 #x1d7ca #x1d7cb #x1d7cc #x1d7ce #x1d800 #x1e800 #x1e8c5 
     #x1e8c7 #x1e8d0 #x1e8d7 #x1ee00 #x1ee04 #x1ee05 #x1ee20 #x1ee21 
     #x1ee23 #x1ee24 #x1ee25 #x1ee27 #x1ee28 #x1ee29 #x1ee33 #x1ee34 
     #x1ee38 #x1ee39 #x1ee3a #x1ee3b #x1ee3c #x1ee42 #x1ee43 #x1ee47 
     #x1ee48 #x1ee49 #x1ee4a #x1ee4b #x1ee4c #x1ee4d #x1ee50 #x1ee51 
     #x1ee53 #x1ee54 #x1ee55 #x1ee57 #x1ee58 #x1ee59 #x1ee5a #x1ee5b 
     #x1ee5c #x1ee5d #x1ee5e #x1ee5f #x1ee60 #x1ee61 #x1ee63 #x1ee64 
     #x1ee65 #x1ee67 #x1ee6b #x1ee6c #x1ee73 #x1ee74 #x1ee78 #x1ee79 
     #x1ee7d #x1ee7e #x1ee7f #x1ee80 #x1ee8a #x1ee8b #x1ee9c #x1eea1 
     #x1eea4 #x1eea5 #x1eeaa #x1eeab #x1eebc #x1eef0 #x1eef2 #x1f000 
     #x1f02c #x1f030 #x1f094 #x1f0a0 #x1f0af #x1f0b1 #x1f0c0 #x1f0c1 
     #x1f0d0 #x1f0d1 #x1f0f6 #x1f100 #x1f10d #x1f110 #x1f12f #x1f130 
     #x1f16c #x1f170 #x1f19b #x1f1e6 #x1f203 #x1f210 #x1f23b #x1f240 
     #x1f249 #x1f250 #x1f252 #x1f300 #x1f32d #x1f330 #x1f37e #x1f380 
     #x1f3cf #x1f3d4 #x1f3f8 #x1f400 #x1f4ff #x1f500 #x1f54b #x1f550 
     #x1f57a #x1f57b #x1f5a4 #x1f5a5 #x1f643 #x1f645 #x1f6d0 #x1f6e0 
     #x1f6ed #x1f6f0 #x1f6f4 #x1f700 #x1f774 #x1f780 #x1f7d5 #x1f800 
     #x1f80c #x1f810 #x1f848 #x1f850 #x1f85a #x1f860 #x1f888 #x1f890 
     #x1f8ae #x20000 #x2a6d7 #x2a700 #x2b735 #x2b740 #x2b81e #x2f800 
     #x2fa1e #xe0001 #xe0002 #xe0020 #xe0080 #xe0100 #xe01f0 #xf0000 
     #xffffe #x100000 #x10fffe ))

; The following vector contains the general category for
; characters whose Unicode scalar value is less than 256.
;
; This table contains 256 entries.

(define general-category-indices-for-common-characters
  (u8-list->bytevector
   (map
    general-category-symbol->index
    '(
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Zs Po Po Po Sc Po Po Po Ps Pe Po Sm Po Pd Po Po 
       Nd Nd Nd Nd Nd Nd Nd Nd Nd Nd Po Po Sm Sm Sm Po 
       Po Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu 
       Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Ps Po Pe Sk Pc 
       Sk Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll 
       Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ps Sm Pe Sm Cc 
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Zs Po Sc Sc Sc Sc So Po Sk So Lo Pi Sm Cf So Sk 
       So Sm No No Sk Ll Po Po Sk No Lo Pf No No No Po 
       Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu 
       Lu Lu Lu Lu Lu Lu Lu Sm Lu Lu Lu Lu Lu Lu Lu Ll 
       Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll 
       Ll Ll Ll Ll Ll Ll Ll Sm Ll Ll Ll Ll Ll Ll Ll Ll ))))


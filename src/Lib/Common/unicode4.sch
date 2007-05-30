($$trace "unicode4")

; Copyright (c) 2006 Michael Sperber
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. The name of the authors may not be used to endorse or promote products
;    derived from this software without specific prior written permission.
; 
; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; From: Michael Sperber <sperber@informatik.uni-tuebingen.de>
; To: "William D. Clinger" <will@ccs.neu.edu>
; Subject: Unicode normalization
; Date: Tue, 20 Jun 2006 19:59:15 +0200
; 
; 
; I've attached the code I wrote....
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Revised by Will Clinger, 20 June - 11 September 2006.
;
; Modifications include:
;     deletion of tables (that occupied at least 136208 bytes)
;     addition of tables (that occupy about 60 kilobytes)
;     deletion of several procedures:
;         sv-normalization-info
;         sv-has-canonical-decomposition?
;         sv-has-compatibility-decomposition?
;         sv-canonical-decomposition-encoding
;     complete rewrites of several procedures:
;         binary-search (since moved to unicode0.sch)
;         sv-combining-class
;         reverse-decomposition
;         compose-2
;     addition of several procedures:
;         sv-decomposition-index-of-index
;         sv-decomposition-sequence-range
;
; The above changes reduced the table sizes by a factor of 2,
; and sped up the tests in NormalizationTests.txt by a factor of 2.
;
; The tables in this file were generated from the
; Unicode Character Database, revision 5.0.0.
;
; This file does not rely on any lexical syntax for
; non-Ascii characters and strings.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(library (proto-unicode4)
;
;  (export
;
;    string-normalize-nfd
;    string-normalize-nfkd
;    string-normalize-nfc
;    string-normalize-nfkc)
;
;  (import (r6rs base)
;          (r6rs bytes)
;          (proto-unicode0)
;          (proto-unicode1)
;          (proto-unicode2)
;          (proto-unicode3))

; Normalization Form D (NFD): Canonical Decomposition

(define (string-normalize-nfd s)
  (decompose #f s))

; Normalization Form KD (NFKD): Compatibility Decomposition

(define (string-normalize-nfkd s)
  (decompose #t s))

; Normalization Form C (NFC): Canonical Decomposition,
; followed by Canonical Composition

(define (string-normalize-nfc s)
  (compose-after string-normalize-nfd s))

; Normalization Form KC (NFKC): Compatibility Decomposition,
; followed by Canonical Composition

(define (string-normalize-nfkc s)
  (compose-after string-normalize-nfkd s))

(define (decompose compat? s)
  (let ((size (string-length s)))
    (define (fast-loop i)
      (if (= i size)
          s
          (let* ((c (string-ref s i))
                 (sv (char->integer c)))
            (if (< sv 128)
                (fast-loop (+ i 1))
                (let ((index2 (sv-decomposition-index-of-index sv))
                      (class (sv-combining-class sv)))
                  (if (or index2 (< 0 class) (sv-hangul-syllable? sv))
                      (slow-loop 0 '())
                      (fast-loop (+ i 1))))))))
    (define (slow-loop i rev-chars)
      (if (>= i size)
          (reorder-according-to-combining-class!
           (list->string (reverse rev-chars)))
          (let* ((c (string-ref s i))
                 (sv (char->integer c)))
            (if (sv-hangul-syllable? sv)
                (slow-loop (+ 1 i)
                           (prepend-reverse-jamo-decomposition sv rev-chars))
                (slow-loop (+ 1 i)
                           (append (reverse-decomposition compat? sv)
                                   rev-chars))))))
    (fast-loop 0)))

; bubble-sort decompositions according to combining class
; returns the modified string

(define (reorder-according-to-combining-class! s)
  (let ((size (string-length s)))
    (let repeat ()
      (let loop ((i 0)
		 (swapped? #f))
	(cond
	 ((< (+ i 1) size)
	  (let ((sv-i (char->integer (string-ref s i)))
		(sv-i+1 (char->integer (string-ref s (+ i 1)))))
	    (let ((cc-i (sv-combining-class sv-i))
		  (cc-i+1 (sv-combining-class sv-i+1)))
	      (if (and (not (zero? cc-i))
		       (not (zero? cc-i+1))
		       (< cc-i+1 cc-i))
		  (begin
		    (string-set! s i (integer->char sv-i+1))
		    (string-set! s (+ i 1) (integer->char sv-i))
		    (loop (+ 1 i) #t))
		  (loop (+ 1 i) swapped?)))))
	 (swapped? (repeat))
	 (else
 s))))))

; Given a normalizer that acts as the identity on Ascii strings,
; and a string,
; returns the canonical composition of the normalized string.

(define (compose-after normalizer s)
  (let ((size (string-length s)))
    (define (fast-loop i)
      (if (= i size)
          s
          (let* ((c (string-ref s i))
                 (sv (char->integer c)))
            (if (< sv 128)
                (fast-loop (+ i 1))
                (slow-case)))))
    (define (slow-case)
      (let ((s2 (normalizer s)))
        (if (and (eq? s s2))
            (compose s)
            (compose! s2))))
    (fast-loop 0)))

; Given a string, returns its canonical composition
; without modifying the given string.

(define (compose s)
  (let ((size (string-length s)))
    (define (fast-loop i)
      (if (= i size)
          s
          (let* ((c (string-ref s i))
                 (sv (char->integer c)))
            (cond ((< sv #x300)              ; implies combining class is 0
                   (fast-loop (+ i 1)))
                  (else
                   (compose! (string-copy s)))))))
    (fast-loop 0)))

; Given a mutable string, performs canonical composition on the
; string (destructively) and returns the string or a substring.

(define (compose! s)
  (let ((size (string-length s)))
    (let loop ((p 0)    ; output index for finished combined character
               (p2 1)         ; output index for uncombined characters
               (i 0)              ; input index for starting character
               (j 1))      ; input index for characters to be combined 

      ; presumed to be invariant:
      ; p <= i <= size
      ; i < j                 (Note: j may exceed size)
      ; p < p2                (Note: p2 may exceed size)

      (if (< i size)
	  (let* ((sv-i (char->integer (string-ref s i)))
		 (cc-i (sv-combining-class sv-i)))
	    (if (zero? cc-i)
		(if (>= j size)
		    (begin
		      ;; we're done combining with sv-i; skip past
		      ;; combining sequences in both input and output
		      (string-set! s p (integer->char sv-i))
		      (substring s 0 (min size (max (+ p 1) p2))))
		    (let* ((sv-j (char->integer (string-ref s j)))
			   (cc-j (sv-combining-class sv-j)))
		      (cond
		       ((and (= j (+ i 1))
			     (sv-jamo-initial-consonant? sv-i)
			     (sv-jamo-vowel? sv-j))
			;; need Hangul composition
			(if (and (< (+ j 1) size)
				 (sv-jamo-trailing-consonant?
				  (char->integer (string-ref s (+ j 1)))))
			    ;; 3-char composition
			    (let ((composite
				   (+ jamo-syllable-start
				      (* (- sv-i jamo-initial-consonant-start)
					 jamo-syllable-per-count)
				      (* (- sv-j jamo-vowel-start)
					 jamo-trailing-consonant-count)
				      (- (char->integer
                                          (string-ref s (+ j 1)))
					 jamo-trailing-consonant-start))))
			      (string-set! s i (integer->char composite))
			      (loop p p2 i (+ j 2)))
			    ;; 2-char composition
			    (let ((composite
				   (+ jamo-syllable-start
				      (* (- sv-i jamo-initial-consonant-start)
					 jamo-syllable-per-count)
				      (* (- sv-j jamo-vowel-start)
					 jamo-trailing-consonant-count))))
			      (string-set! s i (integer->char composite))
			      (loop p p2 i (+ j 1)))))
		       ((let ((previous-cc
                               (sv-combining-class
                                (char->integer (string-ref s (- j 1))))))
			  ;; check if blocked
			  (and (<= previous-cc cc-j)
			       (compose-2 sv-i sv-j)))
			;; we can combine; store result temporarily at i;
			;; advance past the combining mark
			=> (lambda (combined)
			     (string-set! s i (integer->char combined))
			     (loop p p2 i (+ j 1))))
		       ((zero? cc-j)
			;; both are combining class 0; we're done
			;; combining with sv-i; skip past combining sequences
			;; in both input and output
			(string-set! s p (integer->char sv-i))
			(loop p2 (+ p2 1) j (+ 1 j)))
		       (else
			(let skip ((j j) (p2 p2))
			  (if (< j size)
			      (let ((sv-j
                                     (char->integer (string-ref s j))))
				(if (= (sv-combining-class sv-j) cc-j)
				    (begin
				      (string-set! s
                                                   p2
                                                   (integer->char sv-j))
				      (skip (+ j 1) (+ p2 1)))
				    (loop p p2 i j)))
			      (loop p p2 i j)))))))
		(loop (+ p 1) (+ p2 1) (+ i 1) (+ j 1))))
	  (substring s 0 (min size p2))))))

;; Hangul constants
;; from Unicode Standard Annex #15

(define jamo-syllable-start #xAC00)
(define jamo-initial-consonant-start #x1100)
(define jamo-initial-consonant-count 19)
(define jamo-initial-consonant-end
  (+ jamo-initial-consonant-start jamo-initial-consonant-count))
(define jamo-trailing-consonant-start #x11A7)
(define jamo-trailing-consonant-count 28)
(define jamo-trailing-consonant-end
  (+ jamo-trailing-consonant-start jamo-trailing-consonant-count))
(define jamo-vowel-start #x1161)
(define jamo-vowel-count 21)
(define jamo-vowel-end (+ jamo-vowel-start jamo-vowel-count))

;; number of syllables with a given initial consonant
(define jamo-syllable-per-count
  (* jamo-vowel-count jamo-trailing-consonant-count))
(define jamo-syllable-count
  (* jamo-initial-consonant-count jamo-syllable-per-count))
(define jamo-syllable-end (+ jamo-syllable-start jamo-syllable-count))

(define (sv-jamo-initial-consonant? sv)
  (and (>= sv jamo-initial-consonant-start)
       (< sv jamo-initial-consonant-end)))
(define (sv-jamo-trailing-consonant? sv)
  (and (>= sv jamo-trailing-consonant-start)
       (< sv jamo-trailing-consonant-end)))
(define (sv-jamo-vowel? sv)
  (and (>= sv jamo-vowel-start)
       (< sv jamo-vowel-end)))

(define (sv-hangul-syllable? sv)
  (and (>= sv  jamo-syllable-start)
       (< sv jamo-syllable-end)))

(define (prepend-reverse-jamo-decomposition sv rev-chars)
  (let* ((offset (- sv jamo-syllable-start))
	 (l (+ jamo-initial-consonant-start
	       (quotient offset jamo-syllable-per-count)))
	 (v (+ jamo-vowel-start
	       (quotient (modulo offset jamo-syllable-per-count)
			 jamo-trailing-consonant-count)))
	 (t (+ jamo-trailing-consonant-start
	       (modulo offset jamo-trailing-consonant-count)))
	 (either-way
	  (cons (integer->char v)
		(cons (integer->char l)
		      rev-chars))))
    (if (= t jamo-trailing-consonant-start)
	either-way
	(cons (integer->char t) either-way))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright 2006 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a Unicode scalar value, returns its combining class.

(define (sv-combining-class s)
  (let ((index (binary-search s combining-class-is-nonzero)))
    (if index
        (bytevector-u8-ref combining-class-values index)
        0)))

; Given a Unicode scalar value, returns the index of the index
; of its decomposition sequence if it has one, else returns #f.

(define (sv-decomposition-index-of-index s)
  (let ((index
         (if (< s 65536)
             (binary-search-16bit s decomposition-chars-16bit)
             (let ((index0 (binary-search s decomposition-chars-morebits)))
               (if index0
                   (+ index0
                      (quotient (bytevector-length decomposition-chars-16bit)
                                2))
                   index0)))))
    (if index
        (+ index index)
        #f)))

; Given the index of the index of a decomposition sequence,
; returns two values: the starting index (inclusive) and
; ending index (exclusive) of its decomposition sequence.

(define (sv-decomposition-sequence-range index)
  (define (decode hi lo)
    (+ (* 256 (if (< hi 128) hi (- hi 128))) lo))
  (let* ((hi0 (bytevector-u8-ref decomposition-indexes index))
         (lo0 (bytevector-u8-ref decomposition-indexes (+ index 1)))
         (hi1 (bytevector-u8-ref decomposition-indexes (+ index 2)))
         (lo1 (bytevector-u8-ref decomposition-indexes (+ index 3))))
    (values (decode hi0 lo0)
            (decode hi1 lo1))))

; The following procedures aren't used at present, but
; might be used by some future rewrite.
; For now, they are commented out.

'
(begin

; Given a Unicode scalar value, returns the index of its
; canonical decomposition sequence if it has one, else returns #f.

(define (sv-canonical-decomposition-index s)
  (let ((index (sv-decomposition-index-of-index s)))
    (if index
        (let* ((hi (bytevector-u8-ref decomposition-indexes index))
               (lo (bytevector-u8-ref decomposition-indexes (+ index 1))))
          (if (< hi 128)
              (+ (* 256 hi) lo)
              #f))
        #f)))

; Given a Unicode scalar value, returns the index of its
; compatibility decomposition sequence if it has one, else returns #f.

(define (sv-compatibility-decomposition-index s)
  (let ((index (sv-decomposition-index-of-index s)))
    (if index
        (let* ((hi (bytevector-u8-ref decomposition-indexes index))
               (lo (bytevector-u8-ref decomposition-indexes (+ index 1))))
          (if (>= hi 128)
              (+ (* 256 (- hi 128)) lo)
              #f))
        #f)))

; Given a Unicode scalar value, returns a boolean indicating
; whether it has a canonical decomposition.

(define (sv-has-canonical-decomposition? s)
  (if (sv-canonical-decomposition-index s)
      #t
      #f))

; Given a Unicode scalar value, returns a boolean indicating
; whether it has a compatibility decomposition.

(define (sv-has-compatibility-decomposition? s)
  (if (sv-compatibility-decomposition-index s)
      #t
      #f))

)

; End of '(begin ...) comment.

; Given a boolean to indicate whether compatibility
; decompositions are to be used, and a scalar value,
; returns a list of the characters (not scalar values!),
; in reverse order, to which the given scalar value decomposes.

(define (reverse-decomposition compat? sv)
  (let recur ((sv sv))
    (let ((index-of-index (sv-decomposition-index-of-index sv)))
      (if (and index-of-index
               (or compat?
                   (< (bytevector-u8-ref decomposition-indexes index-of-index)
                      128)))
          (call-with-values
           (lambda () (sv-decomposition-sequence-range index-of-index))
           (lambda (pos end)
             (let loop ((index pos)
                        (rev '()))
               (if (>= index end)
                   rev
                   (loop (+ 1 index)
                         (append
                          (recur
                           (vector-ref decomposition-sequences index))
                          rev))))))
          (list (integer->char sv))))))


; Given two Unicode scalar values,
; returns their canonical composition as a scalar value
; if it exists; otherwise returns #f.

(define (compose-2 sv1 sv2)
  (let ((probe (binary-search-16bit sv2 composition-modifiers)))
    (if probe
        (let* ((entry (vector-ref canonical-compositions probe))
               (index (binary-search-16bit sv1 (car entry))))
          (if index
              (let* ((compositions (cadr entry))
                     (i (+ index index))
                     (hi (bytevector-u8-ref compositions i))
                     (lo (bytevector-u8-ref compositions (+ i 1))))
                (+ (* 256 hi) lo))
              #f))
        #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The following tables were generated from
; UnicodeData.txt and SpecialCasing.txt.
; Use parseUCD.sch to regenerate these tables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This vector contains all code points,
; in increasing order, that have a nonzero
; combining class.
;
; This table contains 418 elements.

(define combining-class-is-nonzero
  '#(
     #x300 #x301 #x302 #x303 #x304 #x305 #x306 #x307 
     #x308 #x309 #x30a #x30b #x30c #x30d #x30e #x30f 
     #x310 #x311 #x312 #x313 #x314 #x315 #x316 #x317 
     #x318 #x319 #x31a #x31b #x31c #x31d #x31e #x31f 
     #x320 #x321 #x322 #x323 #x324 #x325 #x326 #x327 
     #x328 #x329 #x32a #x32b #x32c #x32d #x32e #x32f 
     #x330 #x331 #x332 #x333 #x334 #x335 #x336 #x337 
     #x338 #x339 #x33a #x33b #x33c #x33d #x33e #x33f 
     #x340 #x341 #x342 #x343 #x344 #x345 #x346 #x347 
     #x348 #x349 #x34a #x34b #x34c #x34d #x34e #x350 
     #x351 #x352 #x353 #x354 #x355 #x356 #x357 #x358 
     #x359 #x35a #x35b #x35c #x35d #x35e #x35f #x360 
     #x361 #x362 #x363 #x364 #x365 #x366 #x367 #x368 
     #x369 #x36a #x36b #x36c #x36d #x36e #x36f #x483 
     #x484 #x485 #x486 #x591 #x592 #x593 #x594 #x595 
     #x596 #x597 #x598 #x599 #x59a #x59b #x59c #x59d 
     #x59e #x59f #x5a0 #x5a1 #x5a2 #x5a3 #x5a4 #x5a5 
     #x5a6 #x5a7 #x5a8 #x5a9 #x5aa #x5ab #x5ac #x5ad 
     #x5ae #x5af #x5b0 #x5b1 #x5b2 #x5b3 #x5b4 #x5b5 
     #x5b6 #x5b7 #x5b8 #x5b9 #x5ba #x5bb #x5bc #x5bd 
     #x5bf #x5c1 #x5c2 #x5c4 #x5c5 #x5c7 #x610 #x611 
     #x612 #x613 #x614 #x615 #x64b #x64c #x64d #x64e 
     #x64f #x650 #x651 #x652 #x653 #x654 #x655 #x656 
     #x657 #x658 #x659 #x65a #x65b #x65c #x65d #x65e 
     #x670 #x6d6 #x6d7 #x6d8 #x6d9 #x6da #x6db #x6dc 
     #x6df #x6e0 #x6e1 #x6e2 #x6e3 #x6e4 #x6e7 #x6e8 
     #x6ea #x6eb #x6ec #x6ed #x711 #x730 #x731 #x732 
     #x733 #x734 #x735 #x736 #x737 #x738 #x739 #x73a 
     #x73b #x73c #x73d #x73e #x73f #x740 #x741 #x742 
     #x743 #x744 #x745 #x746 #x747 #x748 #x749 #x74a 
     #x7eb #x7ec #x7ed #x7ee #x7ef #x7f0 #x7f1 #x7f2 
     #x7f3 #x93c #x94d #x951 #x952 #x953 #x954 #x9bc 
     #x9cd #xa3c #xa4d #xabc #xacd #xb3c #xb4d #xbcd 
     #xc4d #xc55 #xc56 #xcbc #xccd #xd4d #xdca #xe38 
     #xe39 #xe3a #xe48 #xe49 #xe4a #xe4b #xeb8 #xeb9 
     #xec8 #xec9 #xeca #xecb #xf18 #xf19 #xf35 #xf37 
     #xf39 #xf71 #xf72 #xf74 #xf7a #xf7b #xf7c #xf7d 
     #xf80 #xf82 #xf83 #xf84 #xf86 #xf87 #xfc6 #x1037 
     #x1039 #x135f #x1714 #x1734 #x17d2 #x17dd #x18a9 #x1939 
     #x193a #x193b #x1a17 #x1a18 #x1b34 #x1b44 #x1b6b #x1b6c 
     #x1b6d #x1b6e #x1b6f #x1b70 #x1b71 #x1b72 #x1b73 #x1dc0 
     #x1dc1 #x1dc2 #x1dc3 #x1dc4 #x1dc5 #x1dc6 #x1dc7 #x1dc8 
     #x1dc9 #x1dca #x1dfe #x1dff #x20d0 #x20d1 #x20d2 #x20d3 
     #x20d4 #x20d5 #x20d6 #x20d7 #x20d8 #x20d9 #x20da #x20db 
     #x20dc #x20e1 #x20e5 #x20e6 #x20e7 #x20e8 #x20e9 #x20ea 
     #x20eb #x20ec #x20ed #x20ee #x20ef #x302a #x302b #x302c 
     #x302d #x302e #x302f #x3099 #x309a #xa806 #xfb1e #xfe20 
     #xfe21 #xfe22 #xfe23 #x10a0d #x10a0f #x10a38 #x10a39 #x10a3a 
     #x10a3f #x1d165 #x1d166 #x1d167 #x1d168 #x1d169 #x1d16d #x1d16e 
     #x1d16f #x1d170 #x1d171 #x1d172 #x1d17b #x1d17c #x1d17d #x1d17e 
     #x1d17f #x1d180 #x1d181 #x1d182 #x1d185 #x1d186 #x1d187 #x1d188 
     #x1d189 #x1d18a #x1d18b #x1d1aa #x1d1ab #x1d1ac #x1d1ad #x1d242 
     #x1d243 #x1d244 ))

; This bytevector contains the combining classes
; for the code points in the above vector.
;
; This table contains 418 elements.

(define combining-class-values
  ;'#vu8(
  (list->bytevector '(
        230 230 230 230 230 230 230 230 
        230 230 230 230 230 230 230 230 
        230 230 230 230 230 232 220 220 
        220 220 232 216 220 220 220 220 
        220 202 202 220 220 220 220 202 
        202 220 220 220 220 220 220 220 
        220 220 220 220 1 1 1 1 
        1 220 220 220 220 230 230 230 
        230 230 230 230 230 240 230 220 
        220 220 230 230 230 220 220 230 
        230 230 220 220 220 220 230 232 
        220 220 230 233 234 234 233 234 
        234 233 230 230 230 230 230 230 
        230 230 230 230 230 230 230 230 
        230 230 230 220 230 230 230 230 
        220 230 230 230 222 220 230 230 
        230 230 230 230 220 220 220 220 
        220 220 230 230 220 230 230 222 
        228 230 10 11 12 13 14 15 
        16 17 18 19 19 20 21 22 
        23 24 25 230 220 18 230 230 
        230 230 230 230 27 28 29 30 
        31 32 33 34 230 230 220 220 
        230 230 230 230 230 220 230 230 
        35 230 230 230 230 230 230 230 
        230 230 230 230 220 230 230 230 
        220 230 230 220 36 230 220 230 
        230 220 230 230 220 220 220 230 
        220 220 230 220 230 230 230 220 
        230 220 230 220 230 220 230 230 
        230 230 230 230 230 230 230 220 
        230 7 9 230 220 230 230 7 
        9 7 9 7 9 7 9 9 
        9 84 91 7 9 9 9 103 
        103 9 107 107 107 107 118 118 
        122 122 122 122 220 220 220 220 
        216 129 130 132 130 130 130 130 
        130 230 230 9 230 230 220 7 
        9 230 9 9 9 230 228 222 
        230 220 230 220 7 9 230 220 
        230 230 230 230 230 230 230 230 
        230 220 230 230 230 230 230 230 
        230 220 230 220 230 230 1 1 
        230 230 230 230 1 1 1 230 
        230 230 1 1 230 220 230 1 
        1 220 220 220 220 218 228 232 
        222 224 224 8 8 9 26 230 
        230 230 230 220 230 230 1 220 
        9 216 216 1 1 1 226 216 
        216 216 216 216 220 220 220 220 
        220 220 220 220 230 230 230 230 
        230 220 220 230 230 230 230 230 
        230 230 ))
)

; This bytevector uses two bytes per code point
; to list 16-bit code points, in increasing order,
; that have a canonical or compatibility decomposition.
;
; This table contains 7702 elements.

(define decomposition-chars-16bit
  ;'#vu8(
  (list->bytevector '(
        #x0 #xa0 #x0 #xa8 #x0 #xaa #x0 #xaf 
        #x0 #xb2 #x0 #xb3 #x0 #xb4 #x0 #xb5 
        #x0 #xb8 #x0 #xb9 #x0 #xba #x0 #xbc 
        #x0 #xbd #x0 #xbe #x0 #xc0 #x0 #xc1 
        #x0 #xc2 #x0 #xc3 #x0 #xc4 #x0 #xc5 
        #x0 #xc7 #x0 #xc8 #x0 #xc9 #x0 #xca 
        #x0 #xcb #x0 #xcc #x0 #xcd #x0 #xce 
        #x0 #xcf #x0 #xd1 #x0 #xd2 #x0 #xd3 
        #x0 #xd4 #x0 #xd5 #x0 #xd6 #x0 #xd9 
        #x0 #xda #x0 #xdb #x0 #xdc #x0 #xdd 
        #x0 #xe0 #x0 #xe1 #x0 #xe2 #x0 #xe3 
        #x0 #xe4 #x0 #xe5 #x0 #xe7 #x0 #xe8 
        #x0 #xe9 #x0 #xea #x0 #xeb #x0 #xec 
        #x0 #xed #x0 #xee #x0 #xef #x0 #xf1 
        #x0 #xf2 #x0 #xf3 #x0 #xf4 #x0 #xf5 
        #x0 #xf6 #x0 #xf9 #x0 #xfa #x0 #xfb 
        #x0 #xfc #x0 #xfd #x0 #xff #x1 #x0 
        #x1 #x1 #x1 #x2 #x1 #x3 #x1 #x4 
        #x1 #x5 #x1 #x6 #x1 #x7 #x1 #x8 
        #x1 #x9 #x1 #xa #x1 #xb #x1 #xc 
        #x1 #xd #x1 #xe #x1 #xf #x1 #x12 
        #x1 #x13 #x1 #x14 #x1 #x15 #x1 #x16 
        #x1 #x17 #x1 #x18 #x1 #x19 #x1 #x1a 
        #x1 #x1b #x1 #x1c #x1 #x1d #x1 #x1e 
        #x1 #x1f #x1 #x20 #x1 #x21 #x1 #x22 
        #x1 #x23 #x1 #x24 #x1 #x25 #x1 #x28 
        #x1 #x29 #x1 #x2a #x1 #x2b #x1 #x2c 
        #x1 #x2d #x1 #x2e #x1 #x2f #x1 #x30 
        #x1 #x32 #x1 #x33 #x1 #x34 #x1 #x35 
        #x1 #x36 #x1 #x37 #x1 #x39 #x1 #x3a 
        #x1 #x3b #x1 #x3c #x1 #x3d #x1 #x3e 
        #x1 #x3f #x1 #x40 #x1 #x43 #x1 #x44 
        #x1 #x45 #x1 #x46 #x1 #x47 #x1 #x48 
        #x1 #x49 #x1 #x4c #x1 #x4d #x1 #x4e 
        #x1 #x4f #x1 #x50 #x1 #x51 #x1 #x54 
        #x1 #x55 #x1 #x56 #x1 #x57 #x1 #x58 
        #x1 #x59 #x1 #x5a #x1 #x5b #x1 #x5c 
        #x1 #x5d #x1 #x5e #x1 #x5f #x1 #x60 
        #x1 #x61 #x1 #x62 #x1 #x63 #x1 #x64 
        #x1 #x65 #x1 #x68 #x1 #x69 #x1 #x6a 
        #x1 #x6b #x1 #x6c #x1 #x6d #x1 #x6e 
        #x1 #x6f #x1 #x70 #x1 #x71 #x1 #x72 
        #x1 #x73 #x1 #x74 #x1 #x75 #x1 #x76 
        #x1 #x77 #x1 #x78 #x1 #x79 #x1 #x7a 
        #x1 #x7b #x1 #x7c #x1 #x7d #x1 #x7e 
        #x1 #x7f #x1 #xa0 #x1 #xa1 #x1 #xaf 
        #x1 #xb0 #x1 #xc4 #x1 #xc5 #x1 #xc6 
        #x1 #xc7 #x1 #xc8 #x1 #xc9 #x1 #xca 
        #x1 #xcb #x1 #xcc #x1 #xcd #x1 #xce 
        #x1 #xcf #x1 #xd0 #x1 #xd1 #x1 #xd2 
        #x1 #xd3 #x1 #xd4 #x1 #xd5 #x1 #xd6 
        #x1 #xd7 #x1 #xd8 #x1 #xd9 #x1 #xda 
        #x1 #xdb #x1 #xdc #x1 #xde #x1 #xdf 
        #x1 #xe0 #x1 #xe1 #x1 #xe2 #x1 #xe3 
        #x1 #xe6 #x1 #xe7 #x1 #xe8 #x1 #xe9 
        #x1 #xea #x1 #xeb #x1 #xec #x1 #xed 
        #x1 #xee #x1 #xef #x1 #xf0 #x1 #xf1 
        #x1 #xf2 #x1 #xf3 #x1 #xf4 #x1 #xf5 
        #x1 #xf8 #x1 #xf9 #x1 #xfa #x1 #xfb 
        #x1 #xfc #x1 #xfd #x1 #xfe #x1 #xff 
        #x2 #x0 #x2 #x1 #x2 #x2 #x2 #x3 
        #x2 #x4 #x2 #x5 #x2 #x6 #x2 #x7 
        #x2 #x8 #x2 #x9 #x2 #xa #x2 #xb 
        #x2 #xc #x2 #xd #x2 #xe #x2 #xf 
        #x2 #x10 #x2 #x11 #x2 #x12 #x2 #x13 
        #x2 #x14 #x2 #x15 #x2 #x16 #x2 #x17 
        #x2 #x18 #x2 #x19 #x2 #x1a #x2 #x1b 
        #x2 #x1e #x2 #x1f #x2 #x26 #x2 #x27 
        #x2 #x28 #x2 #x29 #x2 #x2a #x2 #x2b 
        #x2 #x2c #x2 #x2d #x2 #x2e #x2 #x2f 
        #x2 #x30 #x2 #x31 #x2 #x32 #x2 #x33 
        #x2 #xb0 #x2 #xb1 #x2 #xb2 #x2 #xb3 
        #x2 #xb4 #x2 #xb5 #x2 #xb6 #x2 #xb7 
        #x2 #xb8 #x2 #xd8 #x2 #xd9 #x2 #xda 
        #x2 #xdb #x2 #xdc #x2 #xdd #x2 #xe0 
        #x2 #xe1 #x2 #xe2 #x2 #xe3 #x2 #xe4 
        #x3 #x40 #x3 #x41 #x3 #x43 #x3 #x44 
        #x3 #x74 #x3 #x7a #x3 #x7e #x3 #x84 
        #x3 #x85 #x3 #x86 #x3 #x87 #x3 #x88 
        #x3 #x89 #x3 #x8a #x3 #x8c #x3 #x8e 
        #x3 #x8f #x3 #x90 #x3 #xaa #x3 #xab 
        #x3 #xac #x3 #xad #x3 #xae #x3 #xaf 
        #x3 #xb0 #x3 #xca #x3 #xcb #x3 #xcc 
        #x3 #xcd #x3 #xce #x3 #xd0 #x3 #xd1 
        #x3 #xd2 #x3 #xd3 #x3 #xd4 #x3 #xd5 
        #x3 #xd6 #x3 #xf0 #x3 #xf1 #x3 #xf2 
        #x3 #xf4 #x3 #xf5 #x3 #xf9 #x4 #x0 
        #x4 #x1 #x4 #x3 #x4 #x7 #x4 #xc 
        #x4 #xd #x4 #xe #x4 #x19 #x4 #x39 
        #x4 #x50 #x4 #x51 #x4 #x53 #x4 #x57 
        #x4 #x5c #x4 #x5d #x4 #x5e #x4 #x76 
        #x4 #x77 #x4 #xc1 #x4 #xc2 #x4 #xd0 
        #x4 #xd1 #x4 #xd2 #x4 #xd3 #x4 #xd6 
        #x4 #xd7 #x4 #xda #x4 #xdb #x4 #xdc 
        #x4 #xdd #x4 #xde #x4 #xdf #x4 #xe2 
        #x4 #xe3 #x4 #xe4 #x4 #xe5 #x4 #xe6 
        #x4 #xe7 #x4 #xea #x4 #xeb #x4 #xec 
        #x4 #xed #x4 #xee #x4 #xef #x4 #xf0 
        #x4 #xf1 #x4 #xf2 #x4 #xf3 #x4 #xf4 
        #x4 #xf5 #x4 #xf8 #x4 #xf9 #x5 #x87 
        #x6 #x22 #x6 #x23 #x6 #x24 #x6 #x25 
        #x6 #x26 #x6 #x75 #x6 #x76 #x6 #x77 
        #x6 #x78 #x6 #xc0 #x6 #xc2 #x6 #xd3 
        #x9 #x29 #x9 #x31 #x9 #x34 #x9 #x58 
        #x9 #x59 #x9 #x5a #x9 #x5b #x9 #x5c 
        #x9 #x5d #x9 #x5e #x9 #x5f #x9 #xcb 
        #x9 #xcc #x9 #xdc #x9 #xdd #x9 #xdf 
        #xa #x33 #xa #x36 #xa #x59 #xa #x5a 
        #xa #x5b #xa #x5e #xb #x48 #xb #x4b 
        #xb #x4c #xb #x5c #xb #x5d #xb #x94 
        #xb #xca #xb #xcb #xb #xcc #xc #x48 
        #xc #xc0 #xc #xc7 #xc #xc8 #xc #xca 
        #xc #xcb #xd #x4a #xd #x4b #xd #x4c 
        #xd #xda #xd #xdc #xd #xdd #xd #xde 
        #xe #x33 #xe #xb3 #xe #xdc #xe #xdd 
        #xf #xc #xf #x43 #xf #x4d #xf #x52 
        #xf #x57 #xf #x5c #xf #x69 #xf #x73 
        #xf #x75 #xf #x76 #xf #x77 #xf #x78 
        #xf #x79 #xf #x81 #xf #x93 #xf #x9d 
        #xf #xa2 #xf #xa7 #xf #xac #xf #xb9 
        #x10 #x26 #x10 #xfc #x1b #x6 #x1b #x8 
        #x1b #xa #x1b #xc #x1b #xe #x1b #x12 
        #x1b #x3b #x1b #x3d #x1b #x40 #x1b #x41 
        #x1b #x43 #x1d #x2c #x1d #x2d #x1d #x2e 
        #x1d #x30 #x1d #x31 #x1d #x32 #x1d #x33 
        #x1d #x34 #x1d #x35 #x1d #x36 #x1d #x37 
        #x1d #x38 #x1d #x39 #x1d #x3a #x1d #x3c 
        #x1d #x3d #x1d #x3e #x1d #x3f #x1d #x40 
        #x1d #x41 #x1d #x42 #x1d #x43 #x1d #x44 
        #x1d #x45 #x1d #x46 #x1d #x47 #x1d #x48 
        #x1d #x49 #x1d #x4a #x1d #x4b #x1d #x4c 
        #x1d #x4d #x1d #x4f #x1d #x50 #x1d #x51 
        #x1d #x52 #x1d #x53 #x1d #x54 #x1d #x55 
        #x1d #x56 #x1d #x57 #x1d #x58 #x1d #x59 
        #x1d #x5a #x1d #x5b #x1d #x5c #x1d #x5d 
        #x1d #x5e #x1d #x5f #x1d #x60 #x1d #x61 
        #x1d #x62 #x1d #x63 #x1d #x64 #x1d #x65 
        #x1d #x66 #x1d #x67 #x1d #x68 #x1d #x69 
        #x1d #x6a #x1d #x78 #x1d #x9b #x1d #x9c 
        #x1d #x9d #x1d #x9e #x1d #x9f #x1d #xa0 
        #x1d #xa1 #x1d #xa2 #x1d #xa3 #x1d #xa4 
        #x1d #xa5 #x1d #xa6 #x1d #xa7 #x1d #xa8 
        #x1d #xa9 #x1d #xaa #x1d #xab #x1d #xac 
        #x1d #xad #x1d #xae #x1d #xaf #x1d #xb0 
        #x1d #xb1 #x1d #xb2 #x1d #xb3 #x1d #xb4 
        #x1d #xb5 #x1d #xb6 #x1d #xb7 #x1d #xb8 
        #x1d #xb9 #x1d #xba #x1d #xbb #x1d #xbc 
        #x1d #xbd #x1d #xbe #x1d #xbf #x1e #x0 
        #x1e #x1 #x1e #x2 #x1e #x3 #x1e #x4 
        #x1e #x5 #x1e #x6 #x1e #x7 #x1e #x8 
        #x1e #x9 #x1e #xa #x1e #xb #x1e #xc 
        #x1e #xd #x1e #xe #x1e #xf #x1e #x10 
        #x1e #x11 #x1e #x12 #x1e #x13 #x1e #x14 
        #x1e #x15 #x1e #x16 #x1e #x17 #x1e #x18 
        #x1e #x19 #x1e #x1a #x1e #x1b #x1e #x1c 
        #x1e #x1d #x1e #x1e #x1e #x1f #x1e #x20 
        #x1e #x21 #x1e #x22 #x1e #x23 #x1e #x24 
        #x1e #x25 #x1e #x26 #x1e #x27 #x1e #x28 
        #x1e #x29 #x1e #x2a #x1e #x2b #x1e #x2c 
        #x1e #x2d #x1e #x2e #x1e #x2f #x1e #x30 
        #x1e #x31 #x1e #x32 #x1e #x33 #x1e #x34 
        #x1e #x35 #x1e #x36 #x1e #x37 #x1e #x38 
        #x1e #x39 #x1e #x3a #x1e #x3b #x1e #x3c 
        #x1e #x3d #x1e #x3e #x1e #x3f #x1e #x40 
        #x1e #x41 #x1e #x42 #x1e #x43 #x1e #x44 
        #x1e #x45 #x1e #x46 #x1e #x47 #x1e #x48 
        #x1e #x49 #x1e #x4a #x1e #x4b #x1e #x4c 
        #x1e #x4d #x1e #x4e #x1e #x4f #x1e #x50 
        #x1e #x51 #x1e #x52 #x1e #x53 #x1e #x54 
        #x1e #x55 #x1e #x56 #x1e #x57 #x1e #x58 
        #x1e #x59 #x1e #x5a #x1e #x5b #x1e #x5c 
        #x1e #x5d #x1e #x5e #x1e #x5f #x1e #x60 
        #x1e #x61 #x1e #x62 #x1e #x63 #x1e #x64 
        #x1e #x65 #x1e #x66 #x1e #x67 #x1e #x68 
        #x1e #x69 #x1e #x6a #x1e #x6b #x1e #x6c 
        #x1e #x6d #x1e #x6e #x1e #x6f #x1e #x70 
        #x1e #x71 #x1e #x72 #x1e #x73 #x1e #x74 
        #x1e #x75 #x1e #x76 #x1e #x77 #x1e #x78 
        #x1e #x79 #x1e #x7a #x1e #x7b #x1e #x7c 
        #x1e #x7d #x1e #x7e #x1e #x7f #x1e #x80 
        #x1e #x81 #x1e #x82 #x1e #x83 #x1e #x84 
        #x1e #x85 #x1e #x86 #x1e #x87 #x1e #x88 
        #x1e #x89 #x1e #x8a #x1e #x8b #x1e #x8c 
        #x1e #x8d #x1e #x8e #x1e #x8f #x1e #x90 
        #x1e #x91 #x1e #x92 #x1e #x93 #x1e #x94 
        #x1e #x95 #x1e #x96 #x1e #x97 #x1e #x98 
        #x1e #x99 #x1e #x9a #x1e #x9b #x1e #xa0 
        #x1e #xa1 #x1e #xa2 #x1e #xa3 #x1e #xa4 
        #x1e #xa5 #x1e #xa6 #x1e #xa7 #x1e #xa8 
        #x1e #xa9 #x1e #xaa #x1e #xab #x1e #xac 
        #x1e #xad #x1e #xae #x1e #xaf #x1e #xb0 
        #x1e #xb1 #x1e #xb2 #x1e #xb3 #x1e #xb4 
        #x1e #xb5 #x1e #xb6 #x1e #xb7 #x1e #xb8 
        #x1e #xb9 #x1e #xba #x1e #xbb #x1e #xbc 
        #x1e #xbd #x1e #xbe #x1e #xbf #x1e #xc0 
        #x1e #xc1 #x1e #xc2 #x1e #xc3 #x1e #xc4 
        #x1e #xc5 #x1e #xc6 #x1e #xc7 #x1e #xc8 
        #x1e #xc9 #x1e #xca #x1e #xcb #x1e #xcc 
        #x1e #xcd #x1e #xce #x1e #xcf #x1e #xd0 
        #x1e #xd1 #x1e #xd2 #x1e #xd3 #x1e #xd4 
        #x1e #xd5 #x1e #xd6 #x1e #xd7 #x1e #xd8 
        #x1e #xd9 #x1e #xda #x1e #xdb #x1e #xdc 
        #x1e #xdd #x1e #xde #x1e #xdf #x1e #xe0 
        #x1e #xe1 #x1e #xe2 #x1e #xe3 #x1e #xe4 
        #x1e #xe5 #x1e #xe6 #x1e #xe7 #x1e #xe8 
        #x1e #xe9 #x1e #xea #x1e #xeb #x1e #xec 
        #x1e #xed #x1e #xee #x1e #xef #x1e #xf0 
        #x1e #xf1 #x1e #xf2 #x1e #xf3 #x1e #xf4 
        #x1e #xf5 #x1e #xf6 #x1e #xf7 #x1e #xf8 
        #x1e #xf9 #x1f #x0 #x1f #x1 #x1f #x2 
        #x1f #x3 #x1f #x4 #x1f #x5 #x1f #x6 
        #x1f #x7 #x1f #x8 #x1f #x9 #x1f #xa 
        #x1f #xb #x1f #xc #x1f #xd #x1f #xe 
        #x1f #xf #x1f #x10 #x1f #x11 #x1f #x12 
        #x1f #x13 #x1f #x14 #x1f #x15 #x1f #x18 
        #x1f #x19 #x1f #x1a #x1f #x1b #x1f #x1c 
        #x1f #x1d #x1f #x20 #x1f #x21 #x1f #x22 
        #x1f #x23 #x1f #x24 #x1f #x25 #x1f #x26 
        #x1f #x27 #x1f #x28 #x1f #x29 #x1f #x2a 
        #x1f #x2b #x1f #x2c #x1f #x2d #x1f #x2e 
        #x1f #x2f #x1f #x30 #x1f #x31 #x1f #x32 
        #x1f #x33 #x1f #x34 #x1f #x35 #x1f #x36 
        #x1f #x37 #x1f #x38 #x1f #x39 #x1f #x3a 
        #x1f #x3b #x1f #x3c #x1f #x3d #x1f #x3e 
        #x1f #x3f #x1f #x40 #x1f #x41 #x1f #x42 
        #x1f #x43 #x1f #x44 #x1f #x45 #x1f #x48 
        #x1f #x49 #x1f #x4a #x1f #x4b #x1f #x4c 
        #x1f #x4d #x1f #x50 #x1f #x51 #x1f #x52 
        #x1f #x53 #x1f #x54 #x1f #x55 #x1f #x56 
        #x1f #x57 #x1f #x59 #x1f #x5b #x1f #x5d 
        #x1f #x5f #x1f #x60 #x1f #x61 #x1f #x62 
        #x1f #x63 #x1f #x64 #x1f #x65 #x1f #x66 
        #x1f #x67 #x1f #x68 #x1f #x69 #x1f #x6a 
        #x1f #x6b #x1f #x6c #x1f #x6d #x1f #x6e 
        #x1f #x6f #x1f #x70 #x1f #x71 #x1f #x72 
        #x1f #x73 #x1f #x74 #x1f #x75 #x1f #x76 
        #x1f #x77 #x1f #x78 #x1f #x79 #x1f #x7a 
        #x1f #x7b #x1f #x7c #x1f #x7d #x1f #x80 
        #x1f #x81 #x1f #x82 #x1f #x83 #x1f #x84 
        #x1f #x85 #x1f #x86 #x1f #x87 #x1f #x88 
        #x1f #x89 #x1f #x8a #x1f #x8b #x1f #x8c 
        #x1f #x8d #x1f #x8e #x1f #x8f #x1f #x90 
        #x1f #x91 #x1f #x92 #x1f #x93 #x1f #x94 
        #x1f #x95 #x1f #x96 #x1f #x97 #x1f #x98 
        #x1f #x99 #x1f #x9a #x1f #x9b #x1f #x9c 
        #x1f #x9d #x1f #x9e #x1f #x9f #x1f #xa0 
        #x1f #xa1 #x1f #xa2 #x1f #xa3 #x1f #xa4 
        #x1f #xa5 #x1f #xa6 #x1f #xa7 #x1f #xa8 
        #x1f #xa9 #x1f #xaa #x1f #xab #x1f #xac 
        #x1f #xad #x1f #xae #x1f #xaf #x1f #xb0 
        #x1f #xb1 #x1f #xb2 #x1f #xb3 #x1f #xb4 
        #x1f #xb6 #x1f #xb7 #x1f #xb8 #x1f #xb9 
        #x1f #xba #x1f #xbb #x1f #xbc #x1f #xbd 
        #x1f #xbe #x1f #xbf #x1f #xc0 #x1f #xc1 
        #x1f #xc2 #x1f #xc3 #x1f #xc4 #x1f #xc6 
        #x1f #xc7 #x1f #xc8 #x1f #xc9 #x1f #xca 
        #x1f #xcb #x1f #xcc #x1f #xcd #x1f #xce 
        #x1f #xcf #x1f #xd0 #x1f #xd1 #x1f #xd2 
        #x1f #xd3 #x1f #xd6 #x1f #xd7 #x1f #xd8 
        #x1f #xd9 #x1f #xda #x1f #xdb #x1f #xdd 
        #x1f #xde #x1f #xdf #x1f #xe0 #x1f #xe1 
        #x1f #xe2 #x1f #xe3 #x1f #xe4 #x1f #xe5 
        #x1f #xe6 #x1f #xe7 #x1f #xe8 #x1f #xe9 
        #x1f #xea #x1f #xeb #x1f #xec #x1f #xed 
        #x1f #xee #x1f #xef #x1f #xf2 #x1f #xf3 
        #x1f #xf4 #x1f #xf6 #x1f #xf7 #x1f #xf8 
        #x1f #xf9 #x1f #xfa #x1f #xfb #x1f #xfc 
        #x1f #xfd #x1f #xfe #x20 #x0 #x20 #x1 
        #x20 #x2 #x20 #x3 #x20 #x4 #x20 #x5 
        #x20 #x6 #x20 #x7 #x20 #x8 #x20 #x9 
        #x20 #xa #x20 #x11 #x20 #x17 #x20 #x24 
        #x20 #x25 #x20 #x26 #x20 #x2f #x20 #x33 
        #x20 #x34 #x20 #x36 #x20 #x37 #x20 #x3c 
        #x20 #x3e #x20 #x47 #x20 #x48 #x20 #x49 
        #x20 #x57 #x20 #x5f #x20 #x70 #x20 #x71 
        #x20 #x74 #x20 #x75 #x20 #x76 #x20 #x77 
        #x20 #x78 #x20 #x79 #x20 #x7a #x20 #x7b 
        #x20 #x7c #x20 #x7d #x20 #x7e #x20 #x7f 
        #x20 #x80 #x20 #x81 #x20 #x82 #x20 #x83 
        #x20 #x84 #x20 #x85 #x20 #x86 #x20 #x87 
        #x20 #x88 #x20 #x89 #x20 #x8a #x20 #x8b 
        #x20 #x8c #x20 #x8d #x20 #x8e #x20 #x90 
        #x20 #x91 #x20 #x92 #x20 #x93 #x20 #x94 
        #x20 #xa8 #x21 #x0 #x21 #x1 #x21 #x2 
        #x21 #x3 #x21 #x5 #x21 #x6 #x21 #x7 
        #x21 #x9 #x21 #xa #x21 #xb #x21 #xc 
        #x21 #xd #x21 #xe #x21 #xf #x21 #x10 
        #x21 #x11 #x21 #x12 #x21 #x13 #x21 #x15 
        #x21 #x16 #x21 #x19 #x21 #x1a #x21 #x1b 
        #x21 #x1c #x21 #x1d #x21 #x20 #x21 #x21 
        #x21 #x22 #x21 #x24 #x21 #x26 #x21 #x28 
        #x21 #x2a #x21 #x2b #x21 #x2c #x21 #x2d 
        #x21 #x2f #x21 #x30 #x21 #x31 #x21 #x33 
        #x21 #x34 #x21 #x35 #x21 #x36 #x21 #x37 
        #x21 #x38 #x21 #x39 #x21 #x3b #x21 #x3c 
        #x21 #x3d #x21 #x3e #x21 #x3f #x21 #x40 
        #x21 #x45 #x21 #x46 #x21 #x47 #x21 #x48 
        #x21 #x49 #x21 #x53 #x21 #x54 #x21 #x55 
        #x21 #x56 #x21 #x57 #x21 #x58 #x21 #x59 
        #x21 #x5a #x21 #x5b #x21 #x5c #x21 #x5d 
        #x21 #x5e #x21 #x5f #x21 #x60 #x21 #x61 
        #x21 #x62 #x21 #x63 #x21 #x64 #x21 #x65 
        #x21 #x66 #x21 #x67 #x21 #x68 #x21 #x69 
        #x21 #x6a #x21 #x6b #x21 #x6c #x21 #x6d 
        #x21 #x6e #x21 #x6f #x21 #x70 #x21 #x71 
        #x21 #x72 #x21 #x73 #x21 #x74 #x21 #x75 
        #x21 #x76 #x21 #x77 #x21 #x78 #x21 #x79 
        #x21 #x7a #x21 #x7b #x21 #x7c #x21 #x7d 
        #x21 #x7e #x21 #x7f #x21 #x9a #x21 #x9b 
        #x21 #xae #x21 #xcd #x21 #xce #x21 #xcf 
        #x22 #x4 #x22 #x9 #x22 #xc #x22 #x24 
        #x22 #x26 #x22 #x2c #x22 #x2d #x22 #x2f 
        #x22 #x30 #x22 #x41 #x22 #x44 #x22 #x47 
        #x22 #x49 #x22 #x60 #x22 #x62 #x22 #x6d 
        #x22 #x6e #x22 #x6f #x22 #x70 #x22 #x71 
        #x22 #x74 #x22 #x75 #x22 #x78 #x22 #x79 
        #x22 #x80 #x22 #x81 #x22 #x84 #x22 #x85 
        #x22 #x88 #x22 #x89 #x22 #xac #x22 #xad 
        #x22 #xae #x22 #xaf #x22 #xe0 #x22 #xe1 
        #x22 #xe2 #x22 #xe3 #x22 #xea #x22 #xeb 
        #x22 #xec #x22 #xed #x23 #x29 #x23 #x2a 
        #x24 #x60 #x24 #x61 #x24 #x62 #x24 #x63 
        #x24 #x64 #x24 #x65 #x24 #x66 #x24 #x67 
        #x24 #x68 #x24 #x69 #x24 #x6a #x24 #x6b 
        #x24 #x6c #x24 #x6d #x24 #x6e #x24 #x6f 
        #x24 #x70 #x24 #x71 #x24 #x72 #x24 #x73 
        #x24 #x74 #x24 #x75 #x24 #x76 #x24 #x77 
        #x24 #x78 #x24 #x79 #x24 #x7a #x24 #x7b 
        #x24 #x7c #x24 #x7d #x24 #x7e #x24 #x7f 
        #x24 #x80 #x24 #x81 #x24 #x82 #x24 #x83 
        #x24 #x84 #x24 #x85 #x24 #x86 #x24 #x87 
        #x24 #x88 #x24 #x89 #x24 #x8a #x24 #x8b 
        #x24 #x8c #x24 #x8d #x24 #x8e #x24 #x8f 
        #x24 #x90 #x24 #x91 #x24 #x92 #x24 #x93 
        #x24 #x94 #x24 #x95 #x24 #x96 #x24 #x97 
        #x24 #x98 #x24 #x99 #x24 #x9a #x24 #x9b 
        #x24 #x9c #x24 #x9d #x24 #x9e #x24 #x9f 
        #x24 #xa0 #x24 #xa1 #x24 #xa2 #x24 #xa3 
        #x24 #xa4 #x24 #xa5 #x24 #xa6 #x24 #xa7 
        #x24 #xa8 #x24 #xa9 #x24 #xaa #x24 #xab 
        #x24 #xac #x24 #xad #x24 #xae #x24 #xaf 
        #x24 #xb0 #x24 #xb1 #x24 #xb2 #x24 #xb3 
        #x24 #xb4 #x24 #xb5 #x24 #xb6 #x24 #xb7 
        #x24 #xb8 #x24 #xb9 #x24 #xba #x24 #xbb 
        #x24 #xbc #x24 #xbd #x24 #xbe #x24 #xbf 
        #x24 #xc0 #x24 #xc1 #x24 #xc2 #x24 #xc3 
        #x24 #xc4 #x24 #xc5 #x24 #xc6 #x24 #xc7 
        #x24 #xc8 #x24 #xc9 #x24 #xca #x24 #xcb 
        #x24 #xcc #x24 #xcd #x24 #xce #x24 #xcf 
        #x24 #xd0 #x24 #xd1 #x24 #xd2 #x24 #xd3 
        #x24 #xd4 #x24 #xd5 #x24 #xd6 #x24 #xd7 
        #x24 #xd8 #x24 #xd9 #x24 #xda #x24 #xdb 
        #x24 #xdc #x24 #xdd #x24 #xde #x24 #xdf 
        #x24 #xe0 #x24 #xe1 #x24 #xe2 #x24 #xe3 
        #x24 #xe4 #x24 #xe5 #x24 #xe6 #x24 #xe7 
        #x24 #xe8 #x24 #xe9 #x24 #xea #x2a #xc 
        #x2a #x74 #x2a #x75 #x2a #x76 #x2a #xdc 
        #x2d #x6f #x2e #x9f #x2e #xf3 #x2f #x0 
        #x2f #x1 #x2f #x2 #x2f #x3 #x2f #x4 
        #x2f #x5 #x2f #x6 #x2f #x7 #x2f #x8 
        #x2f #x9 #x2f #xa #x2f #xb #x2f #xc 
        #x2f #xd #x2f #xe #x2f #xf #x2f #x10 
        #x2f #x11 #x2f #x12 #x2f #x13 #x2f #x14 
        #x2f #x15 #x2f #x16 #x2f #x17 #x2f #x18 
        #x2f #x19 #x2f #x1a #x2f #x1b #x2f #x1c 
        #x2f #x1d #x2f #x1e #x2f #x1f #x2f #x20 
        #x2f #x21 #x2f #x22 #x2f #x23 #x2f #x24 
        #x2f #x25 #x2f #x26 #x2f #x27 #x2f #x28 
        #x2f #x29 #x2f #x2a #x2f #x2b #x2f #x2c 
        #x2f #x2d #x2f #x2e #x2f #x2f #x2f #x30 
        #x2f #x31 #x2f #x32 #x2f #x33 #x2f #x34 
        #x2f #x35 #x2f #x36 #x2f #x37 #x2f #x38 
        #x2f #x39 #x2f #x3a #x2f #x3b #x2f #x3c 
        #x2f #x3d #x2f #x3e #x2f #x3f #x2f #x40 
        #x2f #x41 #x2f #x42 #x2f #x43 #x2f #x44 
        #x2f #x45 #x2f #x46 #x2f #x47 #x2f #x48 
        #x2f #x49 #x2f #x4a #x2f #x4b #x2f #x4c 
        #x2f #x4d #x2f #x4e #x2f #x4f #x2f #x50 
        #x2f #x51 #x2f #x52 #x2f #x53 #x2f #x54 
        #x2f #x55 #x2f #x56 #x2f #x57 #x2f #x58 
        #x2f #x59 #x2f #x5a #x2f #x5b #x2f #x5c 
        #x2f #x5d #x2f #x5e #x2f #x5f #x2f #x60 
        #x2f #x61 #x2f #x62 #x2f #x63 #x2f #x64 
        #x2f #x65 #x2f #x66 #x2f #x67 #x2f #x68 
        #x2f #x69 #x2f #x6a #x2f #x6b #x2f #x6c 
        #x2f #x6d #x2f #x6e #x2f #x6f #x2f #x70 
        #x2f #x71 #x2f #x72 #x2f #x73 #x2f #x74 
        #x2f #x75 #x2f #x76 #x2f #x77 #x2f #x78 
        #x2f #x79 #x2f #x7a #x2f #x7b #x2f #x7c 
        #x2f #x7d #x2f #x7e #x2f #x7f #x2f #x80 
        #x2f #x81 #x2f #x82 #x2f #x83 #x2f #x84 
        #x2f #x85 #x2f #x86 #x2f #x87 #x2f #x88 
        #x2f #x89 #x2f #x8a #x2f #x8b #x2f #x8c 
        #x2f #x8d #x2f #x8e #x2f #x8f #x2f #x90 
        #x2f #x91 #x2f #x92 #x2f #x93 #x2f #x94 
        #x2f #x95 #x2f #x96 #x2f #x97 #x2f #x98 
        #x2f #x99 #x2f #x9a #x2f #x9b #x2f #x9c 
        #x2f #x9d #x2f #x9e #x2f #x9f #x2f #xa0 
        #x2f #xa1 #x2f #xa2 #x2f #xa3 #x2f #xa4 
        #x2f #xa5 #x2f #xa6 #x2f #xa7 #x2f #xa8 
        #x2f #xa9 #x2f #xaa #x2f #xab #x2f #xac 
        #x2f #xad #x2f #xae #x2f #xaf #x2f #xb0 
        #x2f #xb1 #x2f #xb2 #x2f #xb3 #x2f #xb4 
        #x2f #xb5 #x2f #xb6 #x2f #xb7 #x2f #xb8 
        #x2f #xb9 #x2f #xba #x2f #xbb #x2f #xbc 
        #x2f #xbd #x2f #xbe #x2f #xbf #x2f #xc0 
        #x2f #xc1 #x2f #xc2 #x2f #xc3 #x2f #xc4 
        #x2f #xc5 #x2f #xc6 #x2f #xc7 #x2f #xc8 
        #x2f #xc9 #x2f #xca #x2f #xcb #x2f #xcc 
        #x2f #xcd #x2f #xce #x2f #xcf #x2f #xd0 
        #x2f #xd1 #x2f #xd2 #x2f #xd3 #x2f #xd4 
        #x2f #xd5 #x30 #x0 #x30 #x36 #x30 #x38 
        #x30 #x39 #x30 #x3a #x30 #x4c #x30 #x4e 
        #x30 #x50 #x30 #x52 #x30 #x54 #x30 #x56 
        #x30 #x58 #x30 #x5a #x30 #x5c #x30 #x5e 
        #x30 #x60 #x30 #x62 #x30 #x65 #x30 #x67 
        #x30 #x69 #x30 #x70 #x30 #x71 #x30 #x73 
        #x30 #x74 #x30 #x76 #x30 #x77 #x30 #x79 
        #x30 #x7a #x30 #x7c #x30 #x7d #x30 #x94 
        #x30 #x9b #x30 #x9c #x30 #x9e #x30 #x9f 
        #x30 #xac #x30 #xae #x30 #xb0 #x30 #xb2 
        #x30 #xb4 #x30 #xb6 #x30 #xb8 #x30 #xba 
        #x30 #xbc #x30 #xbe #x30 #xc0 #x30 #xc2 
        #x30 #xc5 #x30 #xc7 #x30 #xc9 #x30 #xd0 
        #x30 #xd1 #x30 #xd3 #x30 #xd4 #x30 #xd6 
        #x30 #xd7 #x30 #xd9 #x30 #xda #x30 #xdc 
        #x30 #xdd #x30 #xf4 #x30 #xf7 #x30 #xf8 
        #x30 #xf9 #x30 #xfa #x30 #xfe #x30 #xff 
        #x31 #x31 #x31 #x32 #x31 #x33 #x31 #x34 
        #x31 #x35 #x31 #x36 #x31 #x37 #x31 #x38 
        #x31 #x39 #x31 #x3a #x31 #x3b #x31 #x3c 
        #x31 #x3d #x31 #x3e #x31 #x3f #x31 #x40 
        #x31 #x41 #x31 #x42 #x31 #x43 #x31 #x44 
        #x31 #x45 #x31 #x46 #x31 #x47 #x31 #x48 
        #x31 #x49 #x31 #x4a #x31 #x4b #x31 #x4c 
        #x31 #x4d #x31 #x4e #x31 #x4f #x31 #x50 
        #x31 #x51 #x31 #x52 #x31 #x53 #x31 #x54 
        #x31 #x55 #x31 #x56 #x31 #x57 #x31 #x58 
        #x31 #x59 #x31 #x5a #x31 #x5b #x31 #x5c 
        #x31 #x5d #x31 #x5e #x31 #x5f #x31 #x60 
        #x31 #x61 #x31 #x62 #x31 #x63 #x31 #x64 
        #x31 #x65 #x31 #x66 #x31 #x67 #x31 #x68 
        #x31 #x69 #x31 #x6a #x31 #x6b #x31 #x6c 
        #x31 #x6d #x31 #x6e #x31 #x6f #x31 #x70 
        #x31 #x71 #x31 #x72 #x31 #x73 #x31 #x74 
        #x31 #x75 #x31 #x76 #x31 #x77 #x31 #x78 
        #x31 #x79 #x31 #x7a #x31 #x7b #x31 #x7c 
        #x31 #x7d #x31 #x7e #x31 #x7f #x31 #x80 
        #x31 #x81 #x31 #x82 #x31 #x83 #x31 #x84 
        #x31 #x85 #x31 #x86 #x31 #x87 #x31 #x88 
        #x31 #x89 #x31 #x8a #x31 #x8b #x31 #x8c 
        #x31 #x8d #x31 #x8e #x31 #x92 #x31 #x93 
        #x31 #x94 #x31 #x95 #x31 #x96 #x31 #x97 
        #x31 #x98 #x31 #x99 #x31 #x9a #x31 #x9b 
        #x31 #x9c #x31 #x9d #x31 #x9e #x31 #x9f 
        #x32 #x0 #x32 #x1 #x32 #x2 #x32 #x3 
        #x32 #x4 #x32 #x5 #x32 #x6 #x32 #x7 
        #x32 #x8 #x32 #x9 #x32 #xa #x32 #xb 
        #x32 #xc #x32 #xd #x32 #xe #x32 #xf 
        #x32 #x10 #x32 #x11 #x32 #x12 #x32 #x13 
        #x32 #x14 #x32 #x15 #x32 #x16 #x32 #x17 
        #x32 #x18 #x32 #x19 #x32 #x1a #x32 #x1b 
        #x32 #x1c #x32 #x1d #x32 #x1e #x32 #x20 
        #x32 #x21 #x32 #x22 #x32 #x23 #x32 #x24 
        #x32 #x25 #x32 #x26 #x32 #x27 #x32 #x28 
        #x32 #x29 #x32 #x2a #x32 #x2b #x32 #x2c 
        #x32 #x2d #x32 #x2e #x32 #x2f #x32 #x30 
        #x32 #x31 #x32 #x32 #x32 #x33 #x32 #x34 
        #x32 #x35 #x32 #x36 #x32 #x37 #x32 #x38 
        #x32 #x39 #x32 #x3a #x32 #x3b #x32 #x3c 
        #x32 #x3d #x32 #x3e #x32 #x3f #x32 #x40 
        #x32 #x41 #x32 #x42 #x32 #x43 #x32 #x50 
        #x32 #x51 #x32 #x52 #x32 #x53 #x32 #x54 
        #x32 #x55 #x32 #x56 #x32 #x57 #x32 #x58 
        #x32 #x59 #x32 #x5a #x32 #x5b #x32 #x5c 
        #x32 #x5d #x32 #x5e #x32 #x5f #x32 #x60 
        #x32 #x61 #x32 #x62 #x32 #x63 #x32 #x64 
        #x32 #x65 #x32 #x66 #x32 #x67 #x32 #x68 
        #x32 #x69 #x32 #x6a #x32 #x6b #x32 #x6c 
        #x32 #x6d #x32 #x6e #x32 #x6f #x32 #x70 
        #x32 #x71 #x32 #x72 #x32 #x73 #x32 #x74 
        #x32 #x75 #x32 #x76 #x32 #x77 #x32 #x78 
        #x32 #x79 #x32 #x7a #x32 #x7b #x32 #x7c 
        #x32 #x7d #x32 #x7e #x32 #x80 #x32 #x81 
        #x32 #x82 #x32 #x83 #x32 #x84 #x32 #x85 
        #x32 #x86 #x32 #x87 #x32 #x88 #x32 #x89 
        #x32 #x8a #x32 #x8b #x32 #x8c #x32 #x8d 
        #x32 #x8e #x32 #x8f #x32 #x90 #x32 #x91 
        #x32 #x92 #x32 #x93 #x32 #x94 #x32 #x95 
        #x32 #x96 #x32 #x97 #x32 #x98 #x32 #x99 
        #x32 #x9a #x32 #x9b #x32 #x9c #x32 #x9d 
        #x32 #x9e #x32 #x9f #x32 #xa0 #x32 #xa1 
        #x32 #xa2 #x32 #xa3 #x32 #xa4 #x32 #xa5 
        #x32 #xa6 #x32 #xa7 #x32 #xa8 #x32 #xa9 
        #x32 #xaa #x32 #xab #x32 #xac #x32 #xad 
        #x32 #xae #x32 #xaf #x32 #xb0 #x32 #xb1 
        #x32 #xb2 #x32 #xb3 #x32 #xb4 #x32 #xb5 
        #x32 #xb6 #x32 #xb7 #x32 #xb8 #x32 #xb9 
        #x32 #xba #x32 #xbb #x32 #xbc #x32 #xbd 
        #x32 #xbe #x32 #xbf #x32 #xc0 #x32 #xc1 
        #x32 #xc2 #x32 #xc3 #x32 #xc4 #x32 #xc5 
        #x32 #xc6 #x32 #xc7 #x32 #xc8 #x32 #xc9 
        #x32 #xca #x32 #xcb #x32 #xcc #x32 #xcd 
        #x32 #xce #x32 #xcf #x32 #xd0 #x32 #xd1 
        #x32 #xd2 #x32 #xd3 #x32 #xd4 #x32 #xd5 
        #x32 #xd6 #x32 #xd7 #x32 #xd8 #x32 #xd9 
        #x32 #xda #x32 #xdb #x32 #xdc #x32 #xdd 
        #x32 #xde #x32 #xdf #x32 #xe0 #x32 #xe1 
        #x32 #xe2 #x32 #xe3 #x32 #xe4 #x32 #xe5 
        #x32 #xe6 #x32 #xe7 #x32 #xe8 #x32 #xe9 
        #x32 #xea #x32 #xeb #x32 #xec #x32 #xed 
        #x32 #xee #x32 #xef #x32 #xf0 #x32 #xf1 
        #x32 #xf2 #x32 #xf3 #x32 #xf4 #x32 #xf5 
        #x32 #xf6 #x32 #xf7 #x32 #xf8 #x32 #xf9 
        #x32 #xfa #x32 #xfb #x32 #xfc #x32 #xfd 
        #x32 #xfe #x33 #x0 #x33 #x1 #x33 #x2 
        #x33 #x3 #x33 #x4 #x33 #x5 #x33 #x6 
        #x33 #x7 #x33 #x8 #x33 #x9 #x33 #xa 
        #x33 #xb #x33 #xc #x33 #xd #x33 #xe 
        #x33 #xf #x33 #x10 #x33 #x11 #x33 #x12 
        #x33 #x13 #x33 #x14 #x33 #x15 #x33 #x16 
        #x33 #x17 #x33 #x18 #x33 #x19 #x33 #x1a 
        #x33 #x1b #x33 #x1c #x33 #x1d #x33 #x1e 
        #x33 #x1f #x33 #x20 #x33 #x21 #x33 #x22 
        #x33 #x23 #x33 #x24 #x33 #x25 #x33 #x26 
        #x33 #x27 #x33 #x28 #x33 #x29 #x33 #x2a 
        #x33 #x2b #x33 #x2c #x33 #x2d #x33 #x2e 
        #x33 #x2f #x33 #x30 #x33 #x31 #x33 #x32 
        #x33 #x33 #x33 #x34 #x33 #x35 #x33 #x36 
        #x33 #x37 #x33 #x38 #x33 #x39 #x33 #x3a 
        #x33 #x3b #x33 #x3c #x33 #x3d #x33 #x3e 
        #x33 #x3f #x33 #x40 #x33 #x41 #x33 #x42 
        #x33 #x43 #x33 #x44 #x33 #x45 #x33 #x46 
        #x33 #x47 #x33 #x48 #x33 #x49 #x33 #x4a 
        #x33 #x4b #x33 #x4c #x33 #x4d #x33 #x4e 
        #x33 #x4f #x33 #x50 #x33 #x51 #x33 #x52 
        #x33 #x53 #x33 #x54 #x33 #x55 #x33 #x56 
        #x33 #x57 #x33 #x58 #x33 #x59 #x33 #x5a 
        #x33 #x5b #x33 #x5c #x33 #x5d #x33 #x5e 
        #x33 #x5f #x33 #x60 #x33 #x61 #x33 #x62 
        #x33 #x63 #x33 #x64 #x33 #x65 #x33 #x66 
        #x33 #x67 #x33 #x68 #x33 #x69 #x33 #x6a 
        #x33 #x6b #x33 #x6c #x33 #x6d #x33 #x6e 
        #x33 #x6f #x33 #x70 #x33 #x71 #x33 #x72 
        #x33 #x73 #x33 #x74 #x33 #x75 #x33 #x76 
        #x33 #x77 #x33 #x78 #x33 #x79 #x33 #x7a 
        #x33 #x7b #x33 #x7c #x33 #x7d #x33 #x7e 
        #x33 #x7f #x33 #x80 #x33 #x81 #x33 #x82 
        #x33 #x83 #x33 #x84 #x33 #x85 #x33 #x86 
        #x33 #x87 #x33 #x88 #x33 #x89 #x33 #x8a 
        #x33 #x8b #x33 #x8c #x33 #x8d #x33 #x8e 
        #x33 #x8f #x33 #x90 #x33 #x91 #x33 #x92 
        #x33 #x93 #x33 #x94 #x33 #x95 #x33 #x96 
        #x33 #x97 #x33 #x98 #x33 #x99 #x33 #x9a 
        #x33 #x9b #x33 #x9c #x33 #x9d #x33 #x9e 
        #x33 #x9f #x33 #xa0 #x33 #xa1 #x33 #xa2 
        #x33 #xa3 #x33 #xa4 #x33 #xa5 #x33 #xa6 
        #x33 #xa7 #x33 #xa8 #x33 #xa9 #x33 #xaa 
        #x33 #xab #x33 #xac #x33 #xad #x33 #xae 
        #x33 #xaf #x33 #xb0 #x33 #xb1 #x33 #xb2 
        #x33 #xb3 #x33 #xb4 #x33 #xb5 #x33 #xb6 
        #x33 #xb7 #x33 #xb8 #x33 #xb9 #x33 #xba 
        #x33 #xbb #x33 #xbc #x33 #xbd #x33 #xbe 
        #x33 #xbf #x33 #xc0 #x33 #xc1 #x33 #xc2 
        #x33 #xc3 #x33 #xc4 #x33 #xc5 #x33 #xc6 
        #x33 #xc7 #x33 #xc8 #x33 #xc9 #x33 #xca 
        #x33 #xcb #x33 #xcc #x33 #xcd #x33 #xce 
        #x33 #xcf #x33 #xd0 #x33 #xd1 #x33 #xd2 
        #x33 #xd3 #x33 #xd4 #x33 #xd5 #x33 #xd6 
        #x33 #xd7 #x33 #xd8 #x33 #xd9 #x33 #xda 
        #x33 #xdb #x33 #xdc #x33 #xdd #x33 #xde 
        #x33 #xdf #x33 #xe0 #x33 #xe1 #x33 #xe2 
        #x33 #xe3 #x33 #xe4 #x33 #xe5 #x33 #xe6 
        #x33 #xe7 #x33 #xe8 #x33 #xe9 #x33 #xea 
        #x33 #xeb #x33 #xec #x33 #xed #x33 #xee 
        #x33 #xef #x33 #xf0 #x33 #xf1 #x33 #xf2 
        #x33 #xf3 #x33 #xf4 #x33 #xf5 #x33 #xf6 
        #x33 #xf7 #x33 #xf8 #x33 #xf9 #x33 #xfa 
        #x33 #xfb #x33 #xfc #x33 #xfd #x33 #xfe 
        #x33 #xff #xf9 #x0 #xf9 #x1 #xf9 #x2 
        #xf9 #x3 #xf9 #x4 #xf9 #x5 #xf9 #x6 
        #xf9 #x7 #xf9 #x8 #xf9 #x9 #xf9 #xa 
        #xf9 #xb #xf9 #xc #xf9 #xd #xf9 #xe 
        #xf9 #xf #xf9 #x10 #xf9 #x11 #xf9 #x12 
        #xf9 #x13 #xf9 #x14 #xf9 #x15 #xf9 #x16 
        #xf9 #x17 #xf9 #x18 #xf9 #x19 #xf9 #x1a 
        #xf9 #x1b #xf9 #x1c #xf9 #x1d #xf9 #x1e 
        #xf9 #x1f #xf9 #x20 #xf9 #x21 #xf9 #x22 
        #xf9 #x23 #xf9 #x24 #xf9 #x25 #xf9 #x26 
        #xf9 #x27 #xf9 #x28 #xf9 #x29 #xf9 #x2a 
        #xf9 #x2b #xf9 #x2c #xf9 #x2d #xf9 #x2e 
        #xf9 #x2f #xf9 #x30 #xf9 #x31 #xf9 #x32 
        #xf9 #x33 #xf9 #x34 #xf9 #x35 #xf9 #x36 
        #xf9 #x37 #xf9 #x38 #xf9 #x39 #xf9 #x3a 
        #xf9 #x3b #xf9 #x3c #xf9 #x3d #xf9 #x3e 
        #xf9 #x3f #xf9 #x40 #xf9 #x41 #xf9 #x42 
        #xf9 #x43 #xf9 #x44 #xf9 #x45 #xf9 #x46 
        #xf9 #x47 #xf9 #x48 #xf9 #x49 #xf9 #x4a 
        #xf9 #x4b #xf9 #x4c #xf9 #x4d #xf9 #x4e 
        #xf9 #x4f #xf9 #x50 #xf9 #x51 #xf9 #x52 
        #xf9 #x53 #xf9 #x54 #xf9 #x55 #xf9 #x56 
        #xf9 #x57 #xf9 #x58 #xf9 #x59 #xf9 #x5a 
        #xf9 #x5b #xf9 #x5c #xf9 #x5d #xf9 #x5e 
        #xf9 #x5f #xf9 #x60 #xf9 #x61 #xf9 #x62 
        #xf9 #x63 #xf9 #x64 #xf9 #x65 #xf9 #x66 
        #xf9 #x67 #xf9 #x68 #xf9 #x69 #xf9 #x6a 
        #xf9 #x6b #xf9 #x6c #xf9 #x6d #xf9 #x6e 
        #xf9 #x6f #xf9 #x70 #xf9 #x71 #xf9 #x72 
        #xf9 #x73 #xf9 #x74 #xf9 #x75 #xf9 #x76 
        #xf9 #x77 #xf9 #x78 #xf9 #x79 #xf9 #x7a 
        #xf9 #x7b #xf9 #x7c #xf9 #x7d #xf9 #x7e 
        #xf9 #x7f #xf9 #x80 #xf9 #x81 #xf9 #x82 
        #xf9 #x83 #xf9 #x84 #xf9 #x85 #xf9 #x86 
        #xf9 #x87 #xf9 #x88 #xf9 #x89 #xf9 #x8a 
        #xf9 #x8b #xf9 #x8c #xf9 #x8d #xf9 #x8e 
        #xf9 #x8f #xf9 #x90 #xf9 #x91 #xf9 #x92 
        #xf9 #x93 #xf9 #x94 #xf9 #x95 #xf9 #x96 
        #xf9 #x97 #xf9 #x98 #xf9 #x99 #xf9 #x9a 
        #xf9 #x9b #xf9 #x9c #xf9 #x9d #xf9 #x9e 
        #xf9 #x9f #xf9 #xa0 #xf9 #xa1 #xf9 #xa2 
        #xf9 #xa3 #xf9 #xa4 #xf9 #xa5 #xf9 #xa6 
        #xf9 #xa7 #xf9 #xa8 #xf9 #xa9 #xf9 #xaa 
        #xf9 #xab #xf9 #xac #xf9 #xad #xf9 #xae 
        #xf9 #xaf #xf9 #xb0 #xf9 #xb1 #xf9 #xb2 
        #xf9 #xb3 #xf9 #xb4 #xf9 #xb5 #xf9 #xb6 
        #xf9 #xb7 #xf9 #xb8 #xf9 #xb9 #xf9 #xba 
        #xf9 #xbb #xf9 #xbc #xf9 #xbd #xf9 #xbe 
        #xf9 #xbf #xf9 #xc0 #xf9 #xc1 #xf9 #xc2 
        #xf9 #xc3 #xf9 #xc4 #xf9 #xc5 #xf9 #xc6 
        #xf9 #xc7 #xf9 #xc8 #xf9 #xc9 #xf9 #xca 
        #xf9 #xcb #xf9 #xcc #xf9 #xcd #xf9 #xce 
        #xf9 #xcf #xf9 #xd0 #xf9 #xd1 #xf9 #xd2 
        #xf9 #xd3 #xf9 #xd4 #xf9 #xd5 #xf9 #xd6 
        #xf9 #xd7 #xf9 #xd8 #xf9 #xd9 #xf9 #xda 
        #xf9 #xdb #xf9 #xdc #xf9 #xdd #xf9 #xde 
        #xf9 #xdf #xf9 #xe0 #xf9 #xe1 #xf9 #xe2 
        #xf9 #xe3 #xf9 #xe4 #xf9 #xe5 #xf9 #xe6 
        #xf9 #xe7 #xf9 #xe8 #xf9 #xe9 #xf9 #xea 
        #xf9 #xeb #xf9 #xec #xf9 #xed #xf9 #xee 
        #xf9 #xef #xf9 #xf0 #xf9 #xf1 #xf9 #xf2 
        #xf9 #xf3 #xf9 #xf4 #xf9 #xf5 #xf9 #xf6 
        #xf9 #xf7 #xf9 #xf8 #xf9 #xf9 #xf9 #xfa 
        #xf9 #xfb #xf9 #xfc #xf9 #xfd #xf9 #xfe 
        #xf9 #xff #xfa #x0 #xfa #x1 #xfa #x2 
        #xfa #x3 #xfa #x4 #xfa #x5 #xfa #x6 
        #xfa #x7 #xfa #x8 #xfa #x9 #xfa #xa 
        #xfa #xb #xfa #xc #xfa #xd #xfa #x10 
        #xfa #x12 #xfa #x15 #xfa #x16 #xfa #x17 
        #xfa #x18 #xfa #x19 #xfa #x1a #xfa #x1b 
        #xfa #x1c #xfa #x1d #xfa #x1e #xfa #x20 
        #xfa #x22 #xfa #x25 #xfa #x26 #xfa #x2a 
        #xfa #x2b #xfa #x2c #xfa #x2d #xfa #x30 
        #xfa #x31 #xfa #x32 #xfa #x33 #xfa #x34 
        #xfa #x35 #xfa #x36 #xfa #x37 #xfa #x38 
        #xfa #x39 #xfa #x3a #xfa #x3b #xfa #x3c 
        #xfa #x3d #xfa #x3e #xfa #x3f #xfa #x40 
        #xfa #x41 #xfa #x42 #xfa #x43 #xfa #x44 
        #xfa #x45 #xfa #x46 #xfa #x47 #xfa #x48 
        #xfa #x49 #xfa #x4a #xfa #x4b #xfa #x4c 
        #xfa #x4d #xfa #x4e #xfa #x4f #xfa #x50 
        #xfa #x51 #xfa #x52 #xfa #x53 #xfa #x54 
        #xfa #x55 #xfa #x56 #xfa #x57 #xfa #x58 
        #xfa #x59 #xfa #x5a #xfa #x5b #xfa #x5c 
        #xfa #x5d #xfa #x5e #xfa #x5f #xfa #x60 
        #xfa #x61 #xfa #x62 #xfa #x63 #xfa #x64 
        #xfa #x65 #xfa #x66 #xfa #x67 #xfa #x68 
        #xfa #x69 #xfa #x6a #xfa #x70 #xfa #x71 
        #xfa #x72 #xfa #x73 #xfa #x74 #xfa #x75 
        #xfa #x76 #xfa #x77 #xfa #x78 #xfa #x79 
        #xfa #x7a #xfa #x7b #xfa #x7c #xfa #x7d 
        #xfa #x7e #xfa #x7f #xfa #x80 #xfa #x81 
        #xfa #x82 #xfa #x83 #xfa #x84 #xfa #x85 
        #xfa #x86 #xfa #x87 #xfa #x88 #xfa #x89 
        #xfa #x8a #xfa #x8b #xfa #x8c #xfa #x8d 
        #xfa #x8e #xfa #x8f #xfa #x90 #xfa #x91 
        #xfa #x92 #xfa #x93 #xfa #x94 #xfa #x95 
        #xfa #x96 #xfa #x97 #xfa #x98 #xfa #x99 
        #xfa #x9a #xfa #x9b #xfa #x9c #xfa #x9d 
        #xfa #x9e #xfa #x9f #xfa #xa0 #xfa #xa1 
        #xfa #xa2 #xfa #xa3 #xfa #xa4 #xfa #xa5 
        #xfa #xa6 #xfa #xa7 #xfa #xa8 #xfa #xa9 
        #xfa #xaa #xfa #xab #xfa #xac #xfa #xad 
        #xfa #xae #xfa #xaf #xfa #xb0 #xfa #xb1 
        #xfa #xb2 #xfa #xb3 #xfa #xb4 #xfa #xb5 
        #xfa #xb6 #xfa #xb7 #xfa #xb8 #xfa #xb9 
        #xfa #xba #xfa #xbb #xfa #xbc #xfa #xbd 
        #xfa #xbe #xfa #xbf #xfa #xc0 #xfa #xc1 
        #xfa #xc2 #xfa #xc3 #xfa #xc4 #xfa #xc5 
        #xfa #xc6 #xfa #xc7 #xfa #xc8 #xfa #xc9 
        #xfa #xca #xfa #xcb #xfa #xcc #xfa #xcd 
        #xfa #xce #xfa #xcf #xfa #xd0 #xfa #xd1 
        #xfa #xd2 #xfa #xd3 #xfa #xd4 #xfa #xd5 
        #xfa #xd6 #xfa #xd7 #xfa #xd8 #xfa #xd9 
        #xfb #x0 #xfb #x1 #xfb #x2 #xfb #x3 
        #xfb #x4 #xfb #x5 #xfb #x6 #xfb #x13 
        #xfb #x14 #xfb #x15 #xfb #x16 #xfb #x17 
        #xfb #x1d #xfb #x1f #xfb #x20 #xfb #x21 
        #xfb #x22 #xfb #x23 #xfb #x24 #xfb #x25 
        #xfb #x26 #xfb #x27 #xfb #x28 #xfb #x29 
        #xfb #x2a #xfb #x2b #xfb #x2c #xfb #x2d 
        #xfb #x2e #xfb #x2f #xfb #x30 #xfb #x31 
        #xfb #x32 #xfb #x33 #xfb #x34 #xfb #x35 
        #xfb #x36 #xfb #x38 #xfb #x39 #xfb #x3a 
        #xfb #x3b #xfb #x3c #xfb #x3e #xfb #x40 
        #xfb #x41 #xfb #x43 #xfb #x44 #xfb #x46 
        #xfb #x47 #xfb #x48 #xfb #x49 #xfb #x4a 
        #xfb #x4b #xfb #x4c #xfb #x4d #xfb #x4e 
        #xfb #x4f #xfb #x50 #xfb #x51 #xfb #x52 
        #xfb #x53 #xfb #x54 #xfb #x55 #xfb #x56 
        #xfb #x57 #xfb #x58 #xfb #x59 #xfb #x5a 
        #xfb #x5b #xfb #x5c #xfb #x5d #xfb #x5e 
        #xfb #x5f #xfb #x60 #xfb #x61 #xfb #x62 
        #xfb #x63 #xfb #x64 #xfb #x65 #xfb #x66 
        #xfb #x67 #xfb #x68 #xfb #x69 #xfb #x6a 
        #xfb #x6b #xfb #x6c #xfb #x6d #xfb #x6e 
        #xfb #x6f #xfb #x70 #xfb #x71 #xfb #x72 
        #xfb #x73 #xfb #x74 #xfb #x75 #xfb #x76 
        #xfb #x77 #xfb #x78 #xfb #x79 #xfb #x7a 
        #xfb #x7b #xfb #x7c #xfb #x7d #xfb #x7e 
        #xfb #x7f #xfb #x80 #xfb #x81 #xfb #x82 
        #xfb #x83 #xfb #x84 #xfb #x85 #xfb #x86 
        #xfb #x87 #xfb #x88 #xfb #x89 #xfb #x8a 
        #xfb #x8b #xfb #x8c #xfb #x8d #xfb #x8e 
        #xfb #x8f #xfb #x90 #xfb #x91 #xfb #x92 
        #xfb #x93 #xfb #x94 #xfb #x95 #xfb #x96 
        #xfb #x97 #xfb #x98 #xfb #x99 #xfb #x9a 
        #xfb #x9b #xfb #x9c #xfb #x9d #xfb #x9e 
        #xfb #x9f #xfb #xa0 #xfb #xa1 #xfb #xa2 
        #xfb #xa3 #xfb #xa4 #xfb #xa5 #xfb #xa6 
        #xfb #xa7 #xfb #xa8 #xfb #xa9 #xfb #xaa 
        #xfb #xab #xfb #xac #xfb #xad #xfb #xae 
        #xfb #xaf #xfb #xb0 #xfb #xb1 #xfb #xd3 
        #xfb #xd4 #xfb #xd5 #xfb #xd6 #xfb #xd7 
        #xfb #xd8 #xfb #xd9 #xfb #xda #xfb #xdb 
        #xfb #xdc #xfb #xdd #xfb #xde #xfb #xdf 
        #xfb #xe0 #xfb #xe1 #xfb #xe2 #xfb #xe3 
        #xfb #xe4 #xfb #xe5 #xfb #xe6 #xfb #xe7 
        #xfb #xe8 #xfb #xe9 #xfb #xea #xfb #xeb 
        #xfb #xec #xfb #xed #xfb #xee #xfb #xef 
        #xfb #xf0 #xfb #xf1 #xfb #xf2 #xfb #xf3 
        #xfb #xf4 #xfb #xf5 #xfb #xf6 #xfb #xf7 
        #xfb #xf8 #xfb #xf9 #xfb #xfa #xfb #xfb 
        #xfb #xfc #xfb #xfd #xfb #xfe #xfb #xff 
        #xfc #x0 #xfc #x1 #xfc #x2 #xfc #x3 
        #xfc #x4 #xfc #x5 #xfc #x6 #xfc #x7 
        #xfc #x8 #xfc #x9 #xfc #xa #xfc #xb 
        #xfc #xc #xfc #xd #xfc #xe #xfc #xf 
        #xfc #x10 #xfc #x11 #xfc #x12 #xfc #x13 
        #xfc #x14 #xfc #x15 #xfc #x16 #xfc #x17 
        #xfc #x18 #xfc #x19 #xfc #x1a #xfc #x1b 
        #xfc #x1c #xfc #x1d #xfc #x1e #xfc #x1f 
        #xfc #x20 #xfc #x21 #xfc #x22 #xfc #x23 
        #xfc #x24 #xfc #x25 #xfc #x26 #xfc #x27 
        #xfc #x28 #xfc #x29 #xfc #x2a #xfc #x2b 
        #xfc #x2c #xfc #x2d #xfc #x2e #xfc #x2f 
        #xfc #x30 #xfc #x31 #xfc #x32 #xfc #x33 
        #xfc #x34 #xfc #x35 #xfc #x36 #xfc #x37 
        #xfc #x38 #xfc #x39 #xfc #x3a #xfc #x3b 
        #xfc #x3c #xfc #x3d #xfc #x3e #xfc #x3f 
        #xfc #x40 #xfc #x41 #xfc #x42 #xfc #x43 
        #xfc #x44 #xfc #x45 #xfc #x46 #xfc #x47 
        #xfc #x48 #xfc #x49 #xfc #x4a #xfc #x4b 
        #xfc #x4c #xfc #x4d #xfc #x4e #xfc #x4f 
        #xfc #x50 #xfc #x51 #xfc #x52 #xfc #x53 
        #xfc #x54 #xfc #x55 #xfc #x56 #xfc #x57 
        #xfc #x58 #xfc #x59 #xfc #x5a #xfc #x5b 
        #xfc #x5c #xfc #x5d #xfc #x5e #xfc #x5f 
        #xfc #x60 #xfc #x61 #xfc #x62 #xfc #x63 
        #xfc #x64 #xfc #x65 #xfc #x66 #xfc #x67 
        #xfc #x68 #xfc #x69 #xfc #x6a #xfc #x6b 
        #xfc #x6c #xfc #x6d #xfc #x6e #xfc #x6f 
        #xfc #x70 #xfc #x71 #xfc #x72 #xfc #x73 
        #xfc #x74 #xfc #x75 #xfc #x76 #xfc #x77 
        #xfc #x78 #xfc #x79 #xfc #x7a #xfc #x7b 
        #xfc #x7c #xfc #x7d #xfc #x7e #xfc #x7f 
        #xfc #x80 #xfc #x81 #xfc #x82 #xfc #x83 
        #xfc #x84 #xfc #x85 #xfc #x86 #xfc #x87 
        #xfc #x88 #xfc #x89 #xfc #x8a #xfc #x8b 
        #xfc #x8c #xfc #x8d #xfc #x8e #xfc #x8f 
        #xfc #x90 #xfc #x91 #xfc #x92 #xfc #x93 
        #xfc #x94 #xfc #x95 #xfc #x96 #xfc #x97 
        #xfc #x98 #xfc #x99 #xfc #x9a #xfc #x9b 
        #xfc #x9c #xfc #x9d #xfc #x9e #xfc #x9f 
        #xfc #xa0 #xfc #xa1 #xfc #xa2 #xfc #xa3 
        #xfc #xa4 #xfc #xa5 #xfc #xa6 #xfc #xa7 
        #xfc #xa8 #xfc #xa9 #xfc #xaa #xfc #xab 
        #xfc #xac #xfc #xad #xfc #xae #xfc #xaf 
        #xfc #xb0 #xfc #xb1 #xfc #xb2 #xfc #xb3 
        #xfc #xb4 #xfc #xb5 #xfc #xb6 #xfc #xb7 
        #xfc #xb8 #xfc #xb9 #xfc #xba #xfc #xbb 
        #xfc #xbc #xfc #xbd #xfc #xbe #xfc #xbf 
        #xfc #xc0 #xfc #xc1 #xfc #xc2 #xfc #xc3 
        #xfc #xc4 #xfc #xc5 #xfc #xc6 #xfc #xc7 
        #xfc #xc8 #xfc #xc9 #xfc #xca #xfc #xcb 
        #xfc #xcc #xfc #xcd #xfc #xce #xfc #xcf 
        #xfc #xd0 #xfc #xd1 #xfc #xd2 #xfc #xd3 
        #xfc #xd4 #xfc #xd5 #xfc #xd6 #xfc #xd7 
        #xfc #xd8 #xfc #xd9 #xfc #xda #xfc #xdb 
        #xfc #xdc #xfc #xdd #xfc #xde #xfc #xdf 
        #xfc #xe0 #xfc #xe1 #xfc #xe2 #xfc #xe3 
        #xfc #xe4 #xfc #xe5 #xfc #xe6 #xfc #xe7 
        #xfc #xe8 #xfc #xe9 #xfc #xea #xfc #xeb 
        #xfc #xec #xfc #xed #xfc #xee #xfc #xef 
        #xfc #xf0 #xfc #xf1 #xfc #xf2 #xfc #xf3 
        #xfc #xf4 #xfc #xf5 #xfc #xf6 #xfc #xf7 
        #xfc #xf8 #xfc #xf9 #xfc #xfa #xfc #xfb 
        #xfc #xfc #xfc #xfd #xfc #xfe #xfc #xff 
        #xfd #x0 #xfd #x1 #xfd #x2 #xfd #x3 
        #xfd #x4 #xfd #x5 #xfd #x6 #xfd #x7 
        #xfd #x8 #xfd #x9 #xfd #xa #xfd #xb 
        #xfd #xc #xfd #xd #xfd #xe #xfd #xf 
        #xfd #x10 #xfd #x11 #xfd #x12 #xfd #x13 
        #xfd #x14 #xfd #x15 #xfd #x16 #xfd #x17 
        #xfd #x18 #xfd #x19 #xfd #x1a #xfd #x1b 
        #xfd #x1c #xfd #x1d #xfd #x1e #xfd #x1f 
        #xfd #x20 #xfd #x21 #xfd #x22 #xfd #x23 
        #xfd #x24 #xfd #x25 #xfd #x26 #xfd #x27 
        #xfd #x28 #xfd #x29 #xfd #x2a #xfd #x2b 
        #xfd #x2c #xfd #x2d #xfd #x2e #xfd #x2f 
        #xfd #x30 #xfd #x31 #xfd #x32 #xfd #x33 
        #xfd #x34 #xfd #x35 #xfd #x36 #xfd #x37 
        #xfd #x38 #xfd #x39 #xfd #x3a #xfd #x3b 
        #xfd #x3c #xfd #x3d #xfd #x50 #xfd #x51 
        #xfd #x52 #xfd #x53 #xfd #x54 #xfd #x55 
        #xfd #x56 #xfd #x57 #xfd #x58 #xfd #x59 
        #xfd #x5a #xfd #x5b #xfd #x5c #xfd #x5d 
        #xfd #x5e #xfd #x5f #xfd #x60 #xfd #x61 
        #xfd #x62 #xfd #x63 #xfd #x64 #xfd #x65 
        #xfd #x66 #xfd #x67 #xfd #x68 #xfd #x69 
        #xfd #x6a #xfd #x6b #xfd #x6c #xfd #x6d 
        #xfd #x6e #xfd #x6f #xfd #x70 #xfd #x71 
        #xfd #x72 #xfd #x73 #xfd #x74 #xfd #x75 
        #xfd #x76 #xfd #x77 #xfd #x78 #xfd #x79 
        #xfd #x7a #xfd #x7b #xfd #x7c #xfd #x7d 
        #xfd #x7e #xfd #x7f #xfd #x80 #xfd #x81 
        #xfd #x82 #xfd #x83 #xfd #x84 #xfd #x85 
        #xfd #x86 #xfd #x87 #xfd #x88 #xfd #x89 
        #xfd #x8a #xfd #x8b #xfd #x8c #xfd #x8d 
        #xfd #x8e #xfd #x8f #xfd #x92 #xfd #x93 
        #xfd #x94 #xfd #x95 #xfd #x96 #xfd #x97 
        #xfd #x98 #xfd #x99 #xfd #x9a #xfd #x9b 
        #xfd #x9c #xfd #x9d #xfd #x9e #xfd #x9f 
        #xfd #xa0 #xfd #xa1 #xfd #xa2 #xfd #xa3 
        #xfd #xa4 #xfd #xa5 #xfd #xa6 #xfd #xa7 
        #xfd #xa8 #xfd #xa9 #xfd #xaa #xfd #xab 
        #xfd #xac #xfd #xad #xfd #xae #xfd #xaf 
        #xfd #xb0 #xfd #xb1 #xfd #xb2 #xfd #xb3 
        #xfd #xb4 #xfd #xb5 #xfd #xb6 #xfd #xb7 
        #xfd #xb8 #xfd #xb9 #xfd #xba #xfd #xbb 
        #xfd #xbc #xfd #xbd #xfd #xbe #xfd #xbf 
        #xfd #xc0 #xfd #xc1 #xfd #xc2 #xfd #xc3 
        #xfd #xc4 #xfd #xc5 #xfd #xc6 #xfd #xc7 
        #xfd #xf0 #xfd #xf1 #xfd #xf2 #xfd #xf3 
        #xfd #xf4 #xfd #xf5 #xfd #xf6 #xfd #xf7 
        #xfd #xf8 #xfd #xf9 #xfd #xfa #xfd #xfb 
        #xfd #xfc #xfe #x10 #xfe #x11 #xfe #x12 
        #xfe #x13 #xfe #x14 #xfe #x15 #xfe #x16 
        #xfe #x17 #xfe #x18 #xfe #x19 #xfe #x30 
        #xfe #x31 #xfe #x32 #xfe #x33 #xfe #x34 
        #xfe #x35 #xfe #x36 #xfe #x37 #xfe #x38 
        #xfe #x39 #xfe #x3a #xfe #x3b #xfe #x3c 
        #xfe #x3d #xfe #x3e #xfe #x3f #xfe #x40 
        #xfe #x41 #xfe #x42 #xfe #x43 #xfe #x44 
        #xfe #x47 #xfe #x48 #xfe #x49 #xfe #x4a 
        #xfe #x4b #xfe #x4c #xfe #x4d #xfe #x4e 
        #xfe #x4f #xfe #x50 #xfe #x51 #xfe #x52 
        #xfe #x54 #xfe #x55 #xfe #x56 #xfe #x57 
        #xfe #x58 #xfe #x59 #xfe #x5a #xfe #x5b 
        #xfe #x5c #xfe #x5d #xfe #x5e #xfe #x5f 
        #xfe #x60 #xfe #x61 #xfe #x62 #xfe #x63 
        #xfe #x64 #xfe #x65 #xfe #x66 #xfe #x68 
        #xfe #x69 #xfe #x6a #xfe #x6b #xfe #x70 
        #xfe #x71 #xfe #x72 #xfe #x74 #xfe #x76 
        #xfe #x77 #xfe #x78 #xfe #x79 #xfe #x7a 
        #xfe #x7b #xfe #x7c #xfe #x7d #xfe #x7e 
        #xfe #x7f #xfe #x80 #xfe #x81 #xfe #x82 
        #xfe #x83 #xfe #x84 #xfe #x85 #xfe #x86 
        #xfe #x87 #xfe #x88 #xfe #x89 #xfe #x8a 
        #xfe #x8b #xfe #x8c #xfe #x8d #xfe #x8e 
        #xfe #x8f #xfe #x90 #xfe #x91 #xfe #x92 
        #xfe #x93 #xfe #x94 #xfe #x95 #xfe #x96 
        #xfe #x97 #xfe #x98 #xfe #x99 #xfe #x9a 
        #xfe #x9b #xfe #x9c #xfe #x9d #xfe #x9e 
        #xfe #x9f #xfe #xa0 #xfe #xa1 #xfe #xa2 
        #xfe #xa3 #xfe #xa4 #xfe #xa5 #xfe #xa6 
        #xfe #xa7 #xfe #xa8 #xfe #xa9 #xfe #xaa 
        #xfe #xab #xfe #xac #xfe #xad #xfe #xae 
        #xfe #xaf #xfe #xb0 #xfe #xb1 #xfe #xb2 
        #xfe #xb3 #xfe #xb4 #xfe #xb5 #xfe #xb6 
        #xfe #xb7 #xfe #xb8 #xfe #xb9 #xfe #xba 
        #xfe #xbb #xfe #xbc #xfe #xbd #xfe #xbe 
        #xfe #xbf #xfe #xc0 #xfe #xc1 #xfe #xc2 
        #xfe #xc3 #xfe #xc4 #xfe #xc5 #xfe #xc6 
        #xfe #xc7 #xfe #xc8 #xfe #xc9 #xfe #xca 
        #xfe #xcb #xfe #xcc #xfe #xcd #xfe #xce 
        #xfe #xcf #xfe #xd0 #xfe #xd1 #xfe #xd2 
        #xfe #xd3 #xfe #xd4 #xfe #xd5 #xfe #xd6 
        #xfe #xd7 #xfe #xd8 #xfe #xd9 #xfe #xda 
        #xfe #xdb #xfe #xdc #xfe #xdd #xfe #xde 
        #xfe #xdf #xfe #xe0 #xfe #xe1 #xfe #xe2 
        #xfe #xe3 #xfe #xe4 #xfe #xe5 #xfe #xe6 
        #xfe #xe7 #xfe #xe8 #xfe #xe9 #xfe #xea 
        #xfe #xeb #xfe #xec #xfe #xed #xfe #xee 
        #xfe #xef #xfe #xf0 #xfe #xf1 #xfe #xf2 
        #xfe #xf3 #xfe #xf4 #xfe #xf5 #xfe #xf6 
        #xfe #xf7 #xfe #xf8 #xfe #xf9 #xfe #xfa 
        #xfe #xfb #xfe #xfc #xff #x1 #xff #x2 
        #xff #x3 #xff #x4 #xff #x5 #xff #x6 
        #xff #x7 #xff #x8 #xff #x9 #xff #xa 
        #xff #xb #xff #xc #xff #xd #xff #xe 
        #xff #xf #xff #x10 #xff #x11 #xff #x12 
        #xff #x13 #xff #x14 #xff #x15 #xff #x16 
        #xff #x17 #xff #x18 #xff #x19 #xff #x1a 
        #xff #x1b #xff #x1c #xff #x1d #xff #x1e 
        #xff #x1f #xff #x20 #xff #x21 #xff #x22 
        #xff #x23 #xff #x24 #xff #x25 #xff #x26 
        #xff #x27 #xff #x28 #xff #x29 #xff #x2a 
        #xff #x2b #xff #x2c #xff #x2d #xff #x2e 
        #xff #x2f #xff #x30 #xff #x31 #xff #x32 
        #xff #x33 #xff #x34 #xff #x35 #xff #x36 
        #xff #x37 #xff #x38 #xff #x39 #xff #x3a 
        #xff #x3b #xff #x3c #xff #x3d #xff #x3e 
        #xff #x3f #xff #x40 #xff #x41 #xff #x42 
        #xff #x43 #xff #x44 #xff #x45 #xff #x46 
        #xff #x47 #xff #x48 #xff #x49 #xff #x4a 
        #xff #x4b #xff #x4c #xff #x4d #xff #x4e 
        #xff #x4f #xff #x50 #xff #x51 #xff #x52 
        #xff #x53 #xff #x54 #xff #x55 #xff #x56 
        #xff #x57 #xff #x58 #xff #x59 #xff #x5a 
        #xff #x5b #xff #x5c #xff #x5d #xff #x5e 
        #xff #x5f #xff #x60 #xff #x61 #xff #x62 
        #xff #x63 #xff #x64 #xff #x65 #xff #x66 
        #xff #x67 #xff #x68 #xff #x69 #xff #x6a 
        #xff #x6b #xff #x6c #xff #x6d #xff #x6e 
        #xff #x6f #xff #x70 #xff #x71 #xff #x72 
        #xff #x73 #xff #x74 #xff #x75 #xff #x76 
        #xff #x77 #xff #x78 #xff #x79 #xff #x7a 
        #xff #x7b #xff #x7c #xff #x7d #xff #x7e 
        #xff #x7f #xff #x80 #xff #x81 #xff #x82 
        #xff #x83 #xff #x84 #xff #x85 #xff #x86 
        #xff #x87 #xff #x88 #xff #x89 #xff #x8a 
        #xff #x8b #xff #x8c #xff #x8d #xff #x8e 
        #xff #x8f #xff #x90 #xff #x91 #xff #x92 
        #xff #x93 #xff #x94 #xff #x95 #xff #x96 
        #xff #x97 #xff #x98 #xff #x99 #xff #x9a 
        #xff #x9b #xff #x9c #xff #x9d #xff #x9e 
        #xff #x9f #xff #xa0 #xff #xa1 #xff #xa2 
        #xff #xa3 #xff #xa4 #xff #xa5 #xff #xa6 
        #xff #xa7 #xff #xa8 #xff #xa9 #xff #xaa 
        #xff #xab #xff #xac #xff #xad #xff #xae 
        #xff #xaf #xff #xb0 #xff #xb1 #xff #xb2 
        #xff #xb3 #xff #xb4 #xff #xb5 #xff #xb6 
        #xff #xb7 #xff #xb8 #xff #xb9 #xff #xba 
        #xff #xbb #xff #xbc #xff #xbd #xff #xbe 
        #xff #xc2 #xff #xc3 #xff #xc4 #xff #xc5 
        #xff #xc6 #xff #xc7 #xff #xca #xff #xcb 
        #xff #xcc #xff #xcd #xff #xce #xff #xcf 
        #xff #xd2 #xff #xd3 #xff #xd4 #xff #xd5 
        #xff #xd6 #xff #xd7 #xff #xda #xff #xdb 
        #xff #xdc #xff #xe0 #xff #xe1 #xff #xe2 
        #xff #xe3 #xff #xe4 #xff #xe5 #xff #xe6 
        #xff #xe8 #xff #xe9 #xff #xea #xff #xeb 
        #xff #xec #xff #xed #xff #xee ))
)

; This vector contains all other code points,
; in increasing order, that have a canonical
; or compatibility decomposition.
;
; This table contains 1551 elements.

(define decomposition-chars-morebits
  '#(
     #x1d15e #x1d15f #x1d160 #x1d161 #x1d162 #x1d163 #x1d164 #x1d1bb 
     #x1d1bc #x1d1bd #x1d1be #x1d1bf #x1d1c0 #x1d400 #x1d401 #x1d402 
     #x1d403 #x1d404 #x1d405 #x1d406 #x1d407 #x1d408 #x1d409 #x1d40a 
     #x1d40b #x1d40c #x1d40d #x1d40e #x1d40f #x1d410 #x1d411 #x1d412 
     #x1d413 #x1d414 #x1d415 #x1d416 #x1d417 #x1d418 #x1d419 #x1d41a 
     #x1d41b #x1d41c #x1d41d #x1d41e #x1d41f #x1d420 #x1d421 #x1d422 
     #x1d423 #x1d424 #x1d425 #x1d426 #x1d427 #x1d428 #x1d429 #x1d42a 
     #x1d42b #x1d42c #x1d42d #x1d42e #x1d42f #x1d430 #x1d431 #x1d432 
     #x1d433 #x1d434 #x1d435 #x1d436 #x1d437 #x1d438 #x1d439 #x1d43a 
     #x1d43b #x1d43c #x1d43d #x1d43e #x1d43f #x1d440 #x1d441 #x1d442 
     #x1d443 #x1d444 #x1d445 #x1d446 #x1d447 #x1d448 #x1d449 #x1d44a 
     #x1d44b #x1d44c #x1d44d #x1d44e #x1d44f #x1d450 #x1d451 #x1d452 
     #x1d453 #x1d454 #x1d456 #x1d457 #x1d458 #x1d459 #x1d45a #x1d45b 
     #x1d45c #x1d45d #x1d45e #x1d45f #x1d460 #x1d461 #x1d462 #x1d463 
     #x1d464 #x1d465 #x1d466 #x1d467 #x1d468 #x1d469 #x1d46a #x1d46b 
     #x1d46c #x1d46d #x1d46e #x1d46f #x1d470 #x1d471 #x1d472 #x1d473 
     #x1d474 #x1d475 #x1d476 #x1d477 #x1d478 #x1d479 #x1d47a #x1d47b 
     #x1d47c #x1d47d #x1d47e #x1d47f #x1d480 #x1d481 #x1d482 #x1d483 
     #x1d484 #x1d485 #x1d486 #x1d487 #x1d488 #x1d489 #x1d48a #x1d48b 
     #x1d48c #x1d48d #x1d48e #x1d48f #x1d490 #x1d491 #x1d492 #x1d493 
     #x1d494 #x1d495 #x1d496 #x1d497 #x1d498 #x1d499 #x1d49a #x1d49b 
     #x1d49c #x1d49e #x1d49f #x1d4a2 #x1d4a5 #x1d4a6 #x1d4a9 #x1d4aa 
     #x1d4ab #x1d4ac #x1d4ae #x1d4af #x1d4b0 #x1d4b1 #x1d4b2 #x1d4b3 
     #x1d4b4 #x1d4b5 #x1d4b6 #x1d4b7 #x1d4b8 #x1d4b9 #x1d4bb #x1d4bd 
     #x1d4be #x1d4bf #x1d4c0 #x1d4c1 #x1d4c2 #x1d4c3 #x1d4c5 #x1d4c6 
     #x1d4c7 #x1d4c8 #x1d4c9 #x1d4ca #x1d4cb #x1d4cc #x1d4cd #x1d4ce 
     #x1d4cf #x1d4d0 #x1d4d1 #x1d4d2 #x1d4d3 #x1d4d4 #x1d4d5 #x1d4d6 
     #x1d4d7 #x1d4d8 #x1d4d9 #x1d4da #x1d4db #x1d4dc #x1d4dd #x1d4de 
     #x1d4df #x1d4e0 #x1d4e1 #x1d4e2 #x1d4e3 #x1d4e4 #x1d4e5 #x1d4e6 
     #x1d4e7 #x1d4e8 #x1d4e9 #x1d4ea #x1d4eb #x1d4ec #x1d4ed #x1d4ee 
     #x1d4ef #x1d4f0 #x1d4f1 #x1d4f2 #x1d4f3 #x1d4f4 #x1d4f5 #x1d4f6 
     #x1d4f7 #x1d4f8 #x1d4f9 #x1d4fa #x1d4fb #x1d4fc #x1d4fd #x1d4fe 
     #x1d4ff #x1d500 #x1d501 #x1d502 #x1d503 #x1d504 #x1d505 #x1d507 
     #x1d508 #x1d509 #x1d50a #x1d50d #x1d50e #x1d50f #x1d510 #x1d511 
     #x1d512 #x1d513 #x1d514 #x1d516 #x1d517 #x1d518 #x1d519 #x1d51a 
     #x1d51b #x1d51c #x1d51e #x1d51f #x1d520 #x1d521 #x1d522 #x1d523 
     #x1d524 #x1d525 #x1d526 #x1d527 #x1d528 #x1d529 #x1d52a #x1d52b 
     #x1d52c #x1d52d #x1d52e #x1d52f #x1d530 #x1d531 #x1d532 #x1d533 
     #x1d534 #x1d535 #x1d536 #x1d537 #x1d538 #x1d539 #x1d53b #x1d53c 
     #x1d53d #x1d53e #x1d540 #x1d541 #x1d542 #x1d543 #x1d544 #x1d546 
     #x1d54a #x1d54b #x1d54c #x1d54d #x1d54e #x1d54f #x1d550 #x1d552 
     #x1d553 #x1d554 #x1d555 #x1d556 #x1d557 #x1d558 #x1d559 #x1d55a 
     #x1d55b #x1d55c #x1d55d #x1d55e #x1d55f #x1d560 #x1d561 #x1d562 
     #x1d563 #x1d564 #x1d565 #x1d566 #x1d567 #x1d568 #x1d569 #x1d56a 
     #x1d56b #x1d56c #x1d56d #x1d56e #x1d56f #x1d570 #x1d571 #x1d572 
     #x1d573 #x1d574 #x1d575 #x1d576 #x1d577 #x1d578 #x1d579 #x1d57a 
     #x1d57b #x1d57c #x1d57d #x1d57e #x1d57f #x1d580 #x1d581 #x1d582 
     #x1d583 #x1d584 #x1d585 #x1d586 #x1d587 #x1d588 #x1d589 #x1d58a 
     #x1d58b #x1d58c #x1d58d #x1d58e #x1d58f #x1d590 #x1d591 #x1d592 
     #x1d593 #x1d594 #x1d595 #x1d596 #x1d597 #x1d598 #x1d599 #x1d59a 
     #x1d59b #x1d59c #x1d59d #x1d59e #x1d59f #x1d5a0 #x1d5a1 #x1d5a2 
     #x1d5a3 #x1d5a4 #x1d5a5 #x1d5a6 #x1d5a7 #x1d5a8 #x1d5a9 #x1d5aa 
     #x1d5ab #x1d5ac #x1d5ad #x1d5ae #x1d5af #x1d5b0 #x1d5b1 #x1d5b2 
     #x1d5b3 #x1d5b4 #x1d5b5 #x1d5b6 #x1d5b7 #x1d5b8 #x1d5b9 #x1d5ba 
     #x1d5bb #x1d5bc #x1d5bd #x1d5be #x1d5bf #x1d5c0 #x1d5c1 #x1d5c2 
     #x1d5c3 #x1d5c4 #x1d5c5 #x1d5c6 #x1d5c7 #x1d5c8 #x1d5c9 #x1d5ca 
     #x1d5cb #x1d5cc #x1d5cd #x1d5ce #x1d5cf #x1d5d0 #x1d5d1 #x1d5d2 
     #x1d5d3 #x1d5d4 #x1d5d5 #x1d5d6 #x1d5d7 #x1d5d8 #x1d5d9 #x1d5da 
     #x1d5db #x1d5dc #x1d5dd #x1d5de #x1d5df #x1d5e0 #x1d5e1 #x1d5e2 
     #x1d5e3 #x1d5e4 #x1d5e5 #x1d5e6 #x1d5e7 #x1d5e8 #x1d5e9 #x1d5ea 
     #x1d5eb #x1d5ec #x1d5ed #x1d5ee #x1d5ef #x1d5f0 #x1d5f1 #x1d5f2 
     #x1d5f3 #x1d5f4 #x1d5f5 #x1d5f6 #x1d5f7 #x1d5f8 #x1d5f9 #x1d5fa 
     #x1d5fb #x1d5fc #x1d5fd #x1d5fe #x1d5ff #x1d600 #x1d601 #x1d602 
     #x1d603 #x1d604 #x1d605 #x1d606 #x1d607 #x1d608 #x1d609 #x1d60a 
     #x1d60b #x1d60c #x1d60d #x1d60e #x1d60f #x1d610 #x1d611 #x1d612 
     #x1d613 #x1d614 #x1d615 #x1d616 #x1d617 #x1d618 #x1d619 #x1d61a 
     #x1d61b #x1d61c #x1d61d #x1d61e #x1d61f #x1d620 #x1d621 #x1d622 
     #x1d623 #x1d624 #x1d625 #x1d626 #x1d627 #x1d628 #x1d629 #x1d62a 
     #x1d62b #x1d62c #x1d62d #x1d62e #x1d62f #x1d630 #x1d631 #x1d632 
     #x1d633 #x1d634 #x1d635 #x1d636 #x1d637 #x1d638 #x1d639 #x1d63a 
     #x1d63b #x1d63c #x1d63d #x1d63e #x1d63f #x1d640 #x1d641 #x1d642 
     #x1d643 #x1d644 #x1d645 #x1d646 #x1d647 #x1d648 #x1d649 #x1d64a 
     #x1d64b #x1d64c #x1d64d #x1d64e #x1d64f #x1d650 #x1d651 #x1d652 
     #x1d653 #x1d654 #x1d655 #x1d656 #x1d657 #x1d658 #x1d659 #x1d65a 
     #x1d65b #x1d65c #x1d65d #x1d65e #x1d65f #x1d660 #x1d661 #x1d662 
     #x1d663 #x1d664 #x1d665 #x1d666 #x1d667 #x1d668 #x1d669 #x1d66a 
     #x1d66b #x1d66c #x1d66d #x1d66e #x1d66f #x1d670 #x1d671 #x1d672 
     #x1d673 #x1d674 #x1d675 #x1d676 #x1d677 #x1d678 #x1d679 #x1d67a 
     #x1d67b #x1d67c #x1d67d #x1d67e #x1d67f #x1d680 #x1d681 #x1d682 
     #x1d683 #x1d684 #x1d685 #x1d686 #x1d687 #x1d688 #x1d689 #x1d68a 
     #x1d68b #x1d68c #x1d68d #x1d68e #x1d68f #x1d690 #x1d691 #x1d692 
     #x1d693 #x1d694 #x1d695 #x1d696 #x1d697 #x1d698 #x1d699 #x1d69a 
     #x1d69b #x1d69c #x1d69d #x1d69e #x1d69f #x1d6a0 #x1d6a1 #x1d6a2 
     #x1d6a3 #x1d6a4 #x1d6a5 #x1d6a8 #x1d6a9 #x1d6aa #x1d6ab #x1d6ac 
     #x1d6ad #x1d6ae #x1d6af #x1d6b0 #x1d6b1 #x1d6b2 #x1d6b3 #x1d6b4 
     #x1d6b5 #x1d6b6 #x1d6b7 #x1d6b8 #x1d6b9 #x1d6ba #x1d6bb #x1d6bc 
     #x1d6bd #x1d6be #x1d6bf #x1d6c0 #x1d6c1 #x1d6c2 #x1d6c3 #x1d6c4 
     #x1d6c5 #x1d6c6 #x1d6c7 #x1d6c8 #x1d6c9 #x1d6ca #x1d6cb #x1d6cc 
     #x1d6cd #x1d6ce #x1d6cf #x1d6d0 #x1d6d1 #x1d6d2 #x1d6d3 #x1d6d4 
     #x1d6d5 #x1d6d6 #x1d6d7 #x1d6d8 #x1d6d9 #x1d6da #x1d6db #x1d6dc 
     #x1d6dd #x1d6de #x1d6df #x1d6e0 #x1d6e1 #x1d6e2 #x1d6e3 #x1d6e4 
     #x1d6e5 #x1d6e6 #x1d6e7 #x1d6e8 #x1d6e9 #x1d6ea #x1d6eb #x1d6ec 
     #x1d6ed #x1d6ee #x1d6ef #x1d6f0 #x1d6f1 #x1d6f2 #x1d6f3 #x1d6f4 
     #x1d6f5 #x1d6f6 #x1d6f7 #x1d6f8 #x1d6f9 #x1d6fa #x1d6fb #x1d6fc 
     #x1d6fd #x1d6fe #x1d6ff #x1d700 #x1d701 #x1d702 #x1d703 #x1d704 
     #x1d705 #x1d706 #x1d707 #x1d708 #x1d709 #x1d70a #x1d70b #x1d70c 
     #x1d70d #x1d70e #x1d70f #x1d710 #x1d711 #x1d712 #x1d713 #x1d714 
     #x1d715 #x1d716 #x1d717 #x1d718 #x1d719 #x1d71a #x1d71b #x1d71c 
     #x1d71d #x1d71e #x1d71f #x1d720 #x1d721 #x1d722 #x1d723 #x1d724 
     #x1d725 #x1d726 #x1d727 #x1d728 #x1d729 #x1d72a #x1d72b #x1d72c 
     #x1d72d #x1d72e #x1d72f #x1d730 #x1d731 #x1d732 #x1d733 #x1d734 
     #x1d735 #x1d736 #x1d737 #x1d738 #x1d739 #x1d73a #x1d73b #x1d73c 
     #x1d73d #x1d73e #x1d73f #x1d740 #x1d741 #x1d742 #x1d743 #x1d744 
     #x1d745 #x1d746 #x1d747 #x1d748 #x1d749 #x1d74a #x1d74b #x1d74c 
     #x1d74d #x1d74e #x1d74f #x1d750 #x1d751 #x1d752 #x1d753 #x1d754 
     #x1d755 #x1d756 #x1d757 #x1d758 #x1d759 #x1d75a #x1d75b #x1d75c 
     #x1d75d #x1d75e #x1d75f #x1d760 #x1d761 #x1d762 #x1d763 #x1d764 
     #x1d765 #x1d766 #x1d767 #x1d768 #x1d769 #x1d76a #x1d76b #x1d76c 
     #x1d76d #x1d76e #x1d76f #x1d770 #x1d771 #x1d772 #x1d773 #x1d774 
     #x1d775 #x1d776 #x1d777 #x1d778 #x1d779 #x1d77a #x1d77b #x1d77c 
     #x1d77d #x1d77e #x1d77f #x1d780 #x1d781 #x1d782 #x1d783 #x1d784 
     #x1d785 #x1d786 #x1d787 #x1d788 #x1d789 #x1d78a #x1d78b #x1d78c 
     #x1d78d #x1d78e #x1d78f #x1d790 #x1d791 #x1d792 #x1d793 #x1d794 
     #x1d795 #x1d796 #x1d797 #x1d798 #x1d799 #x1d79a #x1d79b #x1d79c 
     #x1d79d #x1d79e #x1d79f #x1d7a0 #x1d7a1 #x1d7a2 #x1d7a3 #x1d7a4 
     #x1d7a5 #x1d7a6 #x1d7a7 #x1d7a8 #x1d7a9 #x1d7aa #x1d7ab #x1d7ac 
     #x1d7ad #x1d7ae #x1d7af #x1d7b0 #x1d7b1 #x1d7b2 #x1d7b3 #x1d7b4 
     #x1d7b5 #x1d7b6 #x1d7b7 #x1d7b8 #x1d7b9 #x1d7ba #x1d7bb #x1d7bc 
     #x1d7bd #x1d7be #x1d7bf #x1d7c0 #x1d7c1 #x1d7c2 #x1d7c3 #x1d7c4 
     #x1d7c5 #x1d7c6 #x1d7c7 #x1d7c8 #x1d7c9 #x1d7ca #x1d7cb #x1d7ce 
     #x1d7cf #x1d7d0 #x1d7d1 #x1d7d2 #x1d7d3 #x1d7d4 #x1d7d5 #x1d7d6 
     #x1d7d7 #x1d7d8 #x1d7d9 #x1d7da #x1d7db #x1d7dc #x1d7dd #x1d7de 
     #x1d7df #x1d7e0 #x1d7e1 #x1d7e2 #x1d7e3 #x1d7e4 #x1d7e5 #x1d7e6 
     #x1d7e7 #x1d7e8 #x1d7e9 #x1d7ea #x1d7eb #x1d7ec #x1d7ed #x1d7ee 
     #x1d7ef #x1d7f0 #x1d7f1 #x1d7f2 #x1d7f3 #x1d7f4 #x1d7f5 #x1d7f6 
     #x1d7f7 #x1d7f8 #x1d7f9 #x1d7fa #x1d7fb #x1d7fc #x1d7fd #x1d7fe 
     #x1d7ff #x2f800 #x2f801 #x2f802 #x2f803 #x2f804 #x2f805 #x2f806 
     #x2f807 #x2f808 #x2f809 #x2f80a #x2f80b #x2f80c #x2f80d #x2f80e 
     #x2f80f #x2f810 #x2f811 #x2f812 #x2f813 #x2f814 #x2f815 #x2f816 
     #x2f817 #x2f818 #x2f819 #x2f81a #x2f81b #x2f81c #x2f81d #x2f81e 
     #x2f81f #x2f820 #x2f821 #x2f822 #x2f823 #x2f824 #x2f825 #x2f826 
     #x2f827 #x2f828 #x2f829 #x2f82a #x2f82b #x2f82c #x2f82d #x2f82e 
     #x2f82f #x2f830 #x2f831 #x2f832 #x2f833 #x2f834 #x2f835 #x2f836 
     #x2f837 #x2f838 #x2f839 #x2f83a #x2f83b #x2f83c #x2f83d #x2f83e 
     #x2f83f #x2f840 #x2f841 #x2f842 #x2f843 #x2f844 #x2f845 #x2f846 
     #x2f847 #x2f848 #x2f849 #x2f84a #x2f84b #x2f84c #x2f84d #x2f84e 
     #x2f84f #x2f850 #x2f851 #x2f852 #x2f853 #x2f854 #x2f855 #x2f856 
     #x2f857 #x2f858 #x2f859 #x2f85a #x2f85b #x2f85c #x2f85d #x2f85e 
     #x2f85f #x2f860 #x2f861 #x2f862 #x2f863 #x2f864 #x2f865 #x2f866 
     #x2f867 #x2f868 #x2f869 #x2f86a #x2f86b #x2f86c #x2f86d #x2f86e 
     #x2f86f #x2f870 #x2f871 #x2f872 #x2f873 #x2f874 #x2f875 #x2f876 
     #x2f877 #x2f878 #x2f879 #x2f87a #x2f87b #x2f87c #x2f87d #x2f87e 
     #x2f87f #x2f880 #x2f881 #x2f882 #x2f883 #x2f884 #x2f885 #x2f886 
     #x2f887 #x2f888 #x2f889 #x2f88a #x2f88b #x2f88c #x2f88d #x2f88e 
     #x2f88f #x2f890 #x2f891 #x2f892 #x2f893 #x2f894 #x2f895 #x2f896 
     #x2f897 #x2f898 #x2f899 #x2f89a #x2f89b #x2f89c #x2f89d #x2f89e 
     #x2f89f #x2f8a0 #x2f8a1 #x2f8a2 #x2f8a3 #x2f8a4 #x2f8a5 #x2f8a6 
     #x2f8a7 #x2f8a8 #x2f8a9 #x2f8aa #x2f8ab #x2f8ac #x2f8ad #x2f8ae 
     #x2f8af #x2f8b0 #x2f8b1 #x2f8b2 #x2f8b3 #x2f8b4 #x2f8b5 #x2f8b6 
     #x2f8b7 #x2f8b8 #x2f8b9 #x2f8ba #x2f8bb #x2f8bc #x2f8bd #x2f8be 
     #x2f8bf #x2f8c0 #x2f8c1 #x2f8c2 #x2f8c3 #x2f8c4 #x2f8c5 #x2f8c6 
     #x2f8c7 #x2f8c8 #x2f8c9 #x2f8ca #x2f8cb #x2f8cc #x2f8cd #x2f8ce 
     #x2f8cf #x2f8d0 #x2f8d1 #x2f8d2 #x2f8d3 #x2f8d4 #x2f8d5 #x2f8d6 
     #x2f8d7 #x2f8d8 #x2f8d9 #x2f8da #x2f8db #x2f8dc #x2f8dd #x2f8de 
     #x2f8df #x2f8e0 #x2f8e1 #x2f8e2 #x2f8e3 #x2f8e4 #x2f8e5 #x2f8e6 
     #x2f8e7 #x2f8e8 #x2f8e9 #x2f8ea #x2f8eb #x2f8ec #x2f8ed #x2f8ee 
     #x2f8ef #x2f8f0 #x2f8f1 #x2f8f2 #x2f8f3 #x2f8f4 #x2f8f5 #x2f8f6 
     #x2f8f7 #x2f8f8 #x2f8f9 #x2f8fa #x2f8fb #x2f8fc #x2f8fd #x2f8fe 
     #x2f8ff #x2f900 #x2f901 #x2f902 #x2f903 #x2f904 #x2f905 #x2f906 
     #x2f907 #x2f908 #x2f909 #x2f90a #x2f90b #x2f90c #x2f90d #x2f90e 
     #x2f90f #x2f910 #x2f911 #x2f912 #x2f913 #x2f914 #x2f915 #x2f916 
     #x2f917 #x2f918 #x2f919 #x2f91a #x2f91b #x2f91c #x2f91d #x2f91e 
     #x2f91f #x2f920 #x2f921 #x2f922 #x2f923 #x2f924 #x2f925 #x2f926 
     #x2f927 #x2f928 #x2f929 #x2f92a #x2f92b #x2f92c #x2f92d #x2f92e 
     #x2f92f #x2f930 #x2f931 #x2f932 #x2f933 #x2f934 #x2f935 #x2f936 
     #x2f937 #x2f938 #x2f939 #x2f93a #x2f93b #x2f93c #x2f93d #x2f93e 
     #x2f93f #x2f940 #x2f941 #x2f942 #x2f943 #x2f944 #x2f945 #x2f946 
     #x2f947 #x2f948 #x2f949 #x2f94a #x2f94b #x2f94c #x2f94d #x2f94e 
     #x2f94f #x2f950 #x2f951 #x2f952 #x2f953 #x2f954 #x2f955 #x2f956 
     #x2f957 #x2f958 #x2f959 #x2f95a #x2f95b #x2f95c #x2f95d #x2f95e 
     #x2f95f #x2f960 #x2f961 #x2f962 #x2f963 #x2f964 #x2f965 #x2f966 
     #x2f967 #x2f968 #x2f969 #x2f96a #x2f96b #x2f96c #x2f96d #x2f96e 
     #x2f96f #x2f970 #x2f971 #x2f972 #x2f973 #x2f974 #x2f975 #x2f976 
     #x2f977 #x2f978 #x2f979 #x2f97a #x2f97b #x2f97c #x2f97d #x2f97e 
     #x2f97f #x2f980 #x2f981 #x2f982 #x2f983 #x2f984 #x2f985 #x2f986 
     #x2f987 #x2f988 #x2f989 #x2f98a #x2f98b #x2f98c #x2f98d #x2f98e 
     #x2f98f #x2f990 #x2f991 #x2f992 #x2f993 #x2f994 #x2f995 #x2f996 
     #x2f997 #x2f998 #x2f999 #x2f99a #x2f99b #x2f99c #x2f99d #x2f99e 
     #x2f99f #x2f9a0 #x2f9a1 #x2f9a2 #x2f9a3 #x2f9a4 #x2f9a5 #x2f9a6 
     #x2f9a7 #x2f9a8 #x2f9a9 #x2f9aa #x2f9ab #x2f9ac #x2f9ad #x2f9ae 
     #x2f9af #x2f9b0 #x2f9b1 #x2f9b2 #x2f9b3 #x2f9b4 #x2f9b5 #x2f9b6 
     #x2f9b7 #x2f9b8 #x2f9b9 #x2f9ba #x2f9bb #x2f9bc #x2f9bd #x2f9be 
     #x2f9bf #x2f9c0 #x2f9c1 #x2f9c2 #x2f9c3 #x2f9c4 #x2f9c5 #x2f9c6 
     #x2f9c7 #x2f9c8 #x2f9c9 #x2f9ca #x2f9cb #x2f9cc #x2f9cd #x2f9ce 
     #x2f9cf #x2f9d0 #x2f9d1 #x2f9d2 #x2f9d3 #x2f9d4 #x2f9d5 #x2f9d6 
     #x2f9d7 #x2f9d8 #x2f9d9 #x2f9da #x2f9db #x2f9dc #x2f9dd #x2f9de 
     #x2f9df #x2f9e0 #x2f9e1 #x2f9e2 #x2f9e3 #x2f9e4 #x2f9e5 #x2f9e6 
     #x2f9e7 #x2f9e8 #x2f9e9 #x2f9ea #x2f9eb #x2f9ec #x2f9ed #x2f9ee 
     #x2f9ef #x2f9f0 #x2f9f1 #x2f9f2 #x2f9f3 #x2f9f4 #x2f9f5 #x2f9f6 
     #x2f9f7 #x2f9f8 #x2f9f9 #x2f9fa #x2f9fb #x2f9fc #x2f9fd #x2f9fe 
     #x2f9ff #x2fa00 #x2fa01 #x2fa02 #x2fa03 #x2fa04 #x2fa05 #x2fa06 
     #x2fa07 #x2fa08 #x2fa09 #x2fa0a #x2fa0b #x2fa0c #x2fa0d #x2fa0e 
     #x2fa0f #x2fa10 #x2fa11 #x2fa12 #x2fa13 #x2fa14 #x2fa15 #x2fa16 
     #x2fa17 #x2fa18 #x2fa19 #x2fa1a #x2fa1b #x2fa1c #x2fa1d ))

; This bytevector uses two bytes per index to list
; the starting indexes into decomposition-sequences
; of the canonical or compatibility decompositions
; for the code points in the above two tables.
; If the index is for a compatibility decomposition,
; then the high-order bit of the high-order (first)
; byte is set.
;
; This table contains 10806 elements.

(define decomposition-indexes
  ;'#vu8(
  (list->bytevector '(
        #x80 #x0 #x80 #x1 #x80 #x3 #x80 #x4 
        #x80 #x6 #x80 #x7 #x80 #x8 #x80 #xa 
        #x80 #xb #x80 #xd #x80 #xe #x80 #xf 
        #x80 #x12 #x80 #x15 #x0 #x18 #x0 #x1a 
        #x0 #x1c #x0 #x1e #x0 #x20 #x0 #x22 
        #x0 #x24 #x0 #x26 #x0 #x28 #x0 #x2a 
        #x0 #x2c #x0 #x2e #x0 #x30 #x0 #x32 
        #x0 #x34 #x0 #x36 #x0 #x38 #x0 #x3a 
        #x0 #x3c #x0 #x3e #x0 #x40 #x0 #x42 
        #x0 #x44 #x0 #x46 #x0 #x48 #x0 #x4a 
        #x0 #x4c #x0 #x4e #x0 #x50 #x0 #x52 
        #x0 #x54 #x0 #x56 #x0 #x58 #x0 #x5a 
        #x0 #x5c #x0 #x5e #x0 #x60 #x0 #x62 
        #x0 #x64 #x0 #x66 #x0 #x68 #x0 #x6a 
        #x0 #x6c #x0 #x6e #x0 #x70 #x0 #x72 
        #x0 #x74 #x0 #x76 #x0 #x78 #x0 #x7a 
        #x0 #x7c #x0 #x7e #x0 #x80 #x0 #x82 
        #x0 #x84 #x0 #x86 #x0 #x88 #x0 #x8a 
        #x0 #x8c #x0 #x8e #x0 #x90 #x0 #x92 
        #x0 #x94 #x0 #x96 #x0 #x98 #x0 #x9a 
        #x0 #x9c #x0 #x9e #x0 #xa0 #x0 #xa2 
        #x0 #xa4 #x0 #xa6 #x0 #xa8 #x0 #xaa 
        #x0 #xac #x0 #xae #x0 #xb0 #x0 #xb2 
        #x0 #xb4 #x0 #xb6 #x0 #xb8 #x0 #xba 
        #x0 #xbc #x0 #xbe #x0 #xc0 #x0 #xc2 
        #x0 #xc4 #x0 #xc6 #x0 #xc8 #x0 #xca 
        #x0 #xcc #x0 #xce #x0 #xd0 #x0 #xd2 
        #x0 #xd4 #x0 #xd6 #x0 #xd8 #x0 #xda 
        #x80 #xdc #x80 #xde #x0 #xe0 #x0 #xe2 
        #x0 #xe4 #x0 #xe6 #x0 #xe8 #x0 #xea 
        #x0 #xec #x0 #xee #x0 #xf0 #x0 #xf2 
        #x80 #xf4 #x80 #xf6 #x0 #xf8 #x0 #xfa 
        #x0 #xfc #x0 #xfe #x1 #x0 #x1 #x2 
        #x81 #x4 #x1 #x6 #x1 #x8 #x1 #xa 
        #x1 #xc #x1 #xe #x1 #x10 #x1 #x12 
        #x1 #x14 #x1 #x16 #x1 #x18 #x1 #x1a 
        #x1 #x1c #x1 #x1e #x1 #x20 #x1 #x22 
        #x1 #x24 #x1 #x26 #x1 #x28 #x1 #x2a 
        #x1 #x2c #x1 #x2e #x1 #x30 #x1 #x32 
        #x1 #x34 #x1 #x36 #x1 #x38 #x1 #x3a 
        #x1 #x3c #x1 #x3e #x1 #x40 #x1 #x42 
        #x1 #x44 #x1 #x46 #x1 #x48 #x1 #x4a 
        #x1 #x4c #x1 #x4e #x1 #x50 #x1 #x52 
        #x1 #x54 #x1 #x56 #x1 #x58 #x1 #x5a 
        #x1 #x5c #x1 #x5e #x1 #x60 #x1 #x62 
        #x81 #x64 #x1 #x65 #x1 #x67 #x1 #x69 
        #x1 #x6b #x81 #x6d #x81 #x6f #x81 #x71 
        #x81 #x73 #x81 #x75 #x81 #x77 #x81 #x79 
        #x81 #x7b #x81 #x7d #x1 #x7f #x1 #x81 
        #x1 #x83 #x1 #x85 #x1 #x87 #x1 #x89 
        #x1 #x8b #x1 #x8d #x1 #x8f #x1 #x91 
        #x1 #x93 #x1 #x95 #x1 #x97 #x1 #x99 
        #x1 #x9b #x1 #x9d #x1 #x9f #x1 #xa1 
        #x1 #xa3 #x1 #xa5 #x1 #xa7 #x1 #xa9 
        #x1 #xab #x1 #xad #x1 #xaf #x1 #xb1 
        #x1 #xb3 #x1 #xb5 #x1 #xb7 #x1 #xb9 
        #x1 #xbb #x1 #xbd #x1 #xbf #x81 #xc1 
        #x81 #xc3 #x81 #xc5 #x1 #xc7 #x1 #xc9 
        #x1 #xcb #x1 #xcd #x1 #xcf #x1 #xd1 
        #x1 #xd3 #x1 #xd5 #x1 #xd7 #x1 #xd9 
        #x1 #xdb #x1 #xdd #x1 #xdf #x1 #xe1 
        #x1 #xe3 #x1 #xe5 #x1 #xe7 #x1 #xe9 
        #x1 #xeb #x1 #xed #x1 #xef #x1 #xf1 
        #x1 #xf3 #x1 #xf5 #x1 #xf7 #x1 #xf9 
        #x1 #xfb #x1 #xfd #x1 #xff #x2 #x1 
        #x2 #x3 #x2 #x5 #x2 #x7 #x2 #x9 
        #x2 #xb #x2 #xd #x2 #xf #x2 #x11 
        #x2 #x13 #x2 #x15 #x2 #x17 #x2 #x19 
        #x2 #x1b #x2 #x1d #x2 #x1f #x2 #x21 
        #x2 #x23 #x2 #x25 #x2 #x27 #x2 #x29 
        #x2 #x2b #x2 #x2d #x2 #x2f #x2 #x31 
        #x82 #x33 #x82 #x34 #x82 #x35 #x82 #x36 
        #x82 #x37 #x82 #x38 #x82 #x39 #x82 #x3a 
        #x82 #x3b #x82 #x3c #x82 #x3e #x82 #x40 
        #x82 #x42 #x82 #x44 #x82 #x46 #x82 #x48 
        #x82 #x49 #x82 #x4a #x82 #x4b #x82 #x4c 
        #x2 #x4d #x2 #x4e #x2 #x4f #x2 #x50 
        #x2 #x52 #x82 #x53 #x2 #x55 #x82 #x56 
        #x2 #x58 #x2 #x5a #x2 #x5c #x2 #x5d 
        #x2 #x5f #x2 #x61 #x2 #x63 #x2 #x65 
        #x2 #x67 #x2 #x69 #x2 #x6b #x2 #x6d 
        #x2 #x6f #x2 #x71 #x2 #x73 #x2 #x75 
        #x2 #x77 #x2 #x79 #x2 #x7b #x2 #x7d 
        #x2 #x7f #x2 #x81 #x82 #x83 #x82 #x84 
        #x82 #x85 #x2 #x86 #x2 #x88 #x82 #x8a 
        #x82 #x8b #x82 #x8c #x82 #x8d #x82 #x8e 
        #x82 #x8f #x82 #x90 #x82 #x91 #x2 #x92 
        #x2 #x94 #x2 #x96 #x2 #x98 #x2 #x9a 
        #x2 #x9c #x2 #x9e #x2 #xa0 #x2 #xa2 
        #x2 #xa4 #x2 #xa6 #x2 #xa8 #x2 #xaa 
        #x2 #xac #x2 #xae #x2 #xb0 #x2 #xb2 
        #x2 #xb4 #x2 #xb6 #x2 #xb8 #x2 #xba 
        #x2 #xbc #x2 #xbe #x2 #xc0 #x2 #xc2 
        #x2 #xc4 #x2 #xc6 #x2 #xc8 #x2 #xca 
        #x2 #xcc #x2 #xce #x2 #xd0 #x2 #xd2 
        #x2 #xd4 #x2 #xd6 #x2 #xd8 #x2 #xda 
        #x2 #xdc #x2 #xde #x2 #xe0 #x2 #xe2 
        #x2 #xe4 #x2 #xe6 #x2 #xe8 #x2 #xea 
        #x2 #xec #x2 #xee #x2 #xf0 #x2 #xf2 
        #x2 #xf4 #x2 #xf6 #x2 #xf8 #x82 #xfa 
        #x2 #xfc #x2 #xfe #x3 #x0 #x3 #x2 
        #x3 #x4 #x83 #x6 #x83 #x8 #x83 #xa 
        #x83 #xc #x3 #xe #x3 #x10 #x3 #x12 
        #x3 #x14 #x3 #x16 #x3 #x18 #x3 #x1a 
        #x3 #x1c #x3 #x1e #x3 #x20 #x3 #x22 
        #x3 #x24 #x3 #x26 #x3 #x28 #x3 #x2a 
        #x3 #x2c #x3 #x2e #x3 #x30 #x3 #x32 
        #x3 #x34 #x3 #x36 #x3 #x38 #x3 #x3a 
        #x3 #x3c #x3 #x3e #x3 #x40 #x3 #x42 
        #x3 #x44 #x3 #x46 #x3 #x48 #x3 #x4a 
        #x3 #x4c #x3 #x4e #x3 #x50 #x3 #x52 
        #x3 #x54 #x3 #x56 #x3 #x58 #x3 #x5a 
        #x3 #x5c #x3 #x5e #x3 #x60 #x3 #x62 
        #x3 #x64 #x3 #x66 #x3 #x68 #x3 #x6a 
        #x83 #x6c #x83 #x6e #x83 #x70 #x83 #x72 
        #x83 #x74 #x3 #x75 #x3 #x77 #x3 #x79 
        #x3 #x7b #x3 #x7d #x3 #x7f #x3 #x81 
        #x3 #x83 #x3 #x85 #x83 #x87 #x3 #x89 
        #x83 #x8b #x3 #x8d #x3 #x8f #x3 #x91 
        #x3 #x93 #x3 #x95 #x3 #x97 #x3 #x99 
        #x3 #x9b #x83 #x9d #x3 #x9e #x3 #xa0 
        #x3 #xa2 #x3 #xa4 #x3 #xa6 #x3 #xa8 
        #x3 #xaa #x3 #xac #x3 #xae #x3 #xb0 
        #x3 #xb2 #x83 #xb4 #x83 #xb5 #x83 #xb6 
        #x83 #xb7 #x83 #xb8 #x83 #xb9 #x83 #xba 
        #x83 #xbb #x83 #xbc #x83 #xbd #x83 #xbe 
        #x83 #xbf #x83 #xc0 #x83 #xc1 #x83 #xc2 
        #x83 #xc3 #x83 #xc4 #x83 #xc5 #x83 #xc6 
        #x83 #xc7 #x83 #xc8 #x83 #xc9 #x83 #xca 
        #x83 #xcb #x83 #xcc #x83 #xcd #x83 #xce 
        #x83 #xcf #x83 #xd0 #x83 #xd1 #x83 #xd2 
        #x83 #xd3 #x83 #xd4 #x83 #xd5 #x83 #xd6 
        #x83 #xd7 #x83 #xd8 #x83 #xd9 #x83 #xda 
        #x83 #xdb #x83 #xdc #x83 #xdd #x83 #xde 
        #x83 #xdf #x83 #xe0 #x83 #xe1 #x83 #xe2 
        #x83 #xe3 #x83 #xe4 #x83 #xe5 #x83 #xe6 
        #x83 #xe7 #x83 #xe8 #x83 #xe9 #x83 #xea 
        #x83 #xeb #x83 #xec #x83 #xed #x83 #xee 
        #x83 #xef #x83 #xf0 #x83 #xf1 #x83 #xf2 
        #x83 #xf3 #x83 #xf4 #x83 #xf5 #x83 #xf6 
        #x83 #xf7 #x83 #xf8 #x83 #xf9 #x83 #xfa 
        #x83 #xfb #x83 #xfc #x83 #xfd #x83 #xfe 
        #x83 #xff #x84 #x0 #x84 #x1 #x84 #x2 
        #x84 #x3 #x84 #x4 #x84 #x5 #x84 #x6 
        #x84 #x7 #x84 #x8 #x84 #x9 #x84 #xa 
        #x84 #xb #x84 #xc #x84 #xd #x84 #xe 
        #x84 #xf #x84 #x10 #x84 #x11 #x84 #x12 
        #x84 #x13 #x84 #x14 #x84 #x15 #x4 #x16 
        #x4 #x18 #x4 #x1a #x4 #x1c #x4 #x1e 
        #x4 #x20 #x4 #x22 #x4 #x24 #x4 #x26 
        #x4 #x28 #x4 #x2a #x4 #x2c #x4 #x2e 
        #x4 #x30 #x4 #x32 #x4 #x34 #x4 #x36 
        #x4 #x38 #x4 #x3a #x4 #x3c #x4 #x3e 
        #x4 #x40 #x4 #x42 #x4 #x44 #x4 #x46 
        #x4 #x48 #x4 #x4a #x4 #x4c #x4 #x4e 
        #x4 #x50 #x4 #x52 #x4 #x54 #x4 #x56 
        #x4 #x58 #x4 #x5a #x4 #x5c #x4 #x5e 
        #x4 #x60 #x4 #x62 #x4 #x64 #x4 #x66 
        #x4 #x68 #x4 #x6a #x4 #x6c #x4 #x6e 
        #x4 #x70 #x4 #x72 #x4 #x74 #x4 #x76 
        #x4 #x78 #x4 #x7a #x4 #x7c #x4 #x7e 
        #x4 #x80 #x4 #x82 #x4 #x84 #x4 #x86 
        #x4 #x88 #x4 #x8a #x4 #x8c #x4 #x8e 
        #x4 #x90 #x4 #x92 #x4 #x94 #x4 #x96 
        #x4 #x98 #x4 #x9a #x4 #x9c #x4 #x9e 
        #x4 #xa0 #x4 #xa2 #x4 #xa4 #x4 #xa6 
        #x4 #xa8 #x4 #xaa #x4 #xac #x4 #xae 
        #x4 #xb0 #x4 #xb2 #x4 #xb4 #x4 #xb6 
        #x4 #xb8 #x4 #xba #x4 #xbc #x4 #xbe 
        #x4 #xc0 #x4 #xc2 #x4 #xc4 #x4 #xc6 
        #x4 #xc8 #x4 #xca #x4 #xcc #x4 #xce 
        #x4 #xd0 #x4 #xd2 #x4 #xd4 #x4 #xd6 
        #x4 #xd8 #x4 #xda #x4 #xdc #x4 #xde 
        #x4 #xe0 #x4 #xe2 #x4 #xe4 #x4 #xe6 
        #x4 #xe8 #x4 #xea #x4 #xec #x4 #xee 
        #x4 #xf0 #x4 #xf2 #x4 #xf4 #x4 #xf6 
        #x4 #xf8 #x4 #xfa #x4 #xfc #x4 #xfe 
        #x5 #x0 #x5 #x2 #x5 #x4 #x5 #x6 
        #x5 #x8 #x5 #xa #x5 #xc #x5 #xe 
        #x5 #x10 #x5 #x12 #x5 #x14 #x5 #x16 
        #x5 #x18 #x5 #x1a #x5 #x1c #x5 #x1e 
        #x5 #x20 #x5 #x22 #x5 #x24 #x5 #x26 
        #x5 #x28 #x5 #x2a #x5 #x2c #x5 #x2e 
        #x5 #x30 #x5 #x32 #x5 #x34 #x5 #x36 
        #x5 #x38 #x5 #x3a #x5 #x3c #x5 #x3e 
        #x5 #x40 #x5 #x42 #x5 #x44 #x5 #x46 
        #x5 #x48 #x85 #x4a #x5 #x4c #x5 #x4e 
        #x5 #x50 #x5 #x52 #x5 #x54 #x5 #x56 
        #x5 #x58 #x5 #x5a #x5 #x5c #x5 #x5e 
        #x5 #x60 #x5 #x62 #x5 #x64 #x5 #x66 
        #x5 #x68 #x5 #x6a #x5 #x6c #x5 #x6e 
        #x5 #x70 #x5 #x72 #x5 #x74 #x5 #x76 
        #x5 #x78 #x5 #x7a #x5 #x7c #x5 #x7e 
        #x5 #x80 #x5 #x82 #x5 #x84 #x5 #x86 
        #x5 #x88 #x5 #x8a #x5 #x8c #x5 #x8e 
        #x5 #x90 #x5 #x92 #x5 #x94 #x5 #x96 
        #x5 #x98 #x5 #x9a #x5 #x9c #x5 #x9e 
        #x5 #xa0 #x5 #xa2 #x5 #xa4 #x5 #xa6 
        #x5 #xa8 #x5 #xaa #x5 #xac #x5 #xae 
        #x5 #xb0 #x5 #xb2 #x5 #xb4 #x5 #xb6 
        #x5 #xb8 #x5 #xba #x5 #xbc #x5 #xbe 
        #x5 #xc0 #x5 #xc2 #x5 #xc4 #x5 #xc6 
        #x5 #xc8 #x5 #xca #x5 #xcc #x5 #xce 
        #x5 #xd0 #x5 #xd2 #x5 #xd4 #x5 #xd6 
        #x5 #xd8 #x5 #xda #x5 #xdc #x5 #xde 
        #x5 #xe0 #x5 #xe2 #x5 #xe4 #x5 #xe6 
        #x5 #xe8 #x5 #xea #x5 #xec #x5 #xee 
        #x5 #xf0 #x5 #xf2 #x5 #xf4 #x5 #xf6 
        #x5 #xf8 #x5 #xfa #x5 #xfc #x5 #xfe 
        #x6 #x0 #x6 #x2 #x6 #x4 #x6 #x6 
        #x6 #x8 #x6 #xa #x6 #xc #x6 #xe 
        #x6 #x10 #x6 #x12 #x6 #x14 #x6 #x16 
        #x6 #x18 #x6 #x1a #x6 #x1c #x6 #x1e 
        #x6 #x20 #x6 #x22 #x6 #x24 #x6 #x26 
        #x6 #x28 #x6 #x2a #x6 #x2c #x6 #x2e 
        #x6 #x30 #x6 #x32 #x6 #x34 #x6 #x36 
        #x6 #x38 #x6 #x3a #x6 #x3c #x6 #x3e 
        #x6 #x40 #x6 #x42 #x6 #x44 #x6 #x46 
        #x6 #x48 #x6 #x4a #x6 #x4c #x6 #x4e 
        #x6 #x50 #x6 #x52 #x6 #x54 #x6 #x56 
        #x6 #x58 #x6 #x5a #x6 #x5c #x6 #x5e 
        #x6 #x60 #x6 #x62 #x6 #x64 #x6 #x66 
        #x6 #x68 #x6 #x6a #x6 #x6c #x6 #x6e 
        #x6 #x70 #x6 #x72 #x6 #x74 #x6 #x76 
        #x6 #x78 #x6 #x7a #x6 #x7c #x6 #x7e 
        #x6 #x80 #x6 #x82 #x6 #x84 #x6 #x86 
        #x6 #x88 #x6 #x8a #x6 #x8c #x6 #x8e 
        #x6 #x90 #x6 #x92 #x6 #x94 #x6 #x96 
        #x6 #x98 #x6 #x9a #x6 #x9c #x6 #x9e 
        #x6 #xa0 #x6 #xa2 #x6 #xa4 #x6 #xa6 
        #x6 #xa8 #x6 #xaa #x6 #xac #x6 #xae 
        #x6 #xb0 #x6 #xb2 #x6 #xb4 #x6 #xb6 
        #x6 #xb8 #x6 #xba #x6 #xbc #x6 #xbe 
        #x6 #xc0 #x6 #xc2 #x6 #xc4 #x6 #xc6 
        #x6 #xc8 #x6 #xca #x6 #xcc #x6 #xcd 
        #x6 #xcf #x6 #xd0 #x6 #xd2 #x6 #xd3 
        #x6 #xd5 #x6 #xd6 #x6 #xd8 #x6 #xd9 
        #x6 #xdb #x6 #xdc #x6 #xde #x6 #xdf 
        #x6 #xe1 #x6 #xe3 #x6 #xe5 #x6 #xe7 
        #x6 #xe9 #x6 #xeb #x6 #xed #x6 #xef 
        #x6 #xf1 #x6 #xf3 #x6 #xf5 #x6 #xf7 
        #x6 #xf9 #x6 #xfb #x6 #xfd #x6 #xff 
        #x7 #x1 #x7 #x3 #x7 #x5 #x7 #x7 
        #x7 #x9 #x7 #xb #x7 #xd #x7 #xf 
        #x7 #x11 #x7 #x13 #x7 #x15 #x7 #x17 
        #x7 #x19 #x7 #x1b #x7 #x1d #x7 #x1f 
        #x7 #x21 #x7 #x23 #x7 #x25 #x7 #x27 
        #x7 #x29 #x7 #x2b #x7 #x2d #x7 #x2f 
        #x7 #x31 #x7 #x33 #x7 #x35 #x7 #x37 
        #x7 #x39 #x7 #x3b #x7 #x3d #x7 #x3f 
        #x7 #x41 #x7 #x43 #x7 #x45 #x7 #x47 
        #x7 #x49 #x7 #x4b #x7 #x4d #x7 #x4f 
        #x7 #x51 #x7 #x53 #x7 #x54 #x87 #x56 
        #x7 #x58 #x87 #x59 #x87 #x5b #x7 #x5d 
        #x7 #x5f #x7 #x61 #x7 #x63 #x7 #x65 
        #x7 #x67 #x7 #x69 #x7 #x6b #x7 #x6c 
        #x7 #x6e #x7 #x6f #x7 #x71 #x7 #x73 
        #x7 #x75 #x7 #x77 #x7 #x79 #x7 #x7b 
        #x7 #x7d #x7 #x7e #x7 #x80 #x7 #x82 
        #x7 #x84 #x7 #x86 #x7 #x88 #x7 #x89 
        #x7 #x8b #x7 #x8d #x7 #x8f #x7 #x91 
        #x7 #x93 #x7 #x95 #x7 #x96 #x7 #x98 
        #x7 #x9a #x7 #x9c #x7 #x9e #x7 #xa0 
        #x7 #xa2 #x7 #xa4 #x7 #xa5 #x7 #xa7 
        #x7 #xa9 #x7 #xaa #x7 #xab #x7 #xad 
        #x7 #xaf #x7 #xb1 #x7 #xb3 #x7 #xb5 
        #x7 #xb7 #x7 #xb8 #x7 #xba #x7 #xbb 
        #x7 #xbd #x87 #xbe #x7 #xc0 #x7 #xc1 
        #x87 #xc2 #x87 #xc3 #x87 #xc4 #x87 #xc5 
        #x87 #xc6 #x87 #xc7 #x87 #xc8 #x87 #xc9 
        #x87 #xca #x87 #xcb #x87 #xcc #x87 #xce 
        #x87 #xcf #x87 #xd1 #x87 #xd4 #x87 #xd5 
        #x87 #xd7 #x87 #xda #x87 #xdc #x87 #xdf 
        #x87 #xe1 #x87 #xe3 #x87 #xe5 #x87 #xe7 
        #x87 #xe9 #x87 #xed #x87 #xee #x87 #xef 
        #x87 #xf0 #x87 #xf1 #x87 #xf2 #x87 #xf3 
        #x87 #xf4 #x87 #xf5 #x87 #xf6 #x87 #xf7 
        #x87 #xf8 #x87 #xf9 #x87 #xfa #x87 #xfb 
        #x87 #xfc #x87 #xfd #x87 #xfe #x87 #xff 
        #x88 #x0 #x88 #x1 #x88 #x2 #x88 #x3 
        #x88 #x4 #x88 #x5 #x88 #x6 #x88 #x7 
        #x88 #x8 #x88 #x9 #x88 #xa #x88 #xb 
        #x88 #xc #x88 #xd #x88 #xe #x88 #xf 
        #x88 #x10 #x88 #x12 #x88 #x15 #x88 #x18 
        #x88 #x19 #x88 #x1b #x88 #x1e #x88 #x21 
        #x88 #x22 #x88 #x24 #x88 #x25 #x88 #x26 
        #x88 #x27 #x88 #x28 #x88 #x29 #x88 #x2a 
        #x88 #x2b #x88 #x2c #x88 #x2d #x88 #x2e 
        #x88 #x2f #x88 #x31 #x88 #x32 #x88 #x33 
        #x88 #x34 #x88 #x35 #x88 #x36 #x88 #x38 
        #x88 #x3b #x88 #x3d #x8 #x3e #x88 #x3f 
        #x8 #x40 #x8 #x41 #x88 #x42 #x88 #x43 
        #x88 #x44 #x88 #x45 #x88 #x46 #x88 #x47 
        #x88 #x48 #x88 #x49 #x88 #x4a #x88 #x4b 
        #x88 #x4c #x88 #x4d #x88 #x4e #x88 #x51 
        #x88 #x52 #x88 #x53 #x88 #x54 #x88 #x55 
        #x88 #x56 #x88 #x57 #x88 #x58 #x88 #x59 
        #x88 #x5a #x88 #x5b #x88 #x5e #x88 #x61 
        #x88 #x64 #x88 #x67 #x88 #x6a #x88 #x6d 
        #x88 #x70 #x88 #x73 #x88 #x76 #x88 #x79 
        #x88 #x7c #x88 #x7f #x88 #x81 #x88 #x82 
        #x88 #x84 #x88 #x87 #x88 #x89 #x88 #x8a 
        #x88 #x8c #x88 #x8f #x88 #x93 #x88 #x95 
        #x88 #x96 #x88 #x98 #x88 #x9b #x88 #x9c 
        #x88 #x9d #x88 #x9e #x88 #x9f #x88 #xa0 
        #x88 #xa2 #x88 #xa5 #x88 #xa7 #x88 #xa8 
        #x88 #xaa #x88 #xad #x88 #xb1 #x88 #xb3 
        #x88 #xb4 #x88 #xb6 #x88 #xb9 #x88 #xba 
        #x88 #xbb #x88 #xbc #x8 #xbd #x8 #xbf 
        #x8 #xc1 #x8 #xc3 #x8 #xc5 #x8 #xc7 
        #x8 #xc9 #x8 #xcb #x8 #xcd #x8 #xcf 
        #x8 #xd1 #x88 #xd3 #x88 #xd5 #x88 #xd8 
        #x88 #xda #x8 #xdd #x8 #xdf #x8 #xe1 
        #x8 #xe3 #x8 #xe5 #x8 #xe7 #x8 #xe9 
        #x8 #xeb #x8 #xed #x8 #xef #x8 #xf1 
        #x8 #xf3 #x8 #xf5 #x8 #xf7 #x8 #xf9 
        #x8 #xfb #x8 #xfd #x8 #xff #x9 #x1 
        #x9 #x3 #x9 #x5 #x9 #x7 #x9 #x9 
        #x9 #xb #x9 #xd #x9 #xf #x9 #x11 
        #x9 #x13 #x9 #x15 #x9 #x17 #x9 #x19 
        #x9 #x1b #x9 #x1d #x9 #x1f #x9 #x20 
        #x89 #x21 #x89 #x22 #x89 #x23 #x89 #x24 
        #x89 #x25 #x89 #x26 #x89 #x27 #x89 #x28 
        #x89 #x29 #x89 #x2a #x89 #x2c #x89 #x2e 
        #x89 #x30 #x89 #x32 #x89 #x34 #x89 #x36 
        #x89 #x38 #x89 #x3a #x89 #x3c #x89 #x3e 
        #x89 #x40 #x89 #x43 #x89 #x46 #x89 #x49 
        #x89 #x4c #x89 #x4f #x89 #x52 #x89 #x55 
        #x89 #x58 #x89 #x5b #x89 #x5f #x89 #x63 
        #x89 #x67 #x89 #x6b #x89 #x6f #x89 #x73 
        #x89 #x77 #x89 #x7b #x89 #x7f #x89 #x83 
        #x89 #x87 #x89 #x89 #x89 #x8b #x89 #x8d 
        #x89 #x8f #x89 #x91 #x89 #x93 #x89 #x95 
        #x89 #x97 #x89 #x99 #x89 #x9c #x89 #x9f 
        #x89 #xa2 #x89 #xa5 #x89 #xa8 #x89 #xab 
        #x89 #xae #x89 #xb1 #x89 #xb4 #x89 #xb7 
        #x89 #xba #x89 #xbd #x89 #xc0 #x89 #xc3 
        #x89 #xc6 #x89 #xc9 #x89 #xcc #x89 #xcf 
        #x89 #xd2 #x89 #xd5 #x89 #xd8 #x89 #xdb 
        #x89 #xde #x89 #xe1 #x89 #xe4 #x89 #xe7 
        #x89 #xea #x89 #xed #x89 #xf0 #x89 #xf3 
        #x89 #xf6 #x89 #xf9 #x89 #xfc #x89 #xff 
        #x8a #x2 #x8a #x5 #x8a #x8 #x8a #x9 
        #x8a #xa #x8a #xb #x8a #xc #x8a #xd 
        #x8a #xe #x8a #xf #x8a #x10 #x8a #x11 
        #x8a #x12 #x8a #x13 #x8a #x14 #x8a #x15 
        #x8a #x16 #x8a #x17 #x8a #x18 #x8a #x19 
        #x8a #x1a #x8a #x1b #x8a #x1c #x8a #x1d 
        #x8a #x1e #x8a #x1f #x8a #x20 #x8a #x21 
        #x8a #x22 #x8a #x23 #x8a #x24 #x8a #x25 
        #x8a #x26 #x8a #x27 #x8a #x28 #x8a #x29 
        #x8a #x2a #x8a #x2b #x8a #x2c #x8a #x2d 
        #x8a #x2e #x8a #x2f #x8a #x30 #x8a #x31 
        #x8a #x32 #x8a #x33 #x8a #x34 #x8a #x35 
        #x8a #x36 #x8a #x37 #x8a #x38 #x8a #x39 
        #x8a #x3a #x8a #x3b #x8a #x3c #x8a #x3d 
        #x8a #x41 #x8a #x44 #x8a #x46 #xa #x49 
        #x8a #x4b #x8a #x4c #x8a #x4d #x8a #x4e 
        #x8a #x4f #x8a #x50 #x8a #x51 #x8a #x52 
        #x8a #x53 #x8a #x54 #x8a #x55 #x8a #x56 
        #x8a #x57 #x8a #x58 #x8a #x59 #x8a #x5a 
        #x8a #x5b #x8a #x5c #x8a #x5d #x8a #x5e 
        #x8a #x5f #x8a #x60 #x8a #x61 #x8a #x62 
        #x8a #x63 #x8a #x64 #x8a #x65 #x8a #x66 
        #x8a #x67 #x8a #x68 #x8a #x69 #x8a #x6a 
        #x8a #x6b #x8a #x6c #x8a #x6d #x8a #x6e 
        #x8a #x6f #x8a #x70 #x8a #x71 #x8a #x72 
        #x8a #x73 #x8a #x74 #x8a #x75 #x8a #x76 
        #x8a #x77 #x8a #x78 #x8a #x79 #x8a #x7a 
        #x8a #x7b #x8a #x7c #x8a #x7d #x8a #x7e 
        #x8a #x7f #x8a #x80 #x8a #x81 #x8a #x82 
        #x8a #x83 #x8a #x84 #x8a #x85 #x8a #x86 
        #x8a #x87 #x8a #x88 #x8a #x89 #x8a #x8a 
        #x8a #x8b #x8a #x8c #x8a #x8d #x8a #x8e 
        #x8a #x8f #x8a #x90 #x8a #x91 #x8a #x92 
        #x8a #x93 #x8a #x94 #x8a #x95 #x8a #x96 
        #x8a #x97 #x8a #x98 #x8a #x99 #x8a #x9a 
        #x8a #x9b #x8a #x9c #x8a #x9d #x8a #x9e 
        #x8a #x9f #x8a #xa0 #x8a #xa1 #x8a #xa2 
        #x8a #xa3 #x8a #xa4 #x8a #xa5 #x8a #xa6 
        #x8a #xa7 #x8a #xa8 #x8a #xa9 #x8a #xaa 
        #x8a #xab #x8a #xac #x8a #xad #x8a #xae 
        #x8a #xaf #x8a #xb0 #x8a #xb1 #x8a #xb2 
        #x8a #xb3 #x8a #xb4 #x8a #xb5 #x8a #xb6 
        #x8a #xb7 #x8a #xb8 #x8a #xb9 #x8a #xba 
        #x8a #xbb #x8a #xbc #x8a #xbd #x8a #xbe 
        #x8a #xbf #x8a #xc0 #x8a #xc1 #x8a #xc2 
        #x8a #xc3 #x8a #xc4 #x8a #xc5 #x8a #xc6 
        #x8a #xc7 #x8a #xc8 #x8a #xc9 #x8a #xca 
        #x8a #xcb #x8a #xcc #x8a #xcd #x8a #xce 
        #x8a #xcf #x8a #xd0 #x8a #xd1 #x8a #xd2 
        #x8a #xd3 #x8a #xd4 #x8a #xd5 #x8a #xd6 
        #x8a #xd7 #x8a #xd8 #x8a #xd9 #x8a #xda 
        #x8a #xdb #x8a #xdc #x8a #xdd #x8a #xde 
        #x8a #xdf #x8a #xe0 #x8a #xe1 #x8a #xe2 
        #x8a #xe3 #x8a #xe4 #x8a #xe5 #x8a #xe6 
        #x8a #xe7 #x8a #xe8 #x8a #xe9 #x8a #xea 
        #x8a #xeb #x8a #xec #x8a #xed #x8a #xee 
        #x8a #xef #x8a #xf0 #x8a #xf1 #x8a #xf2 
        #x8a #xf3 #x8a #xf4 #x8a #xf5 #x8a #xf6 
        #x8a #xf7 #x8a #xf8 #x8a #xf9 #x8a #xfa 
        #x8a #xfb #x8a #xfc #x8a #xfd #x8a #xfe 
        #x8a #xff #x8b #x0 #x8b #x1 #x8b #x2 
        #x8b #x3 #x8b #x4 #x8b #x5 #x8b #x6 
        #x8b #x7 #x8b #x8 #x8b #x9 #x8b #xa 
        #x8b #xb #x8b #xc #x8b #xd #x8b #xe 
        #x8b #xf #x8b #x10 #x8b #x11 #x8b #x12 
        #x8b #x13 #x8b #x14 #x8b #x15 #x8b #x16 
        #x8b #x17 #x8b #x18 #x8b #x19 #x8b #x1a 
        #x8b #x1b #x8b #x1c #x8b #x1d #x8b #x1e 
        #x8b #x1f #x8b #x20 #x8b #x21 #x8b #x22 
        #x8b #x23 #x8b #x24 #x8b #x25 #x8b #x26 
        #x8b #x27 #x8b #x28 #xb #x29 #xb #x2b 
        #xb #x2d #xb #x2f #xb #x31 #xb #x33 
        #xb #x35 #xb #x37 #xb #x39 #xb #x3b 
        #xb #x3d #xb #x3f #xb #x41 #xb #x43 
        #xb #x45 #xb #x47 #xb #x49 #xb #x4b 
        #xb #x4d #xb #x4f #xb #x51 #xb #x53 
        #xb #x55 #xb #x57 #xb #x59 #xb #x5b 
        #x8b #x5d #x8b #x5f #xb #x61 #x8b #x63 
        #xb #x65 #xb #x67 #xb #x69 #xb #x6b 
        #xb #x6d #xb #x6f #xb #x71 #xb #x73 
        #xb #x75 #xb #x77 #xb #x79 #xb #x7b 
        #xb #x7d #xb #x7f #xb #x81 #xb #x83 
        #xb #x85 #xb #x87 #xb #x89 #xb #x8b 
        #xb #x8d #xb #x8f #xb #x91 #xb #x93 
        #xb #x95 #xb #x97 #xb #x99 #xb #x9b 
        #xb #x9d #xb #x9f #xb #xa1 #x8b #xa3 
        #x8b #xa5 #x8b #xa6 #x8b #xa7 #x8b #xa8 
        #x8b #xa9 #x8b #xaa #x8b #xab #x8b #xac 
        #x8b #xad #x8b #xae #x8b #xaf #x8b #xb0 
        #x8b #xb1 #x8b #xb2 #x8b #xb3 #x8b #xb4 
        #x8b #xb5 #x8b #xb6 #x8b #xb7 #x8b #xb8 
        #x8b #xb9 #x8b #xba #x8b #xbb #x8b #xbc 
        #x8b #xbd #x8b #xbe #x8b #xbf #x8b #xc0 
        #x8b #xc1 #x8b #xc2 #x8b #xc3 #x8b #xc4 
        #x8b #xc5 #x8b #xc6 #x8b #xc7 #x8b #xc8 
        #x8b #xc9 #x8b #xca #x8b #xcb #x8b #xcc 
        #x8b #xcd #x8b #xce #x8b #xcf #x8b #xd0 
        #x8b #xd1 #x8b #xd2 #x8b #xd3 #x8b #xd4 
        #x8b #xd5 #x8b #xd6 #x8b #xd7 #x8b #xd8 
        #x8b #xd9 #x8b #xda #x8b #xdb #x8b #xdc 
        #x8b #xdd #x8b #xde #x8b #xdf #x8b #xe0 
        #x8b #xe1 #x8b #xe2 #x8b #xe3 #x8b #xe4 
        #x8b #xe5 #x8b #xe6 #x8b #xe7 #x8b #xe8 
        #x8b #xe9 #x8b #xea #x8b #xeb #x8b #xec 
        #x8b #xed #x8b #xee #x8b #xef #x8b #xf0 
        #x8b #xf1 #x8b #xf2 #x8b #xf3 #x8b #xf4 
        #x8b #xf5 #x8b #xf6 #x8b #xf7 #x8b #xf8 
        #x8b #xf9 #x8b #xfa #x8b #xfb #x8b #xfc 
        #x8b #xfd #x8b #xfe #x8b #xff #x8c #x0 
        #x8c #x1 #x8c #x2 #x8c #x3 #x8c #x4 
        #x8c #x5 #x8c #x6 #x8c #x7 #x8c #x8 
        #x8c #x9 #x8c #xa #x8c #xb #x8c #xc 
        #x8c #xd #x8c #xe #x8c #xf #x8c #x10 
        #x8c #x11 #x8c #x14 #x8c #x17 #x8c #x1a 
        #x8c #x1d #x8c #x20 #x8c #x23 #x8c #x26 
        #x8c #x29 #x8c #x2c #x8c #x2f #x8c #x32 
        #x8c #x35 #x8c #x38 #x8c #x3b #x8c #x3f 
        #x8c #x43 #x8c #x47 #x8c #x4b #x8c #x4f 
        #x8c #x53 #x8c #x57 #x8c #x5b #x8c #x5f 
        #x8c #x63 #x8c #x67 #x8c #x6b #x8c #x6f 
        #x8c #x73 #x8c #x77 #x8c #x7e #x8c #x84 
        #x8c #x87 #x8c #x8a #x8c #x8d #x8c #x90 
        #x8c #x93 #x8c #x96 #x8c #x99 #x8c #x9c 
        #x8c #x9f #x8c #xa2 #x8c #xa5 #x8c #xa8 
        #x8c #xab #x8c #xae #x8c #xb1 #x8c #xb4 
        #x8c #xb7 #x8c #xba #x8c #xbd #x8c #xc0 
        #x8c #xc3 #x8c #xc6 #x8c #xc9 #x8c #xcc 
        #x8c #xcf #x8c #xd2 #x8c #xd5 #x8c #xd8 
        #x8c #xdb #x8c #xde #x8c #xe1 #x8c #xe4 
        #x8c #xe7 #x8c #xea #x8c #xed #x8c #xf0 
        #x8c #xf3 #x8c #xf5 #x8c #xf7 #x8c #xf9 
        #x8c #xfb #x8c #xfd #x8c #xff #x8d #x1 
        #x8d #x3 #x8d #x5 #x8d #x7 #x8d #x9 
        #x8d #xb #x8d #xd #x8d #xf #x8d #x11 
        #x8d #x12 #x8d #x13 #x8d #x14 #x8d #x15 
        #x8d #x16 #x8d #x17 #x8d #x18 #x8d #x19 
        #x8d #x1a #x8d #x1b #x8d #x1c #x8d #x1d 
        #x8d #x1e #x8d #x1f #x8d #x21 #x8d #x23 
        #x8d #x25 #x8d #x27 #x8d #x29 #x8d #x2b 
        #x8d #x2d #x8d #x2f #x8d #x31 #x8d #x33 
        #x8d #x35 #x8d #x37 #x8d #x39 #x8d #x3b 
        #x8d #x40 #x8d #x44 #x8d #x46 #x8d #x47 
        #x8d #x48 #x8d #x49 #x8d #x4a #x8d #x4b 
        #x8d #x4c #x8d #x4d #x8d #x4e #x8d #x4f 
        #x8d #x50 #x8d #x51 #x8d #x52 #x8d #x53 
        #x8d #x54 #x8d #x55 #x8d #x56 #x8d #x57 
        #x8d #x58 #x8d #x59 #x8d #x5a #x8d #x5b 
        #x8d #x5c #x8d #x5d #x8d #x5e #x8d #x5f 
        #x8d #x60 #x8d #x61 #x8d #x62 #x8d #x63 
        #x8d #x64 #x8d #x65 #x8d #x66 #x8d #x67 
        #x8d #x68 #x8d #x69 #x8d #x6a #x8d #x6b 
        #x8d #x6c #x8d #x6d #x8d #x6e #x8d #x6f 
        #x8d #x70 #x8d #x71 #x8d #x72 #x8d #x73 
        #x8d #x74 #x8d #x75 #x8d #x76 #x8d #x77 
        #x8d #x79 #x8d #x7b #x8d #x7d #x8d #x7f 
        #x8d #x81 #x8d #x83 #x8d #x85 #x8d #x87 
        #x8d #x89 #x8d #x8b #x8d #x8d #x8d #x8f 
        #x8d #x91 #x8d #x93 #x8d #x95 #x8d #x97 
        #x8d #x99 #x8d #x9b #x8d #x9d #x8d #x9f 
        #x8d #xa1 #x8d #xa3 #x8d #xa5 #x8d #xa7 
        #x8d #xaa #x8d #xad #x8d #xb0 #x8d #xb2 
        #x8d #xb5 #x8d #xb7 #x8d #xba #x8d #xbb 
        #x8d #xbc #x8d #xbd #x8d #xbe #x8d #xbf 
        #x8d #xc0 #x8d #xc1 #x8d #xc2 #x8d #xc3 
        #x8d #xc4 #x8d #xc5 #x8d #xc6 #x8d #xc7 
        #x8d #xc8 #x8d #xc9 #x8d #xca #x8d #xcb 
        #x8d #xcc #x8d #xcd #x8d #xce #x8d #xcf 
        #x8d #xd0 #x8d #xd1 #x8d #xd2 #x8d #xd3 
        #x8d #xd4 #x8d #xd5 #x8d #xd6 #x8d #xd7 
        #x8d #xd8 #x8d #xd9 #x8d #xda #x8d #xdb 
        #x8d #xdc #x8d #xdd #x8d #xde #x8d #xdf 
        #x8d #xe0 #x8d #xe1 #x8d #xe2 #x8d #xe3 
        #x8d #xe4 #x8d #xe5 #x8d #xe6 #x8d #xe7 
        #x8d #xe8 #x8d #xe9 #x8d #xed #x8d #xf1 
        #x8d #xf5 #x8d #xf8 #x8d #xfc #x8d #xff 
        #x8e #x2 #x8e #x7 #x8e #xb #x8e #xe 
        #x8e #x11 #x8e #x14 #x8e #x18 #x8e #x1c 
        #x8e #x1f #x8e #x22 #x8e #x24 #x8e #x27 
        #x8e #x2b #x8e #x2f #x8e #x31 #x8e #x36 
        #x8e #x3c #x8e #x41 #x8e #x44 #x8e #x49 
        #x8e #x4e #x8e #x52 #x8e #x55 #x8e #x58 
        #x8e #x5b #x8e #x5f #x8e #x64 #x8e #x68 
        #x8e #x6b #x8e #x6e #x8e #x71 #x8e #x73 
        #x8e #x75 #x8e #x77 #x8e #x79 #x8e #x7c 
        #x8e #x7f #x8e #x84 #x8e #x87 #x8e #x8b 
        #x8e #x90 #x8e #x93 #x8e #x95 #x8e #x97 
        #x8e #x9c #x8e #xa0 #x8e #xa5 #x8e #xa8 
        #x8e #xad #x8e #xaf #x8e #xb2 #x8e #xb5 
        #x8e #xb8 #x8e #xbb #x8e #xbe #x8e #xc2 
        #x8e #xc5 #x8e #xc7 #x8e #xca #x8e #xcd 
        #x8e #xd0 #x8e #xd4 #x8e #xd7 #x8e #xda 
        #x8e #xdd #x8e #xe2 #x8e #xe6 #x8e #xe8 
        #x8e #xed #x8e #xef #x8e #xf3 #x8e #xf7 
        #x8e #xfa #x8e #xfd #x8f #x0 #x8f #x4 
        #x8f #x6 #x8f #x9 #x8f #xd #x8f #xf 
        #x8f #x14 #x8f #x17 #x8f #x19 #x8f #x1b 
        #x8f #x1d #x8f #x1f #x8f #x21 #x8f #x23 
        #x8f #x25 #x8f #x27 #x8f #x29 #x8f #x2b 
        #x8f #x2e #x8f #x31 #x8f #x34 #x8f #x37 
        #x8f #x3a #x8f #x3d #x8f #x40 #x8f #x43 
        #x8f #x46 #x8f #x49 #x8f #x4c #x8f #x4f 
        #x8f #x52 #x8f #x55 #x8f #x58 #x8f #x5b 
        #x8f #x5d #x8f #x5f #x8f #x62 #x8f #x64 
        #x8f #x66 #x8f #x68 #x8f #x6b #x8f #x6e 
        #x8f #x70 #x8f #x72 #x8f #x74 #x8f #x76 
        #x8f #x78 #x8f #x7c #x8f #x7e #x8f #x80 
        #x8f #x82 #x8f #x84 #x8f #x86 #x8f #x88 
        #x8f #x8a #x8f #x8c #x8f #x8f #x8f #x93 
        #x8f #x95 #x8f #x97 #x8f #x99 #x8f #x9b 
        #x8f #x9d #x8f #x9f #x8f #xa1 #x8f #xa4 
        #x8f #xa7 #x8f #xaa #x8f #xad #x8f #xaf 
        #x8f #xb1 #x8f #xb3 #x8f #xb5 #x8f #xb7 
        #x8f #xb9 #x8f #xbb #x8f #xbd #x8f #xbf 
        #x8f #xc1 #x8f #xc4 #x8f #xc7 #x8f #xc9 
        #x8f #xcc #x8f #xcf #x8f #xd2 #x8f #xd4 
        #x8f #xd7 #x8f #xda #x8f #xde #x8f #xe0 
        #x8f #xe3 #x8f #xe6 #x8f #xe9 #x8f #xec 
        #x8f #xf1 #x8f #xf7 #x8f #xf9 #x8f #xfb 
        #x8f #xfd #x8f #xff #x90 #x1 #x90 #x3 
        #x90 #x5 #x90 #x7 #x90 #x9 #x90 #xb 
        #x90 #xd #x90 #xf #x90 #x11 #x90 #x13 
        #x90 #x15 #x90 #x17 #x90 #x19 #x90 #x1b 
        #x90 #x1f #x90 #x21 #x90 #x23 #x90 #x25 
        #x90 #x29 #x90 #x2c #x90 #x2e #x90 #x30 
        #x90 #x32 #x90 #x34 #x90 #x36 #x90 #x38 
        #x90 #x3a #x90 #x3c #x90 #x3e #x90 #x40 
        #x90 #x43 #x90 #x45 #x90 #x47 #x90 #x4a 
        #x90 #x4d #x90 #x4f #x90 #x53 #x90 #x56 
        #x90 #x58 #x90 #x5a #x90 #x5c #x90 #x5e 
        #x90 #x61 #x90 #x64 #x90 #x66 #x90 #x68 
        #x90 #x6a #x90 #x6c #x90 #x6e #x90 #x70 
        #x90 #x72 #x90 #x74 #x90 #x76 #x90 #x79 
        #x90 #x7c #x90 #x7f #x90 #x82 #x90 #x85 
        #x90 #x88 #x90 #x8b #x90 #x8e #x90 #x91 
        #x90 #x94 #x90 #x97 #x90 #x9a #x90 #x9d 
        #x90 #xa0 #x90 #xa3 #x90 #xa6 #x90 #xa9 
        #x90 #xac #x90 #xaf #x90 #xb2 #x90 #xb5 
        #x90 #xb8 #x10 #xbb #x10 #xbc #x10 #xbd 
        #x10 #xbe #x10 #xbf #x10 #xc0 #x10 #xc1 
        #x10 #xc2 #x10 #xc3 #x10 #xc4 #x10 #xc5 
        #x10 #xc6 #x10 #xc7 #x10 #xc8 #x10 #xc9 
        #x10 #xca #x10 #xcb #x10 #xcc #x10 #xcd 
        #x10 #xce #x10 #xcf #x10 #xd0 #x10 #xd1 
        #x10 #xd2 #x10 #xd3 #x10 #xd4 #x10 #xd5 
        #x10 #xd6 #x10 #xd7 #x10 #xd8 #x10 #xd9 
        #x10 #xda #x10 #xdb #x10 #xdc #x10 #xdd 
        #x10 #xde #x10 #xdf #x10 #xe0 #x10 #xe1 
        #x10 #xe2 #x10 #xe3 #x10 #xe4 #x10 #xe5 
        #x10 #xe6 #x10 #xe7 #x10 #xe8 #x10 #xe9 
        #x10 #xea #x10 #xeb #x10 #xec #x10 #xed 
        #x10 #xee #x10 #xef #x10 #xf0 #x10 #xf1 
        #x10 #xf2 #x10 #xf3 #x10 #xf4 #x10 #xf5 
        #x10 #xf6 #x10 #xf7 #x10 #xf8 #x10 #xf9 
        #x10 #xfa #x10 #xfb #x10 #xfc #x10 #xfd 
        #x10 #xfe #x10 #xff #x11 #x0 #x11 #x1 
        #x11 #x2 #x11 #x3 #x11 #x4 #x11 #x5 
        #x11 #x6 #x11 #x7 #x11 #x8 #x11 #x9 
        #x11 #xa #x11 #xb #x11 #xc #x11 #xd 
        #x11 #xe #x11 #xf #x11 #x10 #x11 #x11 
        #x11 #x12 #x11 #x13 #x11 #x14 #x11 #x15 
        #x11 #x16 #x11 #x17 #x11 #x18 #x11 #x19 
        #x11 #x1a #x11 #x1b #x11 #x1c #x11 #x1d 
        #x11 #x1e #x11 #x1f #x11 #x20 #x11 #x21 
        #x11 #x22 #x11 #x23 #x11 #x24 #x11 #x25 
        #x11 #x26 #x11 #x27 #x11 #x28 #x11 #x29 
        #x11 #x2a #x11 #x2b #x11 #x2c #x11 #x2d 
        #x11 #x2e #x11 #x2f #x11 #x30 #x11 #x31 
        #x11 #x32 #x11 #x33 #x11 #x34 #x11 #x35 
        #x11 #x36 #x11 #x37 #x11 #x38 #x11 #x39 
        #x11 #x3a #x11 #x3b #x11 #x3c #x11 #x3d 
        #x11 #x3e #x11 #x3f #x11 #x40 #x11 #x41 
        #x11 #x42 #x11 #x43 #x11 #x44 #x11 #x45 
        #x11 #x46 #x11 #x47 #x11 #x48 #x11 #x49 
        #x11 #x4a #x11 #x4b #x11 #x4c #x11 #x4d 
        #x11 #x4e #x11 #x4f #x11 #x50 #x11 #x51 
        #x11 #x52 #x11 #x53 #x11 #x54 #x11 #x55 
        #x11 #x56 #x11 #x57 #x11 #x58 #x11 #x59 
        #x11 #x5a #x11 #x5b #x11 #x5c #x11 #x5d 
        #x11 #x5e #x11 #x5f #x11 #x60 #x11 #x61 
        #x11 #x62 #x11 #x63 #x11 #x64 #x11 #x65 
        #x11 #x66 #x11 #x67 #x11 #x68 #x11 #x69 
        #x11 #x6a #x11 #x6b #x11 #x6c #x11 #x6d 
        #x11 #x6e #x11 #x6f #x11 #x70 #x11 #x71 
        #x11 #x72 #x11 #x73 #x11 #x74 #x11 #x75 
        #x11 #x76 #x11 #x77 #x11 #x78 #x11 #x79 
        #x11 #x7a #x11 #x7b #x11 #x7c #x11 #x7d 
        #x11 #x7e #x11 #x7f #x11 #x80 #x11 #x81 
        #x11 #x82 #x11 #x83 #x11 #x84 #x11 #x85 
        #x11 #x86 #x11 #x87 #x11 #x88 #x11 #x89 
        #x11 #x8a #x11 #x8b #x11 #x8c #x11 #x8d 
        #x11 #x8e #x11 #x8f #x11 #x90 #x11 #x91 
        #x11 #x92 #x11 #x93 #x11 #x94 #x11 #x95 
        #x11 #x96 #x11 #x97 #x11 #x98 #x11 #x99 
        #x11 #x9a #x11 #x9b #x11 #x9c #x11 #x9d 
        #x11 #x9e #x11 #x9f #x11 #xa0 #x11 #xa1 
        #x11 #xa2 #x11 #xa3 #x11 #xa4 #x11 #xa5 
        #x11 #xa6 #x11 #xa7 #x11 #xa8 #x11 #xa9 
        #x11 #xaa #x11 #xab #x11 #xac #x11 #xad 
        #x11 #xae #x11 #xaf #x11 #xb0 #x11 #xb1 
        #x11 #xb2 #x11 #xb3 #x11 #xb4 #x11 #xb5 
        #x11 #xb6 #x11 #xb7 #x11 #xb8 #x11 #xb9 
        #x11 #xba #x11 #xbb #x11 #xbc #x11 #xbd 
        #x11 #xbe #x11 #xbf #x11 #xc0 #x11 #xc1 
        #x11 #xc2 #x11 #xc3 #x11 #xc4 #x11 #xc5 
        #x11 #xc6 #x11 #xc7 #x11 #xc8 #x11 #xc9 
        #x11 #xca #x11 #xcb #x11 #xcc #x11 #xcd 
        #x11 #xce #x11 #xcf #x11 #xd0 #x11 #xd1 
        #x11 #xd2 #x11 #xd3 #x11 #xd4 #x11 #xd5 
        #x11 #xd6 #x11 #xd7 #x11 #xd8 #x11 #xd9 
        #x11 #xda #x11 #xdb #x11 #xdc #x11 #xdd 
        #x11 #xde #x11 #xdf #x11 #xe0 #x11 #xe1 
        #x11 #xe2 #x11 #xe3 #x11 #xe4 #x11 #xe5 
        #x11 #xe6 #x11 #xe7 #x11 #xe8 #x11 #xe9 
        #x11 #xea #x11 #xeb #x11 #xec #x11 #xed 
        #x11 #xee #x11 #xef #x11 #xf0 #x11 #xf1 
        #x11 #xf2 #x11 #xf3 #x11 #xf4 #x11 #xf5 
        #x11 #xf6 #x11 #xf7 #x11 #xf8 #x11 #xf9 
        #x11 #xfa #x11 #xfb #x11 #xfc #x11 #xfd 
        #x11 #xfe #x11 #xff #x12 #x0 #x12 #x1 
        #x12 #x2 #x12 #x3 #x12 #x4 #x12 #x5 
        #x12 #x6 #x12 #x7 #x12 #x8 #x12 #x9 
        #x12 #xa #x12 #xb #x12 #xc #x12 #xd 
        #x12 #xe #x12 #xf #x12 #x10 #x12 #x11 
        #x12 #x12 #x12 #x13 #x12 #x14 #x12 #x15 
        #x12 #x16 #x12 #x17 #x12 #x18 #x12 #x19 
        #x12 #x1a #x12 #x1b #x12 #x1c #x12 #x1d 
        #x12 #x1e #x12 #x1f #x12 #x20 #x12 #x21 
        #x12 #x22 #x12 #x23 #x12 #x24 #x12 #x25 
        #x12 #x26 #x12 #x27 #x12 #x28 #x12 #x29 
        #x12 #x2a #x12 #x2b #x12 #x2c #x12 #x2d 
        #x12 #x2e #x12 #x2f #x12 #x30 #x12 #x31 
        #x12 #x32 #x12 #x33 #x12 #x34 #x12 #x35 
        #x12 #x36 #x12 #x37 #x12 #x38 #x12 #x39 
        #x12 #x3a #x12 #x3b #x12 #x3c #x12 #x3d 
        #x12 #x3e #x12 #x3f #x12 #x40 #x12 #x41 
        #x12 #x42 #x12 #x43 #x12 #x44 #x12 #x45 
        #x12 #x46 #x12 #x47 #x12 #x48 #x12 #x49 
        #x12 #x4a #x12 #x4b #x12 #x4c #x12 #x4d 
        #x12 #x4e #x12 #x4f #x12 #x50 #x12 #x51 
        #x12 #x52 #x12 #x53 #x12 #x54 #x12 #x55 
        #x12 #x56 #x12 #x57 #x12 #x58 #x12 #x59 
        #x12 #x5a #x12 #x5b #x12 #x5c #x12 #x5d 
        #x12 #x5e #x12 #x5f #x12 #x60 #x12 #x61 
        #x12 #x62 #x12 #x63 #x12 #x64 #x12 #x65 
        #x12 #x66 #x12 #x67 #x12 #x68 #x12 #x69 
        #x12 #x6a #x12 #x6b #x12 #x6c #x12 #x6d 
        #x12 #x6e #x12 #x6f #x12 #x70 #x12 #x71 
        #x12 #x72 #x12 #x73 #x12 #x74 #x12 #x75 
        #x12 #x76 #x12 #x77 #x12 #x78 #x12 #x79 
        #x12 #x7a #x12 #x7b #x12 #x7c #x12 #x7d 
        #x12 #x7e #x12 #x7f #x12 #x80 #x12 #x81 
        #x92 #x82 #x92 #x84 #x92 #x86 #x92 #x88 
        #x92 #x8b #x92 #x8e #x92 #x90 #x92 #x92 
        #x92 #x94 #x92 #x96 #x92 #x98 #x92 #x9a 
        #x12 #x9c #x12 #x9e #x92 #xa0 #x92 #xa1 
        #x92 #xa2 #x92 #xa3 #x92 #xa4 #x92 #xa5 
        #x92 #xa6 #x92 #xa7 #x92 #xa8 #x92 #xa9 
        #x12 #xaa #x12 #xac #x12 #xae #x12 #xb0 
        #x12 #xb2 #x12 #xb4 #x12 #xb6 #x12 #xb8 
        #x12 #xba #x12 #xbc #x12 #xbe #x12 #xc0 
        #x12 #xc2 #x12 #xc4 #x12 #xc6 #x12 #xc8 
        #x12 #xca #x12 #xcc #x12 #xce #x12 #xd0 
        #x12 #xd2 #x12 #xd4 #x12 #xd6 #x12 #xd8 
        #x12 #xda #x12 #xdc #x12 #xde #x12 #xe0 
        #x12 #xe2 #x12 #xe4 #x12 #xe6 #x12 #xe8 
        #x92 #xea #x92 #xec #x92 #xed #x92 #xee 
        #x92 #xef #x92 #xf0 #x92 #xf1 #x92 #xf2 
        #x92 #xf3 #x92 #xf4 #x92 #xf5 #x92 #xf6 
        #x92 #xf7 #x92 #xf8 #x92 #xf9 #x92 #xfa 
        #x92 #xfb #x92 #xfc #x92 #xfd #x92 #xfe 
        #x92 #xff #x93 #x0 #x93 #x1 #x93 #x2 
        #x93 #x3 #x93 #x4 #x93 #x5 #x93 #x6 
        #x93 #x7 #x93 #x8 #x93 #x9 #x93 #xa 
        #x93 #xb #x93 #xc #x93 #xd #x93 #xe 
        #x93 #xf #x93 #x10 #x93 #x11 #x93 #x12 
        #x93 #x13 #x93 #x14 #x93 #x15 #x93 #x16 
        #x93 #x17 #x93 #x18 #x93 #x19 #x93 #x1a 
        #x93 #x1b #x93 #x1c #x93 #x1d #x93 #x1e 
        #x93 #x1f #x93 #x20 #x93 #x21 #x93 #x22 
        #x93 #x23 #x93 #x24 #x93 #x25 #x93 #x26 
        #x93 #x27 #x93 #x28 #x93 #x29 #x93 #x2a 
        #x93 #x2b #x93 #x2c #x93 #x2d #x93 #x2e 
        #x93 #x2f #x93 #x30 #x93 #x31 #x93 #x32 
        #x93 #x33 #x93 #x34 #x93 #x35 #x93 #x36 
        #x93 #x37 #x93 #x38 #x93 #x39 #x93 #x3a 
        #x93 #x3b #x93 #x3c #x93 #x3d #x93 #x3e 
        #x93 #x3f #x93 #x40 #x93 #x41 #x93 #x42 
        #x93 #x43 #x93 #x44 #x93 #x45 #x93 #x46 
        #x93 #x47 #x93 #x48 #x93 #x49 #x93 #x4a 
        #x93 #x4b #x93 #x4c #x93 #x4d #x93 #x4e 
        #x93 #x4f #x93 #x50 #x93 #x51 #x93 #x52 
        #x93 #x53 #x93 #x54 #x93 #x55 #x93 #x56 
        #x93 #x57 #x93 #x58 #x93 #x59 #x93 #x5a 
        #x93 #x5b #x93 #x5c #x93 #x5d #x93 #x5e 
        #x93 #x5f #x93 #x60 #x93 #x61 #x93 #x62 
        #x93 #x63 #x93 #x64 #x93 #x65 #x93 #x67 
        #x93 #x69 #x93 #x6b #x93 #x6d #x93 #x6f 
        #x93 #x71 #x93 #x73 #x93 #x75 #x93 #x77 
        #x93 #x79 #x93 #x7b #x93 #x7d #x93 #x7f 
        #x93 #x81 #x93 #x83 #x93 #x85 #x93 #x87 
        #x93 #x89 #x93 #x8a #x93 #x8b #x93 #x8c 
        #x93 #x8d #x93 #x8f #x93 #x91 #x93 #x93 
        #x93 #x95 #x93 #x97 #x93 #x99 #x93 #x9b 
        #x93 #x9d #x93 #x9f #x93 #xa1 #x93 #xa3 
        #x93 #xa5 #x93 #xa7 #x93 #xa9 #x93 #xab 
        #x93 #xad #x93 #xaf #x93 #xb1 #x93 #xb3 
        #x93 #xb5 #x93 #xb7 #x93 #xb9 #x93 #xbb 
        #x93 #xbd #x93 #xbf #x93 #xc1 #x93 #xc3 
        #x93 #xc5 #x93 #xc7 #x93 #xc9 #x93 #xcb 
        #x93 #xcd #x93 #xcf #x93 #xd1 #x93 #xd3 
        #x93 #xd5 #x93 #xd7 #x93 #xd9 #x93 #xdb 
        #x93 #xdd #x93 #xdf #x93 #xe1 #x93 #xe3 
        #x93 #xe5 #x93 #xe7 #x93 #xe9 #x93 #xeb 
        #x93 #xed #x93 #xef #x93 #xf1 #x93 #xf3 
        #x93 #xf5 #x93 #xf7 #x93 #xf9 #x93 #xfb 
        #x93 #xfd #x93 #xff #x94 #x1 #x94 #x3 
        #x94 #x5 #x94 #x7 #x94 #x9 #x94 #xb 
        #x94 #xd #x94 #xf #x94 #x11 #x94 #x13 
        #x94 #x15 #x94 #x17 #x94 #x19 #x94 #x1b 
        #x94 #x1d #x94 #x1f #x94 #x21 #x94 #x23 
        #x94 #x25 #x94 #x27 #x94 #x29 #x94 #x2b 
        #x94 #x2d #x94 #x2f #x94 #x31 #x94 #x33 
        #x94 #x35 #x94 #x37 #x94 #x39 #x94 #x3b 
        #x94 #x3d #x94 #x3f #x94 #x41 #x94 #x43 
        #x94 #x45 #x94 #x47 #x94 #x49 #x94 #x4c 
        #x94 #x4f #x94 #x52 #x94 #x55 #x94 #x58 
        #x94 #x5b #x94 #x5d #x94 #x5f #x94 #x61 
        #x94 #x63 #x94 #x65 #x94 #x67 #x94 #x69 
        #x94 #x6b #x94 #x6d #x94 #x6f #x94 #x71 
        #x94 #x73 #x94 #x75 #x94 #x77 #x94 #x79 
        #x94 #x7b #x94 #x7d #x94 #x7f #x94 #x81 
        #x94 #x83 #x94 #x85 #x94 #x87 #x94 #x89 
        #x94 #x8b #x94 #x8d #x94 #x8f #x94 #x91 
        #x94 #x93 #x94 #x95 #x94 #x97 #x94 #x99 
        #x94 #x9b #x94 #x9d #x94 #x9f #x94 #xa1 
        #x94 #xa3 #x94 #xa5 #x94 #xa7 #x94 #xa9 
        #x94 #xab #x94 #xad #x94 #xaf #x94 #xb1 
        #x94 #xb3 #x94 #xb5 #x94 #xb7 #x94 #xb9 
        #x94 #xbb #x94 #xbd #x94 #xbf #x94 #xc1 
        #x94 #xc3 #x94 #xc5 #x94 #xc7 #x94 #xc9 
        #x94 #xcb #x94 #xcd #x94 #xcf #x94 #xd1 
        #x94 #xd3 #x94 #xd5 #x94 #xd7 #x94 #xd9 
        #x94 #xdb #x94 #xdd #x94 #xdf #x94 #xe1 
        #x94 #xe3 #x94 #xe5 #x94 #xe7 #x94 #xe9 
        #x94 #xeb #x94 #xed #x94 #xef #x94 #xf1 
        #x94 #xf3 #x94 #xf5 #x94 #xf7 #x94 #xf9 
        #x94 #xfb #x94 #xfd #x94 #xff #x95 #x1 
        #x95 #x3 #x95 #x5 #x95 #x7 #x95 #x9 
        #x95 #xb #x95 #xd #x95 #xf #x95 #x11 
        #x95 #x13 #x95 #x15 #x95 #x17 #x95 #x19 
        #x95 #x1b #x95 #x1d #x95 #x1f #x95 #x21 
        #x95 #x23 #x95 #x25 #x95 #x27 #x95 #x29 
        #x95 #x2b #x95 #x2d #x95 #x2f #x95 #x31 
        #x95 #x33 #x95 #x35 #x95 #x37 #x95 #x39 
        #x95 #x3b #x95 #x3d #x95 #x3f #x95 #x41 
        #x95 #x43 #x95 #x45 #x95 #x47 #x95 #x49 
        #x95 #x4b #x95 #x4d #x95 #x4f #x95 #x51 
        #x95 #x53 #x95 #x55 #x95 #x57 #x95 #x59 
        #x95 #x5b #x95 #x5d #x95 #x5f #x95 #x61 
        #x95 #x63 #x95 #x65 #x95 #x67 #x95 #x69 
        #x95 #x6b #x95 #x6d #x95 #x6f #x95 #x71 
        #x95 #x73 #x95 #x75 #x95 #x77 #x95 #x7a 
        #x95 #x7d #x95 #x80 #x95 #x82 #x95 #x84 
        #x95 #x86 #x95 #x88 #x95 #x8a #x95 #x8c 
        #x95 #x8e #x95 #x90 #x95 #x92 #x95 #x94 
        #x95 #x96 #x95 #x98 #x95 #x9a #x95 #x9c 
        #x95 #x9e #x95 #xa0 #x95 #xa2 #x95 #xa4 
        #x95 #xa6 #x95 #xa8 #x95 #xaa #x95 #xac 
        #x95 #xae #x95 #xb0 #x95 #xb2 #x95 #xb4 
        #x95 #xb6 #x95 #xb8 #x95 #xba #x95 #xbc 
        #x95 #xbe #x95 #xc0 #x95 #xc2 #x95 #xc4 
        #x95 #xc6 #x95 #xc8 #x95 #xca #x95 #xcc 
        #x95 #xce #x95 #xd0 #x95 #xd2 #x95 #xd4 
        #x95 #xd6 #x95 #xd8 #x95 #xda #x95 #xdc 
        #x95 #xde #x95 #xe0 #x95 #xe2 #x95 #xe4 
        #x95 #xe6 #x95 #xe8 #x95 #xea #x95 #xec 
        #x95 #xee #x95 #xf0 #x95 #xf2 #x95 #xf4 
        #x95 #xf6 #x95 #xf8 #x95 #xfa #x95 #xfc 
        #x95 #xfe #x96 #x0 #x96 #x2 #x96 #x4 
        #x96 #x6 #x96 #x8 #x96 #xa #x96 #xc 
        #x96 #xe #x96 #x10 #x96 #x12 #x96 #x15 
        #x96 #x18 #x96 #x1b #x96 #x1e #x96 #x21 
        #x96 #x24 #x96 #x27 #x96 #x2a #x96 #x2d 
        #x96 #x30 #x96 #x33 #x96 #x36 #x96 #x39 
        #x96 #x3c #x96 #x3f #x96 #x42 #x96 #x45 
        #x96 #x48 #x96 #x4b #x96 #x4e #x96 #x51 
        #x96 #x54 #x96 #x57 #x96 #x5a #x96 #x5d 
        #x96 #x60 #x96 #x63 #x96 #x66 #x96 #x69 
        #x96 #x6c #x96 #x6f #x96 #x72 #x96 #x75 
        #x96 #x78 #x96 #x7b #x96 #x7e #x96 #x81 
        #x96 #x84 #x96 #x87 #x96 #x8a #x96 #x8d 
        #x96 #x90 #x96 #x93 #x96 #x96 #x96 #x99 
        #x96 #x9c #x96 #x9f #x96 #xa2 #x96 #xa5 
        #x96 #xa8 #x96 #xab #x96 #xae #x96 #xb1 
        #x96 #xb4 #x96 #xb7 #x96 #xba #x96 #xbd 
        #x96 #xc0 #x96 #xc3 #x96 #xc6 #x96 #xc9 
        #x96 #xcc #x96 #xcf #x96 #xd2 #x96 #xd5 
        #x96 #xd8 #x96 #xdb #x96 #xde #x96 #xe1 
        #x96 #xe4 #x96 #xe7 #x96 #xea #x96 #xed 
        #x96 #xf0 #x96 #xf3 #x96 #xf6 #x96 #xf9 
        #x96 #xfc #x96 #xff #x97 #x2 #x97 #x5 
        #x97 #x8 #x97 #xb #x97 #xe #x97 #x11 
        #x97 #x14 #x97 #x17 #x97 #x1a #x97 #x1d 
        #x97 #x20 #x97 #x23 #x97 #x26 #x97 #x29 
        #x97 #x2c #x97 #x2f #x97 #x32 #x97 #x35 
        #x97 #x38 #x97 #x3b #x97 #x3e #x97 #x41 
        #x97 #x44 #x97 #x47 #x97 #x4a #x97 #x4d 
        #x97 #x50 #x97 #x53 #x97 #x56 #x97 #x59 
        #x97 #x5c #x97 #x5f #x97 #x62 #x97 #x65 
        #x97 #x68 #x97 #x6b #x97 #x6e #x97 #x71 
        #x97 #x74 #x97 #x77 #x97 #x7a #x97 #x7e 
        #x97 #x82 #x97 #x86 #x97 #x8a #x97 #x8e 
        #x97 #x92 #x97 #x96 #x97 #x99 #x97 #xab 
        #x97 #xb3 #x97 #xb7 #x97 #xb8 #x97 #xb9 
        #x97 #xba #x97 #xbb #x97 #xbc #x97 #xbd 
        #x97 #xbe #x97 #xbf #x97 #xc0 #x97 #xc1 
        #x97 #xc2 #x97 #xc3 #x97 #xc4 #x97 #xc5 
        #x97 #xc6 #x97 #xc7 #x97 #xc8 #x97 #xc9 
        #x97 #xca #x97 #xcb #x97 #xcc #x97 #xcd 
        #x97 #xce #x97 #xcf #x97 #xd0 #x97 #xd1 
        #x97 #xd2 #x97 #xd3 #x97 #xd4 #x97 #xd5 
        #x97 #xd6 #x97 #xd7 #x97 #xd8 #x97 #xd9 
        #x97 #xda #x97 #xdb #x97 #xdc #x97 #xdd 
        #x97 #xde #x97 #xdf #x97 #xe0 #x97 #xe1 
        #x97 #xe2 #x97 #xe3 #x97 #xe4 #x97 #xe5 
        #x97 #xe6 #x97 #xe7 #x97 #xe8 #x97 #xe9 
        #x97 #xea #x97 #xeb #x97 #xec #x97 #xed 
        #x97 #xee #x97 #xef #x97 #xf0 #x97 #xf1 
        #x97 #xf2 #x97 #xf3 #x97 #xf4 #x97 #xf5 
        #x97 #xf6 #x97 #xf7 #x97 #xf8 #x97 #xf9 
        #x97 #xfb #x97 #xfd #x97 #xff #x98 #x1 
        #x98 #x3 #x98 #x5 #x98 #x7 #x98 #x9 
        #x98 #xb #x98 #xd #x98 #xf #x98 #x11 
        #x98 #x13 #x98 #x15 #x98 #x16 #x98 #x17 
        #x98 #x18 #x98 #x19 #x98 #x1a #x98 #x1b 
        #x98 #x1c #x98 #x1d #x98 #x1e #x98 #x1f 
        #x98 #x20 #x98 #x21 #x98 #x22 #x98 #x23 
        #x98 #x24 #x98 #x25 #x98 #x26 #x98 #x27 
        #x98 #x28 #x98 #x29 #x98 #x2a #x98 #x2b 
        #x98 #x2c #x98 #x2d #x98 #x2e #x98 #x2f 
        #x98 #x30 #x98 #x31 #x98 #x32 #x98 #x33 
        #x98 #x34 #x98 #x35 #x98 #x36 #x98 #x37 
        #x98 #x38 #x98 #x39 #x98 #x3a #x98 #x3b 
        #x98 #x3c #x98 #x3d #x98 #x3e #x98 #x3f 
        #x98 #x40 #x98 #x41 #x98 #x42 #x98 #x43 
        #x98 #x44 #x98 #x45 #x98 #x46 #x98 #x47 
        #x98 #x48 #x98 #x49 #x98 #x4a #x98 #x4b 
        #x98 #x4c #x98 #x4d #x98 #x4e #x98 #x4f 
        #x98 #x50 #x98 #x51 #x98 #x52 #x98 #x53 
        #x98 #x54 #x98 #x55 #x98 #x56 #x98 #x57 
        #x98 #x58 #x98 #x59 #x98 #x5a #x98 #x5b 
        #x98 #x5c #x98 #x5d #x98 #x5e #x98 #x5f 
        #x98 #x60 #x98 #x61 #x98 #x62 #x98 #x63 
        #x98 #x64 #x98 #x65 #x98 #x66 #x98 #x67 
        #x98 #x68 #x98 #x69 #x98 #x6a #x98 #x6b 
        #x98 #x6c #x98 #x6d #x98 #x6e #x98 #x6f 
        #x98 #x70 #x98 #x71 #x98 #x72 #x98 #x73 
        #x98 #x74 #x98 #x75 #x98 #x76 #x98 #x77 
        #x98 #x78 #x98 #x79 #x98 #x7a #x98 #x7b 
        #x98 #x7c #x98 #x7d #x98 #x7e #x98 #x7f 
        #x98 #x80 #x98 #x81 #x98 #x82 #x98 #x83 
        #x98 #x84 #x98 #x85 #x98 #x86 #x98 #x87 
        #x98 #x88 #x98 #x89 #x98 #x8a #x98 #x8c 
        #x98 #x8e #x98 #x90 #x98 #x92 #x98 #x94 
        #x98 #x96 #x98 #x98 #x98 #x9a #x98 #x9b 
        #x98 #x9c #x98 #x9d #x98 #x9e #x98 #x9f 
        #x98 #xa0 #x98 #xa1 #x98 #xa2 #x98 #xa3 
        #x98 #xa4 #x98 #xa5 #x98 #xa6 #x98 #xa7 
        #x98 #xa8 #x98 #xa9 #x98 #xaa #x98 #xab 
        #x98 #xac #x98 #xad #x98 #xae #x98 #xaf 
        #x98 #xb0 #x98 #xb1 #x98 #xb2 #x98 #xb3 
        #x98 #xb4 #x98 #xb5 #x98 #xb6 #x98 #xb7 
        #x98 #xb8 #x98 #xb9 #x98 #xba #x98 #xbb 
        #x98 #xbc #x98 #xbd #x98 #xbe #x98 #xbf 
        #x98 #xc0 #x98 #xc1 #x98 #xc2 #x98 #xc3 
        #x98 #xc4 #x98 #xc5 #x98 #xc6 #x98 #xc7 
        #x98 #xc8 #x98 #xc9 #x98 #xca #x98 #xcb 
        #x98 #xcc #x98 #xcd #x98 #xce #x98 #xcf 
        #x98 #xd0 #x98 #xd1 #x98 #xd2 #x98 #xd3 
        #x98 #xd4 #x98 #xd5 #x98 #xd6 #x98 #xd7 
        #x98 #xd8 #x98 #xd9 #x98 #xda #x98 #xdb 
        #x98 #xdc #x98 #xdd #x98 #xde #x98 #xdf 
        #x98 #xe0 #x98 #xe1 #x98 #xe2 #x98 #xe3 
        #x98 #xe4 #x98 #xe5 #x98 #xe6 #x98 #xe7 
        #x98 #xe8 #x98 #xe9 #x98 #xea #x98 #xeb 
        #x98 #xec #x98 #xed #x98 #xee #x98 #xef 
        #x98 #xf0 #x98 #xf1 #x98 #xf2 #x98 #xf3 
        #x98 #xf4 #x98 #xf5 #x98 #xf6 #x98 #xf7 
        #x98 #xf8 #x98 #xf9 #x98 #xfa #x98 #xfb 
        #x98 #xfc #x98 #xfd #x98 #xfe #x98 #xff 
        #x99 #x0 #x99 #x1 #x99 #x2 #x99 #x3 
        #x99 #x4 #x99 #x5 #x99 #x6 #x99 #x7 
        #x99 #x8 #x99 #x9 #x99 #xa #x99 #xb 
        #x99 #xc #x99 #xd #x99 #xe #x99 #xf 
        #x99 #x10 #x99 #x11 #x99 #x12 #x99 #x13 
        #x99 #x14 #x99 #x15 #x99 #x16 #x99 #x17 
        #x99 #x18 #x99 #x19 #x99 #x1a #x99 #x1b 
        #x99 #x1c #x99 #x1d #x99 #x1e #x99 #x1f 
        #x99 #x20 #x99 #x21 #x99 #x22 #x99 #x23 
        #x99 #x24 #x99 #x25 #x99 #x26 #x99 #x27 
        #x99 #x28 #x99 #x29 #x99 #x2a #x99 #x2b 
        #x99 #x2c #x99 #x2d #x99 #x2e #x99 #x2f 
        #x99 #x30 #x99 #x31 #x99 #x32 #x99 #x33 
        #x99 #x34 #x99 #x35 #x99 #x36 #x99 #x37 
        #x99 #x38 #x99 #x39 #x99 #x3a #x99 #x3b 
        #x99 #x3c #x99 #x3d #x99 #x3e #x99 #x3f 
        #x99 #x40 #x99 #x41 #x99 #x42 #x99 #x43 
        #x99 #x44 #x99 #x45 #x99 #x46 #x99 #x47 
        #x99 #x48 #x99 #x49 #x99 #x4a #x99 #x4b 
        #x99 #x4c #x99 #x4d #x99 #x4e #x99 #x4f 
        #x99 #x50 #x99 #x51 #x99 #x52 #x99 #x53 
        #x99 #x54 #x99 #x55 #x99 #x56 #x99 #x57 
        #x99 #x58 #x99 #x59 #x99 #x5a #x99 #x5b 
        #x99 #x5c #x99 #x5d #x99 #x5e #x99 #x5f 
        #x99 #x60 #x99 #x61 #x99 #x62 #x99 #x63 
        #x99 #x64 #x99 #x65 #x99 #x66 #x99 #x67 
        #x99 #x68 #x99 #x69 #x99 #x6a #x99 #x6b 
        #x99 #x6c #x99 #x6d #x99 #x6e #x99 #x6f 
        #x99 #x70 #x99 #x71 #x99 #x72 #x99 #x73 
        #x99 #x74 #x99 #x75 #x99 #x76 #x99 #x77 
        #x99 #x78 #x99 #x79 #x99 #x7a #x19 #x7b 
        #x19 #x7d #x19 #x7f #x19 #x81 #x19 #x83 
        #x19 #x85 #x19 #x87 #x19 #x89 #x19 #x8b 
        #x19 #x8d #x19 #x8f #x19 #x91 #x19 #x93 
        #x99 #x95 #x99 #x96 #x99 #x97 #x99 #x98 
        #x99 #x99 #x99 #x9a #x99 #x9b #x99 #x9c 
        #x99 #x9d #x99 #x9e #x99 #x9f #x99 #xa0 
        #x99 #xa1 #x99 #xa2 #x99 #xa3 #x99 #xa4 
        #x99 #xa5 #x99 #xa6 #x99 #xa7 #x99 #xa8 
        #x99 #xa9 #x99 #xaa #x99 #xab #x99 #xac 
        #x99 #xad #x99 #xae #x99 #xaf #x99 #xb0 
        #x99 #xb1 #x99 #xb2 #x99 #xb3 #x99 #xb4 
        #x99 #xb5 #x99 #xb6 #x99 #xb7 #x99 #xb8 
        #x99 #xb9 #x99 #xba #x99 #xbb #x99 #xbc 
        #x99 #xbd #x99 #xbe #x99 #xbf #x99 #xc0 
        #x99 #xc1 #x99 #xc2 #x99 #xc3 #x99 #xc4 
        #x99 #xc5 #x99 #xc6 #x99 #xc7 #x99 #xc8 
        #x99 #xc9 #x99 #xca #x99 #xcb #x99 #xcc 
        #x99 #xcd #x99 #xce #x99 #xcf #x99 #xd0 
        #x99 #xd1 #x99 #xd2 #x99 #xd3 #x99 #xd4 
        #x99 #xd5 #x99 #xd6 #x99 #xd7 #x99 #xd8 
        #x99 #xd9 #x99 #xda #x99 #xdb #x99 #xdc 
        #x99 #xdd #x99 #xde #x99 #xdf #x99 #xe0 
        #x99 #xe1 #x99 #xe2 #x99 #xe3 #x99 #xe4 
        #x99 #xe5 #x99 #xe6 #x99 #xe7 #x99 #xe8 
        #x99 #xe9 #x99 #xea #x99 #xeb #x99 #xec 
        #x99 #xed #x99 #xee #x99 #xef #x99 #xf0 
        #x99 #xf1 #x99 #xf2 #x99 #xf3 #x99 #xf4 
        #x99 #xf5 #x99 #xf6 #x99 #xf7 #x99 #xf8 
        #x99 #xf9 #x99 #xfa #x99 #xfb #x99 #xfc 
        #x99 #xfd #x99 #xfe #x99 #xff #x9a #x0 
        #x9a #x1 #x9a #x2 #x9a #x3 #x9a #x4 
        #x9a #x5 #x9a #x6 #x9a #x7 #x9a #x8 
        #x9a #x9 #x9a #xa #x9a #xb #x9a #xc 
        #x9a #xd #x9a #xe #x9a #xf #x9a #x10 
        #x9a #x11 #x9a #x12 #x9a #x13 #x9a #x14 
        #x9a #x15 #x9a #x16 #x9a #x17 #x9a #x18 
        #x9a #x19 #x9a #x1a #x9a #x1b #x9a #x1c 
        #x9a #x1d #x9a #x1e #x9a #x1f #x9a #x20 
        #x9a #x21 #x9a #x22 #x9a #x23 #x9a #x24 
        #x9a #x25 #x9a #x26 #x9a #x27 #x9a #x28 
        #x9a #x29 #x9a #x2a #x9a #x2b #x9a #x2c 
        #x9a #x2d #x9a #x2e #x9a #x2f #x9a #x30 
        #x9a #x31 #x9a #x32 #x9a #x33 #x9a #x34 
        #x9a #x35 #x9a #x36 #x9a #x37 #x9a #x38 
        #x9a #x39 #x9a #x3a #x9a #x3b #x9a #x3c 
        #x9a #x3d #x9a #x3e #x9a #x3f #x9a #x40 
        #x9a #x41 #x9a #x42 #x9a #x43 #x9a #x44 
        #x9a #x45 #x9a #x46 #x9a #x47 #x9a #x48 
        #x9a #x49 #x9a #x4a #x9a #x4b #x9a #x4c 
        #x9a #x4d #x9a #x4e #x9a #x4f #x9a #x50 
        #x9a #x51 #x9a #x52 #x9a #x53 #x9a #x54 
        #x9a #x55 #x9a #x56 #x9a #x57 #x9a #x58 
        #x9a #x59 #x9a #x5a #x9a #x5b #x9a #x5c 
        #x9a #x5d #x9a #x5e #x9a #x5f #x9a #x60 
        #x9a #x61 #x9a #x62 #x9a #x63 #x9a #x64 
        #x9a #x65 #x9a #x66 #x9a #x67 #x9a #x68 
        #x9a #x69 #x9a #x6a #x9a #x6b #x9a #x6c 
        #x9a #x6d #x9a #x6e #x9a #x6f #x9a #x70 
        #x9a #x71 #x9a #x72 #x9a #x73 #x9a #x74 
        #x9a #x75 #x9a #x76 #x9a #x77 #x9a #x78 
        #x9a #x79 #x9a #x7a #x9a #x7b #x9a #x7c 
        #x9a #x7d #x9a #x7e #x9a #x7f #x9a #x80 
        #x9a #x81 #x9a #x82 #x9a #x83 #x9a #x84 
        #x9a #x85 #x9a #x86 #x9a #x87 #x9a #x88 
        #x9a #x89 #x9a #x8a #x9a #x8b #x9a #x8c 
        #x9a #x8d #x9a #x8e #x9a #x8f #x9a #x90 
        #x9a #x91 #x9a #x92 #x9a #x93 #x9a #x94 
        #x9a #x95 #x9a #x96 #x9a #x97 #x9a #x98 
        #x9a #x99 #x9a #x9a #x9a #x9b #x9a #x9c 
        #x9a #x9d #x9a #x9e #x9a #x9f #x9a #xa0 
        #x9a #xa1 #x9a #xa2 #x9a #xa3 #x9a #xa4 
        #x9a #xa5 #x9a #xa6 #x9a #xa7 #x9a #xa8 
        #x9a #xa9 #x9a #xaa #x9a #xab #x9a #xac 
        #x9a #xad #x9a #xae #x9a #xaf #x9a #xb0 
        #x9a #xb1 #x9a #xb2 #x9a #xb3 #x9a #xb4 
        #x9a #xb5 #x9a #xb6 #x9a #xb7 #x9a #xb8 
        #x9a #xb9 #x9a #xba #x9a #xbb #x9a #xbc 
        #x9a #xbd #x9a #xbe #x9a #xbf #x9a #xc0 
        #x9a #xc1 #x9a #xc2 #x9a #xc3 #x9a #xc4 
        #x9a #xc5 #x9a #xc6 #x9a #xc7 #x9a #xc8 
        #x9a #xc9 #x9a #xca #x9a #xcb #x9a #xcc 
        #x9a #xcd #x9a #xce #x9a #xcf #x9a #xd0 
        #x9a #xd1 #x9a #xd2 #x9a #xd3 #x9a #xd4 
        #x9a #xd5 #x9a #xd6 #x9a #xd7 #x9a #xd8 
        #x9a #xd9 #x9a #xda #x9a #xdb #x9a #xdc 
        #x9a #xdd #x9a #xde #x9a #xdf #x9a #xe0 
        #x9a #xe1 #x9a #xe2 #x9a #xe3 #x9a #xe4 
        #x9a #xe5 #x9a #xe6 #x9a #xe7 #x9a #xe8 
        #x9a #xe9 #x9a #xea #x9a #xeb #x9a #xec 
        #x9a #xed #x9a #xee #x9a #xef #x9a #xf0 
        #x9a #xf1 #x9a #xf2 #x9a #xf3 #x9a #xf4 
        #x9a #xf5 #x9a #xf6 #x9a #xf7 #x9a #xf8 
        #x9a #xf9 #x9a #xfa #x9a #xfb #x9a #xfc 
        #x9a #xfd #x9a #xfe #x9a #xff #x9b #x0 
        #x9b #x1 #x9b #x2 #x9b #x3 #x9b #x4 
        #x9b #x5 #x9b #x6 #x9b #x7 #x9b #x8 
        #x9b #x9 #x9b #xa #x9b #xb #x9b #xc 
        #x9b #xd #x9b #xe #x9b #xf #x9b #x10 
        #x9b #x11 #x9b #x12 #x9b #x13 #x9b #x14 
        #x9b #x15 #x9b #x16 #x9b #x17 #x9b #x18 
        #x9b #x19 #x9b #x1a #x9b #x1b #x9b #x1c 
        #x9b #x1d #x9b #x1e #x9b #x1f #x9b #x20 
        #x9b #x21 #x9b #x22 #x9b #x23 #x9b #x24 
        #x9b #x25 #x9b #x26 #x9b #x27 #x9b #x28 
        #x9b #x29 #x9b #x2a #x9b #x2b #x9b #x2c 
        #x9b #x2d #x9b #x2e #x9b #x2f #x9b #x30 
        #x9b #x31 #x9b #x32 #x9b #x33 #x9b #x34 
        #x9b #x35 #x9b #x36 #x9b #x37 #x9b #x38 
        #x9b #x39 #x9b #x3a #x9b #x3b #x9b #x3c 
        #x9b #x3d #x9b #x3e #x9b #x3f #x9b #x40 
        #x9b #x41 #x9b #x42 #x9b #x43 #x9b #x44 
        #x9b #x45 #x9b #x46 #x9b #x47 #x9b #x48 
        #x9b #x49 #x9b #x4a #x9b #x4b #x9b #x4c 
        #x9b #x4d #x9b #x4e #x9b #x4f #x9b #x50 
        #x9b #x51 #x9b #x52 #x9b #x53 #x9b #x54 
        #x9b #x55 #x9b #x56 #x9b #x57 #x9b #x58 
        #x9b #x59 #x9b #x5a #x9b #x5b #x9b #x5c 
        #x9b #x5d #x9b #x5e #x9b #x5f #x9b #x60 
        #x9b #x61 #x9b #x62 #x9b #x63 #x9b #x64 
        #x9b #x65 #x9b #x66 #x9b #x67 #x9b #x68 
        #x9b #x69 #x9b #x6a #x9b #x6b #x9b #x6c 
        #x9b #x6d #x9b #x6e #x9b #x6f #x9b #x70 
        #x9b #x71 #x9b #x72 #x9b #x73 #x9b #x74 
        #x9b #x75 #x9b #x76 #x9b #x77 #x9b #x78 
        #x9b #x79 #x9b #x7a #x9b #x7b #x9b #x7c 
        #x9b #x7d #x9b #x7e #x9b #x7f #x9b #x80 
        #x9b #x81 #x9b #x82 #x9b #x83 #x9b #x84 
        #x9b #x85 #x9b #x86 #x9b #x87 #x9b #x88 
        #x9b #x89 #x9b #x8a #x9b #x8b #x9b #x8c 
        #x9b #x8d #x9b #x8e #x9b #x8f #x9b #x90 
        #x9b #x91 #x9b #x92 #x9b #x93 #x9b #x94 
        #x9b #x95 #x9b #x96 #x9b #x97 #x9b #x98 
        #x9b #x99 #x9b #x9a #x9b #x9b #x9b #x9c 
        #x9b #x9d #x9b #x9e #x9b #x9f #x9b #xa0 
        #x9b #xa1 #x9b #xa2 #x9b #xa3 #x9b #xa4 
        #x9b #xa5 #x9b #xa6 #x9b #xa7 #x9b #xa8 
        #x9b #xa9 #x9b #xaa #x9b #xab #x9b #xac 
        #x9b #xad #x9b #xae #x9b #xaf #x9b #xb0 
        #x9b #xb1 #x9b #xb2 #x9b #xb3 #x9b #xb4 
        #x9b #xb5 #x9b #xb6 #x9b #xb7 #x9b #xb8 
        #x9b #xb9 #x9b #xba #x9b #xbb #x9b #xbc 
        #x9b #xbd #x9b #xbe #x9b #xbf #x9b #xc0 
        #x9b #xc1 #x9b #xc2 #x9b #xc3 #x9b #xc4 
        #x9b #xc5 #x9b #xc6 #x9b #xc7 #x9b #xc8 
        #x9b #xc9 #x9b #xca #x9b #xcb #x9b #xcc 
        #x9b #xcd #x9b #xce #x9b #xcf #x9b #xd0 
        #x9b #xd1 #x9b #xd2 #x9b #xd3 #x9b #xd4 
        #x9b #xd5 #x9b #xd6 #x9b #xd7 #x9b #xd8 
        #x9b #xd9 #x9b #xda #x9b #xdb #x9b #xdc 
        #x9b #xdd #x9b #xde #x9b #xdf #x9b #xe0 
        #x9b #xe1 #x9b #xe2 #x9b #xe3 #x9b #xe4 
        #x9b #xe5 #x9b #xe6 #x9b #xe7 #x9b #xe8 
        #x9b #xe9 #x9b #xea #x9b #xeb #x9b #xec 
        #x9b #xed #x9b #xee #x9b #xef #x9b #xf0 
        #x9b #xf1 #x9b #xf2 #x9b #xf3 #x9b #xf4 
        #x9b #xf5 #x9b #xf6 #x9b #xf7 #x9b #xf8 
        #x9b #xf9 #x9b #xfa #x9b #xfb #x9b #xfc 
        #x9b #xfd #x9b #xfe #x9b #xff #x9c #x0 
        #x9c #x1 #x9c #x2 #x9c #x3 #x9c #x4 
        #x9c #x5 #x9c #x6 #x9c #x7 #x9c #x8 
        #x9c #x9 #x9c #xa #x9c #xb #x9c #xc 
        #x9c #xd #x9c #xe #x9c #xf #x9c #x10 
        #x9c #x11 #x9c #x12 #x9c #x13 #x9c #x14 
        #x9c #x15 #x9c #x16 #x9c #x17 #x9c #x18 
        #x9c #x19 #x9c #x1a #x9c #x1b #x9c #x1c 
        #x9c #x1d #x9c #x1e #x9c #x1f #x9c #x20 
        #x9c #x21 #x9c #x22 #x9c #x23 #x9c #x24 
        #x9c #x25 #x9c #x26 #x9c #x27 #x9c #x28 
        #x9c #x29 #x9c #x2a #x9c #x2b #x9c #x2c 
        #x9c #x2d #x9c #x2e #x9c #x2f #x9c #x30 
        #x9c #x31 #x9c #x32 #x9c #x33 #x9c #x34 
        #x9c #x35 #x9c #x36 #x9c #x37 #x9c #x38 
        #x9c #x39 #x9c #x3a #x9c #x3b #x9c #x3c 
        #x9c #x3d #x9c #x3e #x9c #x3f #x9c #x40 
        #x9c #x41 #x9c #x42 #x9c #x43 #x9c #x44 
        #x9c #x45 #x9c #x46 #x9c #x47 #x9c #x48 
        #x9c #x49 #x9c #x4a #x9c #x4b #x9c #x4c 
        #x9c #x4d #x9c #x4e #x9c #x4f #x9c #x50 
        #x9c #x51 #x9c #x52 #x9c #x53 #x9c #x54 
        #x9c #x55 #x9c #x56 #x9c #x57 #x9c #x58 
        #x9c #x59 #x9c #x5a #x9c #x5b #x9c #x5c 
        #x9c #x5d #x9c #x5e #x9c #x5f #x9c #x60 
        #x9c #x61 #x9c #x62 #x9c #x63 #x9c #x64 
        #x9c #x65 #x9c #x66 #x9c #x67 #x9c #x68 
        #x9c #x69 #x9c #x6a #x9c #x6b #x9c #x6c 
        #x9c #x6d #x9c #x6e #x9c #x6f #x9c #x70 
        #x9c #x71 #x9c #x72 #x9c #x73 #x9c #x74 
        #x9c #x75 #x9c #x76 #x9c #x77 #x9c #x78 
        #x9c #x79 #x9c #x7a #x9c #x7b #x9c #x7c 
        #x9c #x7d #x9c #x7e #x9c #x7f #x9c #x80 
        #x9c #x81 #x9c #x82 #x9c #x83 #x9c #x84 
        #x9c #x85 #x9c #x86 #x9c #x87 #x9c #x88 
        #x9c #x89 #x9c #x8a #x9c #x8b #x9c #x8c 
        #x9c #x8d #x9c #x8e #x9c #x8f #x9c #x90 
        #x9c #x91 #x9c #x92 #x9c #x93 #x9c #x94 
        #x9c #x95 #x9c #x96 #x9c #x97 #x9c #x98 
        #x9c #x99 #x9c #x9a #x9c #x9b #x9c #x9c 
        #x9c #x9d #x9c #x9e #x9c #x9f #x9c #xa0 
        #x9c #xa1 #x9c #xa2 #x9c #xa3 #x9c #xa4 
        #x9c #xa5 #x9c #xa6 #x9c #xa7 #x9c #xa8 
        #x9c #xa9 #x9c #xaa #x9c #xab #x9c #xac 
        #x9c #xad #x9c #xae #x9c #xaf #x9c #xb0 
        #x9c #xb1 #x9c #xb2 #x9c #xb3 #x9c #xb4 
        #x9c #xb5 #x9c #xb6 #x9c #xb7 #x9c #xb8 
        #x9c #xb9 #x9c #xba #x9c #xbb #x9c #xbc 
        #x9c #xbd #x9c #xbe #x9c #xbf #x9c #xc0 
        #x9c #xc1 #x9c #xc2 #x9c #xc3 #x9c #xc4 
        #x9c #xc5 #x9c #xc6 #x9c #xc7 #x9c #xc8 
        #x9c #xc9 #x9c #xca #x9c #xcb #x9c #xcc 
        #x9c #xcd #x9c #xce #x9c #xcf #x9c #xd0 
        #x9c #xd1 #x9c #xd2 #x9c #xd3 #x9c #xd4 
        #x9c #xd5 #x9c #xd6 #x9c #xd7 #x9c #xd8 
        #x9c #xd9 #x9c #xda #x9c #xdb #x9c #xdc 
        #x9c #xdd #x9c #xde #x9c #xdf #x9c #xe0 
        #x9c #xe1 #x9c #xe2 #x9c #xe3 #x9c #xe4 
        #x9c #xe5 #x9c #xe6 #x9c #xe7 #x9c #xe8 
        #x9c #xe9 #x9c #xea #x9c #xeb #x9c #xec 
        #x9c #xed #x9c #xee #x9c #xef #x9c #xf0 
        #x9c #xf1 #x9c #xf2 #x9c #xf3 #x9c #xf4 
        #x9c #xf5 #x9c #xf6 #x9c #xf7 #x9c #xf8 
        #x9c #xf9 #x9c #xfa #x9c #xfb #x9c #xfc 
        #x9c #xfd #x9c #xfe #x9c #xff #x9d #x0 
        #x9d #x1 #x9d #x2 #x9d #x3 #x9d #x4 
        #x9d #x5 #x9d #x6 #x9d #x7 #x9d #x8 
        #x9d #x9 #x9d #xa #x9d #xb #x9d #xc 
        #x9d #xd #x9d #xe #x9d #xf #x9d #x10 
        #x9d #x11 #x9d #x12 #x9d #x13 #x9d #x14 
        #x9d #x15 #x9d #x16 #x9d #x17 #x9d #x18 
        #x9d #x19 #x9d #x1a #x9d #x1b #x9d #x1c 
        #x9d #x1d #x9d #x1e #x9d #x1f #x9d #x20 
        #x9d #x21 #x9d #x22 #x9d #x23 #x9d #x24 
        #x9d #x25 #x9d #x26 #x9d #x27 #x9d #x28 
        #x9d #x29 #x9d #x2a #x9d #x2b #x9d #x2c 
        #x9d #x2d #x9d #x2e #x9d #x2f #x9d #x30 
        #x9d #x31 #x9d #x32 #x9d #x33 #x9d #x34 
        #x9d #x35 #x9d #x36 #x9d #x37 #x9d #x38 
        #x9d #x39 #x9d #x3a #x9d #x3b #x9d #x3c 
        #x9d #x3d #x9d #x3e #x9d #x3f #x9d #x40 
        #x9d #x41 #x9d #x42 #x9d #x43 #x9d #x44 
        #x9d #x45 #x9d #x46 #x9d #x47 #x9d #x48 
        #x9d #x49 #x9d #x4a #x9d #x4b #x9d #x4c 
        #x9d #x4d #x9d #x4e #x9d #x4f #x9d #x50 
        #x9d #x51 #x9d #x52 #x9d #x53 #x9d #x54 
        #x9d #x55 #x9d #x56 #x9d #x57 #x9d #x58 
        #x9d #x59 #x9d #x5a #x9d #x5b #x9d #x5c 
        #x9d #x5d #x9d #x5e #x9d #x5f #x9d #x60 
        #x9d #x61 #x9d #x62 #x9d #x63 #x9d #x64 
        #x9d #x65 #x9d #x66 #x9d #x67 #x9d #x68 
        #x9d #x69 #x9d #x6a #x9d #x6b #x9d #x6c 
        #x9d #x6d #x9d #x6e #x9d #x6f #x9d #x70 
        #x9d #x71 #x9d #x72 #x9d #x73 #x9d #x74 
        #x9d #x75 #x9d #x76 #x9d #x77 #x9d #x78 
        #x1d #x79 #x1d #x7a #x1d #x7b #x1d #x7c 
        #x1d #x7d #x1d #x7e #x1d #x7f #x1d #x80 
        #x1d #x81 #x1d #x82 #x1d #x83 #x1d #x84 
        #x1d #x85 #x1d #x86 #x1d #x87 #x1d #x88 
        #x1d #x89 #x1d #x8a #x1d #x8b #x1d #x8c 
        #x1d #x8d #x1d #x8e #x1d #x8f #x1d #x90 
        #x1d #x91 #x1d #x92 #x1d #x93 #x1d #x94 
        #x1d #x95 #x1d #x96 #x1d #x97 #x1d #x98 
        #x1d #x99 #x1d #x9a #x1d #x9b #x1d #x9c 
        #x1d #x9d #x1d #x9e #x1d #x9f #x1d #xa0 
        #x1d #xa1 #x1d #xa2 #x1d #xa3 #x1d #xa4 
        #x1d #xa5 #x1d #xa6 #x1d #xa7 #x1d #xa8 
        #x1d #xa9 #x1d #xaa #x1d #xab #x1d #xac 
        #x1d #xad #x1d #xae #x1d #xaf #x1d #xb0 
        #x1d #xb1 #x1d #xb2 #x1d #xb3 #x1d #xb4 
        #x1d #xb5 #x1d #xb6 #x1d #xb7 #x1d #xb8 
        #x1d #xb9 #x1d #xba #x1d #xbb #x1d #xbc 
        #x1d #xbd #x1d #xbe #x1d #xbf #x1d #xc0 
        #x1d #xc1 #x1d #xc2 #x1d #xc3 #x1d #xc4 
        #x1d #xc5 #x1d #xc6 #x1d #xc7 #x1d #xc8 
        #x1d #xc9 #x1d #xca #x1d #xcb #x1d #xcc 
        #x1d #xcd #x1d #xce #x1d #xcf #x1d #xd0 
        #x1d #xd1 #x1d #xd2 #x1d #xd3 #x1d #xd4 
        #x1d #xd5 #x1d #xd6 #x1d #xd7 #x1d #xd8 
        #x1d #xd9 #x1d #xda #x1d #xdb #x1d #xdc 
        #x1d #xdd #x1d #xde #x1d #xdf #x1d #xe0 
        #x1d #xe1 #x1d #xe2 #x1d #xe3 #x1d #xe4 
        #x1d #xe5 #x1d #xe6 #x1d #xe7 #x1d #xe8 
        #x1d #xe9 #x1d #xea #x1d #xeb #x1d #xec 
        #x1d #xed #x1d #xee #x1d #xef #x1d #xf0 
        #x1d #xf1 #x1d #xf2 #x1d #xf3 #x1d #xf4 
        #x1d #xf5 #x1d #xf6 #x1d #xf7 #x1d #xf8 
        #x1d #xf9 #x1d #xfa #x1d #xfb #x1d #xfc 
        #x1d #xfd #x1d #xfe #x1d #xff #x1e #x0 
        #x1e #x1 #x1e #x2 #x1e #x3 #x1e #x4 
        #x1e #x5 #x1e #x6 #x1e #x7 #x1e #x8 
        #x1e #x9 #x1e #xa #x1e #xb #x1e #xc 
        #x1e #xd #x1e #xe #x1e #xf #x1e #x10 
        #x1e #x11 #x1e #x12 #x1e #x13 #x1e #x14 
        #x1e #x15 #x1e #x16 #x1e #x17 #x1e #x18 
        #x1e #x19 #x1e #x1a #x1e #x1b #x1e #x1c 
        #x1e #x1d #x1e #x1e #x1e #x1f #x1e #x20 
        #x1e #x21 #x1e #x22 #x1e #x23 #x1e #x24 
        #x1e #x25 #x1e #x26 #x1e #x27 #x1e #x28 
        #x1e #x29 #x1e #x2a #x1e #x2b #x1e #x2c 
        #x1e #x2d #x1e #x2e #x1e #x2f #x1e #x30 
        #x1e #x31 #x1e #x32 #x1e #x33 #x1e #x34 
        #x1e #x35 #x1e #x36 #x1e #x37 #x1e #x38 
        #x1e #x39 #x1e #x3a #x1e #x3b #x1e #x3c 
        #x1e #x3d #x1e #x3e #x1e #x3f #x1e #x40 
        #x1e #x41 #x1e #x42 #x1e #x43 #x1e #x44 
        #x1e #x45 #x1e #x46 #x1e #x47 #x1e #x48 
        #x1e #x49 #x1e #x4a #x1e #x4b #x1e #x4c 
        #x1e #x4d #x1e #x4e #x1e #x4f #x1e #x50 
        #x1e #x51 #x1e #x52 #x1e #x53 #x1e #x54 
        #x1e #x55 #x1e #x56 #x1e #x57 #x1e #x58 
        #x1e #x59 #x1e #x5a #x1e #x5b #x1e #x5c 
        #x1e #x5d #x1e #x5e #x1e #x5f #x1e #x60 
        #x1e #x61 #x1e #x62 #x1e #x63 #x1e #x64 
        #x1e #x65 #x1e #x66 #x1e #x67 #x1e #x68 
        #x1e #x69 #x1e #x6a #x1e #x6b #x1e #x6c 
        #x1e #x6d #x1e #x6e #x1e #x6f #x1e #x70 
        #x1e #x71 #x1e #x72 #x1e #x73 #x1e #x74 
        #x1e #x75 #x1e #x76 #x1e #x77 #x1e #x78 
        #x1e #x79 #x1e #x7a #x1e #x7b #x1e #x7c 
        #x1e #x7d #x1e #x7e #x1e #x7f #x1e #x80 
        #x1e #x81 #x1e #x82 #x1e #x83 #x1e #x84 
        #x1e #x85 #x1e #x86 #x1e #x87 #x1e #x88 
        #x1e #x89 #x1e #x8a #x1e #x8b #x1e #x8c 
        #x1e #x8d #x1e #x8e #x1e #x8f #x1e #x90 
        #x1e #x91 #x1e #x92 #x1e #x93 #x1e #x94 
        #x1e #x95 #x1e #x96 #x1e #x97 #x1e #x98 
        #x1e #x99 #x1e #x9a #x1e #x9b #x1e #x9c 
        #x1e #x9d #x1e #x9e #x1e #x9f #x1e #xa0 
        #x1e #xa1 #x1e #xa2 #x1e #xa3 #x1e #xa4 
        #x1e #xa5 #x1e #xa6 #x1e #xa7 #x1e #xa8 
        #x1e #xa9 #x1e #xaa #x1e #xab #x1e #xac 
        #x1e #xad #x1e #xae #x1e #xaf #x1e #xb0 
        #x1e #xb1 #x1e #xb2 #x1e #xb3 #x1e #xb4 
        #x1e #xb5 #x1e #xb6 #x1e #xb7 #x1e #xb8 
        #x1e #xb9 #x1e #xba #x1e #xbb #x1e #xbc 
        #x1e #xbd #x1e #xbe #x1e #xbf #x1e #xc0 
        #x1e #xc1 #x1e #xc2 #x1e #xc3 #x1e #xc4 
        #x1e #xc5 #x1e #xc6 #x1e #xc7 #x1e #xc8 
        #x1e #xc9 #x1e #xca #x1e #xcb #x1e #xcc 
        #x1e #xcd #x1e #xce #x1e #xcf #x1e #xd0 
        #x1e #xd1 #x1e #xd2 #x1e #xd3 #x1e #xd4 
        #x1e #xd5 #x1e #xd6 #x1e #xd7 #x1e #xd8 
        #x1e #xd9 #x1e #xda #x1e #xdb #x1e #xdc 
        #x1e #xdd #x1e #xde #x1e #xdf #x1e #xe0 
        #x1e #xe1 #x1e #xe2 #x1e #xe3 #x1e #xe4 
        #x1e #xe5 #x1e #xe6 #x1e #xe7 #x1e #xe8 
        #x1e #xe9 #x1e #xea #x1e #xeb #x1e #xec 
        #x1e #xed #x1e #xee #x1e #xef #x1e #xf0 
        #x1e #xf1 #x1e #xf2 #x1e #xf3 #x1e #xf4 
        #x1e #xf5 #x1e #xf6 #x1e #xf7 #x1e #xf8 
        #x1e #xf9 #x1e #xfa #x1e #xfb #x1e #xfc 
        #x1e #xfd #x1e #xfe #x1e #xff #x1f #x0 
        #x1f #x1 #x1f #x2 #x1f #x3 #x1f #x4 
        #x1f #x5 #x1f #x6 #x1f #x7 #x1f #x8 
        #x1f #x9 #x1f #xa #x1f #xb #x1f #xc 
        #x1f #xd #x1f #xe #x1f #xf #x1f #x10 
        #x1f #x11 #x1f #x12 #x1f #x13 #x1f #x14 
        #x1f #x15 #x1f #x16 #x1f #x17 #x1f #x18 
        #x1f #x19 #x1f #x1a #x1f #x1b #x1f #x1c 
        #x1f #x1d #x1f #x1e #x1f #x1f #x1f #x20 
        #x1f #x21 #x1f #x22 #x1f #x23 #x1f #x24 
        #x1f #x25 #x1f #x26 #x1f #x27 #x1f #x28 
        #x1f #x29 #x1f #x2a #x1f #x2b #x1f #x2c 
        #x1f #x2d #x1f #x2e #x1f #x2f #x1f #x30 
        #x1f #x31 #x1f #x32 #x1f #x33 #x1f #x34 
        #x1f #x35 #x1f #x36 #x1f #x37 #x1f #x38 
        #x1f #x39 #x1f #x3a #x1f #x3b #x1f #x3c 
        #x1f #x3d #x1f #x3e #x1f #x3f #x1f #x40 
        #x1f #x41 #x1f #x42 #x1f #x43 #x1f #x44 
        #x1f #x45 #x1f #x46 #x1f #x47 #x1f #x48 
        #x1f #x49 #x1f #x4a #x1f #x4b #x1f #x4c 
        #x1f #x4d #x1f #x4e #x1f #x4f #x1f #x50 
        #x1f #x51 #x1f #x52 #x1f #x53 #x1f #x54 
        #x1f #x55 #x1f #x56 #x1f #x57 #x1f #x58 
        #x1f #x59 #x1f #x5a #x1f #x5b #x1f #x5c 
        #x1f #x5d #x1f #x5e #x1f #x5f #x1f #x60 
        #x1f #x61 #x1f #x62 #x1f #x63 #x1f #x64 
        #x1f #x65 #x1f #x66 #x1f #x67 #x1f #x68 
        #x1f #x69 #x1f #x6a #x1f #x6b #x1f #x6c 
        #x1f #x6d #x1f #x6e #x1f #x6f #x1f #x70 
        #x1f #x71 #x1f #x72 #x1f #x73 #x1f #x74 
        #x1f #x75 #x1f #x76 #x1f #x77 #x1f #x78 
        #x1f #x79 #x1f #x7a #x1f #x7b #x1f #x7c 
        #x1f #x7d #x1f #x7e #x1f #x7f #x1f #x80 
        #x1f #x81 #x1f #x82 #x1f #x83 #x1f #x84 
        #x1f #x85 #x1f #x86 #x1f #x87 #x1f #x88 
        #x1f #x89 #x1f #x8a #x1f #x8b #x1f #x8c 
        #x1f #x8d #x1f #x8e #x1f #x8f #x1f #x90 
        #x1f #x91 #x1f #x92 #x1f #x93 #x1f #x94 
        #x1f #x95 #x1f #x96 #x1f #x97))
)

; This vector contains sequences of code points
; for canonical and compatibility decompositions.
;
; This table contains 8087 elements.

(define decomposition-sequences
  '#(
     #x20 #x20 #x308 #x61 #x20 #x304 #x32 #x33 
     #x20 #x301 #x3bc #x20 #x327 #x31 #x6f #x31 
     #x2044 #x34 #x31 #x2044 #x32 #x33 #x2044 #x34 
     #x41 #x300 #x41 #x301 #x41 #x302 #x41 #x303 
     #x41 #x308 #x41 #x30a #x43 #x327 #x45 #x300 
     #x45 #x301 #x45 #x302 #x45 #x308 #x49 #x300 
     #x49 #x301 #x49 #x302 #x49 #x308 #x4e #x303 
     #x4f #x300 #x4f #x301 #x4f #x302 #x4f #x303 
     #x4f #x308 #x55 #x300 #x55 #x301 #x55 #x302 
     #x55 #x308 #x59 #x301 #x61 #x300 #x61 #x301 
     #x61 #x302 #x61 #x303 #x61 #x308 #x61 #x30a 
     #x63 #x327 #x65 #x300 #x65 #x301 #x65 #x302 
     #x65 #x308 #x69 #x300 #x69 #x301 #x69 #x302 
     #x69 #x308 #x6e #x303 #x6f #x300 #x6f #x301 
     #x6f #x302 #x6f #x303 #x6f #x308 #x75 #x300 
     #x75 #x301 #x75 #x302 #x75 #x308 #x79 #x301 
     #x79 #x308 #x41 #x304 #x61 #x304 #x41 #x306 
     #x61 #x306 #x41 #x328 #x61 #x328 #x43 #x301 
     #x63 #x301 #x43 #x302 #x63 #x302 #x43 #x307 
     #x63 #x307 #x43 #x30c #x63 #x30c #x44 #x30c 
     #x64 #x30c #x45 #x304 #x65 #x304 #x45 #x306 
     #x65 #x306 #x45 #x307 #x65 #x307 #x45 #x328 
     #x65 #x328 #x45 #x30c #x65 #x30c #x47 #x302 
     #x67 #x302 #x47 #x306 #x67 #x306 #x47 #x307 
     #x67 #x307 #x47 #x327 #x67 #x327 #x48 #x302 
     #x68 #x302 #x49 #x303 #x69 #x303 #x49 #x304 
     #x69 #x304 #x49 #x306 #x69 #x306 #x49 #x328 
     #x69 #x328 #x49 #x307 #x49 #x4a #x69 #x6a 
     #x4a #x302 #x6a #x302 #x4b #x327 #x6b #x327 
     #x4c #x301 #x6c #x301 #x4c #x327 #x6c #x327 
     #x4c #x30c #x6c #x30c #x4c #xb7 #x6c #xb7 
     #x4e #x301 #x6e #x301 #x4e #x327 #x6e #x327 
     #x4e #x30c #x6e #x30c #x2bc #x6e #x4f #x304 
     #x6f #x304 #x4f #x306 #x6f #x306 #x4f #x30b 
     #x6f #x30b #x52 #x301 #x72 #x301 #x52 #x327 
     #x72 #x327 #x52 #x30c #x72 #x30c #x53 #x301 
     #x73 #x301 #x53 #x302 #x73 #x302 #x53 #x327 
     #x73 #x327 #x53 #x30c #x73 #x30c #x54 #x327 
     #x74 #x327 #x54 #x30c #x74 #x30c #x55 #x303 
     #x75 #x303 #x55 #x304 #x75 #x304 #x55 #x306 
     #x75 #x306 #x55 #x30a #x75 #x30a #x55 #x30b 
     #x75 #x30b #x55 #x328 #x75 #x328 #x57 #x302 
     #x77 #x302 #x59 #x302 #x79 #x302 #x59 #x308 
     #x5a #x301 #x7a #x301 #x5a #x307 #x7a #x307 
     #x5a #x30c #x7a #x30c #x73 #x4f #x31b #x6f 
     #x31b #x55 #x31b #x75 #x31b #x44 #x17d #x44 
     #x17e #x64 #x17e #x4c #x4a #x4c #x6a #x6c 
     #x6a #x4e #x4a #x4e #x6a #x6e #x6a #x41 
     #x30c #x61 #x30c #x49 #x30c #x69 #x30c #x4f 
     #x30c #x6f #x30c #x55 #x30c #x75 #x30c #xdc 
     #x304 #xfc #x304 #xdc #x301 #xfc #x301 #xdc 
     #x30c #xfc #x30c #xdc #x300 #xfc #x300 #xc4 
     #x304 #xe4 #x304 #x226 #x304 #x227 #x304 #xc6 
     #x304 #xe6 #x304 #x47 #x30c #x67 #x30c #x4b 
     #x30c #x6b #x30c #x4f #x328 #x6f #x328 #x1ea 
     #x304 #x1eb #x304 #x1b7 #x30c #x292 #x30c #x6a 
     #x30c #x44 #x5a #x44 #x7a #x64 #x7a #x47 
     #x301 #x67 #x301 #x4e #x300 #x6e #x300 #xc5 
     #x301 #xe5 #x301 #xc6 #x301 #xe6 #x301 #xd8 
     #x301 #xf8 #x301 #x41 #x30f #x61 #x30f #x41 
     #x311 #x61 #x311 #x45 #x30f #x65 #x30f #x45 
     #x311 #x65 #x311 #x49 #x30f #x69 #x30f #x49 
     #x311 #x69 #x311 #x4f #x30f #x6f #x30f #x4f 
     #x311 #x6f #x311 #x52 #x30f #x72 #x30f #x52 
     #x311 #x72 #x311 #x55 #x30f #x75 #x30f #x55 
     #x311 #x75 #x311 #x53 #x326 #x73 #x326 #x54 
     #x326 #x74 #x326 #x48 #x30c #x68 #x30c #x41 
     #x307 #x61 #x307 #x45 #x327 #x65 #x327 #xd6 
     #x304 #xf6 #x304 #xd5 #x304 #xf5 #x304 #x4f 
     #x307 #x6f #x307 #x22e #x304 #x22f #x304 #x59 
     #x304 #x79 #x304 #x68 #x266 #x6a #x72 #x279 
     #x27b #x281 #x77 #x79 #x20 #x306 #x20 #x307 
     #x20 #x30a #x20 #x328 #x20 #x303 #x20 #x30b 
     #x263 #x6c #x73 #x78 #x295 #x300 #x301 #x313 
     #x308 #x301 #x2b9 #x20 #x345 #x3b #x20 #x301 
     #xa8 #x301 #x391 #x301 #xb7 #x395 #x301 #x397 
     #x301 #x399 #x301 #x39f #x301 #x3a5 #x301 #x3a9 
     #x301 #x3ca #x301 #x399 #x308 #x3a5 #x308 #x3b1 
     #x301 #x3b5 #x301 #x3b7 #x301 #x3b9 #x301 #x3cb 
     #x301 #x3b9 #x308 #x3c5 #x308 #x3bf #x301 #x3c5 
     #x301 #x3c9 #x301 #x3b2 #x3b8 #x3a5 #x3d2 #x301 
     #x3d2 #x308 #x3c6 #x3c0 #x3ba #x3c1 #x3c2 #x398 
     #x3b5 #x3a3 #x415 #x300 #x415 #x308 #x413 #x301 
     #x406 #x308 #x41a #x301 #x418 #x300 #x423 #x306 
     #x418 #x306 #x438 #x306 #x435 #x300 #x435 #x308 
     #x433 #x301 #x456 #x308 #x43a #x301 #x438 #x300 
     #x443 #x306 #x474 #x30f #x475 #x30f #x416 #x306 
     #x436 #x306 #x410 #x306 #x430 #x306 #x410 #x308 
     #x430 #x308 #x415 #x306 #x435 #x306 #x4d8 #x308 
     #x4d9 #x308 #x416 #x308 #x436 #x308 #x417 #x308 
     #x437 #x308 #x418 #x304 #x438 #x304 #x418 #x308 
     #x438 #x308 #x41e #x308 #x43e #x308 #x4e8 #x308 
     #x4e9 #x308 #x42d #x308 #x44d #x308 #x423 #x304 
     #x443 #x304 #x423 #x308 #x443 #x308 #x423 #x30b 
     #x443 #x30b #x427 #x308 #x447 #x308 #x42b #x308 
     #x44b #x308 #x565 #x582 #x627 #x653 #x627 #x654 
     #x648 #x654 #x627 #x655 #x64a #x654 #x627 #x674 
     #x648 #x674 #x6c7 #x674 #x64a #x674 #x6d5 #x654 
     #x6c1 #x654 #x6d2 #x654 #x928 #x93c #x930 #x93c 
     #x933 #x93c #x915 #x93c #x916 #x93c #x917 #x93c 
     #x91c #x93c #x921 #x93c #x922 #x93c #x92b #x93c 
     #x92f #x93c #x9c7 #x9be #x9c7 #x9d7 #x9a1 #x9bc 
     #x9a2 #x9bc #x9af #x9bc #xa32 #xa3c #xa38 #xa3c 
     #xa16 #xa3c #xa17 #xa3c #xa1c #xa3c #xa2b #xa3c 
     #xb47 #xb56 #xb47 #xb3e #xb47 #xb57 #xb21 #xb3c 
     #xb22 #xb3c #xb92 #xbd7 #xbc6 #xbbe #xbc7 #xbbe 
     #xbc6 #xbd7 #xc46 #xc56 #xcbf #xcd5 #xcc6 #xcd5 
     #xcc6 #xcd6 #xcc6 #xcc2 #xcca #xcd5 #xd46 #xd3e 
     #xd47 #xd3e #xd46 #xd57 #xdd9 #xdca #xdd9 #xdcf 
     #xddc #xdca #xdd9 #xddf #xe4d #xe32 #xecd #xeb2 
     #xeab #xe99 #xeab #xea1 #xf0b #xf42 #xfb7 #xf4c 
     #xfb7 #xf51 #xfb7 #xf56 #xfb7 #xf5b #xfb7 #xf40 
     #xfb5 #xf71 #xf72 #xf71 #xf74 #xfb2 #xf80 #xfb2 
     #xf81 #xfb3 #xf80 #xfb3 #xf81 #xf71 #xf80 #xf92 
     #xfb7 #xf9c #xfb7 #xfa1 #xfb7 #xfa6 #xfb7 #xfab 
     #xfb7 #xf90 #xfb5 #x1025 #x102e #x10dc #x1b05 #x1b35 
     #x1b07 #x1b35 #x1b09 #x1b35 #x1b0b #x1b35 #x1b0d #x1b35 
     #x1b11 #x1b35 #x1b3a #x1b35 #x1b3c #x1b35 #x1b3e #x1b35 
     #x1b3f #x1b35 #x1b42 #x1b35 #x41 #xc6 #x42 #x44 
     #x45 #x18e #x47 #x48 #x49 #x4a #x4b #x4c 
     #x4d #x4e #x4f #x222 #x50 #x52 #x54 #x55 
     #x57 #x61 #x250 #x251 #x1d02 #x62 #x64 #x65 
     #x259 #x25b #x25c #x67 #x6b #x6d #x14b #x6f 
     #x254 #x1d16 #x1d17 #x70 #x74 #x75 #x1d1d #x26f 
     #x76 #x1d25 #x3b2 #x3b3 #x3b4 #x3c6 #x3c7 #x69 
     #x72 #x75 #x76 #x3b2 #x3b3 #x3c1 #x3c6 #x3c7 
     #x43d #x252 #x63 #x255 #xf0 #x25c #x66 #x25f 
     #x261 #x265 #x268 #x269 #x26a #x1d7b #x29d #x26d 
     #x1d85 #x29f #x271 #x270 #x272 #x273 #x274 #x275 
     #x278 #x282 #x283 #x1ab #x289 #x28a #x1d1c #x28b 
     #x28c #x7a #x290 #x291 #x292 #x3b8 #x41 #x325 
     #x61 #x325 #x42 #x307 #x62 #x307 #x42 #x323 
     #x62 #x323 #x42 #x331 #x62 #x331 #xc7 #x301 
     #xe7 #x301 #x44 #x307 #x64 #x307 #x44 #x323 
     #x64 #x323 #x44 #x331 #x64 #x331 #x44 #x327 
     #x64 #x327 #x44 #x32d #x64 #x32d #x112 #x300 
     #x113 #x300 #x112 #x301 #x113 #x301 #x45 #x32d 
     #x65 #x32d #x45 #x330 #x65 #x330 #x228 #x306 
     #x229 #x306 #x46 #x307 #x66 #x307 #x47 #x304 
     #x67 #x304 #x48 #x307 #x68 #x307 #x48 #x323 
     #x68 #x323 #x48 #x308 #x68 #x308 #x48 #x327 
     #x68 #x327 #x48 #x32e #x68 #x32e #x49 #x330 
     #x69 #x330 #xcf #x301 #xef #x301 #x4b #x301 
     #x6b #x301 #x4b #x323 #x6b #x323 #x4b #x331 
     #x6b #x331 #x4c #x323 #x6c #x323 #x1e36 #x304 
     #x1e37 #x304 #x4c #x331 #x6c #x331 #x4c #x32d 
     #x6c #x32d #x4d #x301 #x6d #x301 #x4d #x307 
     #x6d #x307 #x4d #x323 #x6d #x323 #x4e #x307 
     #x6e #x307 #x4e #x323 #x6e #x323 #x4e #x331 
     #x6e #x331 #x4e #x32d #x6e #x32d #xd5 #x301 
     #xf5 #x301 #xd5 #x308 #xf5 #x308 #x14c #x300 
     #x14d #x300 #x14c #x301 #x14d #x301 #x50 #x301 
     #x70 #x301 #x50 #x307 #x70 #x307 #x52 #x307 
     #x72 #x307 #x52 #x323 #x72 #x323 #x1e5a #x304 
     #x1e5b #x304 #x52 #x331 #x72 #x331 #x53 #x307 
     #x73 #x307 #x53 #x323 #x73 #x323 #x15a #x307 
     #x15b #x307 #x160 #x307 #x161 #x307 #x1e62 #x307 
     #x1e63 #x307 #x54 #x307 #x74 #x307 #x54 #x323 
     #x74 #x323 #x54 #x331 #x74 #x331 #x54 #x32d 
     #x74 #x32d #x55 #x324 #x75 #x324 #x55 #x330 
     #x75 #x330 #x55 #x32d #x75 #x32d #x168 #x301 
     #x169 #x301 #x16a #x308 #x16b #x308 #x56 #x303 
     #x76 #x303 #x56 #x323 #x76 #x323 #x57 #x300 
     #x77 #x300 #x57 #x301 #x77 #x301 #x57 #x308 
     #x77 #x308 #x57 #x307 #x77 #x307 #x57 #x323 
     #x77 #x323 #x58 #x307 #x78 #x307 #x58 #x308 
     #x78 #x308 #x59 #x307 #x79 #x307 #x5a #x302 
     #x7a #x302 #x5a #x323 #x7a #x323 #x5a #x331 
     #x7a #x331 #x68 #x331 #x74 #x308 #x77 #x30a 
     #x79 #x30a #x61 #x2be #x17f #x307 #x41 #x323 
     #x61 #x323 #x41 #x309 #x61 #x309 #xc2 #x301 
     #xe2 #x301 #xc2 #x300 #xe2 #x300 #xc2 #x309 
     #xe2 #x309 #xc2 #x303 #xe2 #x303 #x1ea0 #x302 
     #x1ea1 #x302 #x102 #x301 #x103 #x301 #x102 #x300 
     #x103 #x300 #x102 #x309 #x103 #x309 #x102 #x303 
     #x103 #x303 #x1ea0 #x306 #x1ea1 #x306 #x45 #x323 
     #x65 #x323 #x45 #x309 #x65 #x309 #x45 #x303 
     #x65 #x303 #xca #x301 #xea #x301 #xca #x300 
     #xea #x300 #xca #x309 #xea #x309 #xca #x303 
     #xea #x303 #x1eb8 #x302 #x1eb9 #x302 #x49 #x309 
     #x69 #x309 #x49 #x323 #x69 #x323 #x4f #x323 
     #x6f #x323 #x4f #x309 #x6f #x309 #xd4 #x301 
     #xf4 #x301 #xd4 #x300 #xf4 #x300 #xd4 #x309 
     #xf4 #x309 #xd4 #x303 #xf4 #x303 #x1ecc #x302 
     #x1ecd #x302 #x1a0 #x301 #x1a1 #x301 #x1a0 #x300 
     #x1a1 #x300 #x1a0 #x309 #x1a1 #x309 #x1a0 #x303 
     #x1a1 #x303 #x1a0 #x323 #x1a1 #x323 #x55 #x323 
     #x75 #x323 #x55 #x309 #x75 #x309 #x1af #x301 
     #x1b0 #x301 #x1af #x300 #x1b0 #x300 #x1af #x309 
     #x1b0 #x309 #x1af #x303 #x1b0 #x303 #x1af #x323 
     #x1b0 #x323 #x59 #x300 #x79 #x300 #x59 #x323 
     #x79 #x323 #x59 #x309 #x79 #x309 #x59 #x303 
     #x79 #x303 #x3b1 #x313 #x3b1 #x314 #x1f00 #x300 
     #x1f01 #x300 #x1f00 #x301 #x1f01 #x301 #x1f00 #x342 
     #x1f01 #x342 #x391 #x313 #x391 #x314 #x1f08 #x300 
     #x1f09 #x300 #x1f08 #x301 #x1f09 #x301 #x1f08 #x342 
     #x1f09 #x342 #x3b5 #x313 #x3b5 #x314 #x1f10 #x300 
     #x1f11 #x300 #x1f10 #x301 #x1f11 #x301 #x395 #x313 
     #x395 #x314 #x1f18 #x300 #x1f19 #x300 #x1f18 #x301 
     #x1f19 #x301 #x3b7 #x313 #x3b7 #x314 #x1f20 #x300 
     #x1f21 #x300 #x1f20 #x301 #x1f21 #x301 #x1f20 #x342 
     #x1f21 #x342 #x397 #x313 #x397 #x314 #x1f28 #x300 
     #x1f29 #x300 #x1f28 #x301 #x1f29 #x301 #x1f28 #x342 
     #x1f29 #x342 #x3b9 #x313 #x3b9 #x314 #x1f30 #x300 
     #x1f31 #x300 #x1f30 #x301 #x1f31 #x301 #x1f30 #x342 
     #x1f31 #x342 #x399 #x313 #x399 #x314 #x1f38 #x300 
     #x1f39 #x300 #x1f38 #x301 #x1f39 #x301 #x1f38 #x342 
     #x1f39 #x342 #x3bf #x313 #x3bf #x314 #x1f40 #x300 
     #x1f41 #x300 #x1f40 #x301 #x1f41 #x301 #x39f #x313 
     #x39f #x314 #x1f48 #x300 #x1f49 #x300 #x1f48 #x301 
     #x1f49 #x301 #x3c5 #x313 #x3c5 #x314 #x1f50 #x300 
     #x1f51 #x300 #x1f50 #x301 #x1f51 #x301 #x1f50 #x342 
     #x1f51 #x342 #x3a5 #x314 #x1f59 #x300 #x1f59 #x301 
     #x1f59 #x342 #x3c9 #x313 #x3c9 #x314 #x1f60 #x300 
     #x1f61 #x300 #x1f60 #x301 #x1f61 #x301 #x1f60 #x342 
     #x1f61 #x342 #x3a9 #x313 #x3a9 #x314 #x1f68 #x300 
     #x1f69 #x300 #x1f68 #x301 #x1f69 #x301 #x1f68 #x342 
     #x1f69 #x342 #x3b1 #x300 #x3ac #x3b5 #x300 #x3ad 
     #x3b7 #x300 #x3ae #x3b9 #x300 #x3af #x3bf #x300 
     #x3cc #x3c5 #x300 #x3cd #x3c9 #x300 #x3ce #x1f00 
     #x345 #x1f01 #x345 #x1f02 #x345 #x1f03 #x345 #x1f04 
     #x345 #x1f05 #x345 #x1f06 #x345 #x1f07 #x345 #x1f08 
     #x345 #x1f09 #x345 #x1f0a #x345 #x1f0b #x345 #x1f0c 
     #x345 #x1f0d #x345 #x1f0e #x345 #x1f0f #x345 #x1f20 
     #x345 #x1f21 #x345 #x1f22 #x345 #x1f23 #x345 #x1f24 
     #x345 #x1f25 #x345 #x1f26 #x345 #x1f27 #x345 #x1f28 
     #x345 #x1f29 #x345 #x1f2a #x345 #x1f2b #x345 #x1f2c 
     #x345 #x1f2d #x345 #x1f2e #x345 #x1f2f #x345 #x1f60 
     #x345 #x1f61 #x345 #x1f62 #x345 #x1f63 #x345 #x1f64 
     #x345 #x1f65 #x345 #x1f66 #x345 #x1f67 #x345 #x1f68 
     #x345 #x1f69 #x345 #x1f6a #x345 #x1f6b #x345 #x1f6c 
     #x345 #x1f6d #x345 #x1f6e #x345 #x1f6f #x345 #x3b1 
     #x306 #x3b1 #x304 #x1f70 #x345 #x3b1 #x345 #x3ac 
     #x345 #x3b1 #x342 #x1fb6 #x345 #x391 #x306 #x391 
     #x304 #x391 #x300 #x386 #x391 #x345 #x20 #x313 
     #x3b9 #x20 #x313 #x20 #x342 #xa8 #x342 #x1f74 
     #x345 #x3b7 #x345 #x3ae #x345 #x3b7 #x342 #x1fc6 
     #x345 #x395 #x300 #x388 #x397 #x300 #x389 #x397 
     #x345 #x1fbf #x300 #x1fbf #x301 #x1fbf #x342 #x3b9 
     #x306 #x3b9 #x304 #x3ca #x300 #x390 #x3b9 #x342 
     #x3ca #x342 #x399 #x306 #x399 #x304 #x399 #x300 
     #x38a #x1ffe #x300 #x1ffe #x301 #x1ffe #x342 #x3c5 
     #x306 #x3c5 #x304 #x3cb #x300 #x3b0 #x3c1 #x313 
     #x3c1 #x314 #x3c5 #x342 #x3cb #x342 #x3a5 #x306 
     #x3a5 #x304 #x3a5 #x300 #x38e #x3a1 #x314 #xa8 
     #x300 #x385 #x60 #x1f7c #x345 #x3c9 #x345 #x3ce 
     #x345 #x3c9 #x342 #x1ff6 #x345 #x39f #x300 #x38c 
     #x3a9 #x300 #x38f #x3a9 #x345 #xb4 #x20 #x314 
     #x2002 #x2003 #x20 #x20 #x20 #x20 #x20 #x20 
     #x20 #x20 #x20 #x2010 #x20 #x333 #x2e #x2e 
     #x2e #x2e #x2e #x2e #x20 #x2032 #x2032 #x2032 
     #x2032 #x2032 #x2035 #x2035 #x2035 #x2035 #x2035 #x21 
     #x21 #x20 #x305 #x3f #x3f #x3f #x21 #x21 
     #x3f #x2032 #x2032 #x2032 #x2032 #x20 #x30 #x69 
     #x34 #x35 #x36 #x37 #x38 #x39 #x2b #x2212 
     #x3d #x28 #x29 #x6e #x30 #x31 #x32 #x33 
     #x34 #x35 #x36 #x37 #x38 #x39 #x2b #x2212 
     #x3d #x28 #x29 #x61 #x65 #x6f #x78 #x259 
     #x52 #x73 #x61 #x2f #x63 #x61 #x2f #x73 
     #x43 #xb0 #x43 #x63 #x2f #x6f #x63 #x2f 
     #x75 #x190 #xb0 #x46 #x67 #x48 #x48 #x48 
     #x68 #x127 #x49 #x49 #x4c #x6c #x4e #x4e 
     #x6f #x50 #x51 #x52 #x52 #x52 #x53 #x4d 
     #x54 #x45 #x4c #x54 #x4d #x5a #x3a9 #x5a 
     #x4b #xc5 #x42 #x43 #x65 #x45 #x46 #x4d 
     #x6f #x5d0 #x5d1 #x5d2 #x5d3 #x69 #x46 #x41 
     #x58 #x3c0 #x3b3 #x393 #x3a0 #x2211 #x44 #x64 
     #x65 #x69 #x6a #x31 #x2044 #x33 #x32 #x2044 
     #x33 #x31 #x2044 #x35 #x32 #x2044 #x35 #x33 
     #x2044 #x35 #x34 #x2044 #x35 #x31 #x2044 #x36 
     #x35 #x2044 #x36 #x31 #x2044 #x38 #x33 #x2044 
     #x38 #x35 #x2044 #x38 #x37 #x2044 #x38 #x31 
     #x2044 #x49 #x49 #x49 #x49 #x49 #x49 #x49 
     #x56 #x56 #x56 #x49 #x56 #x49 #x49 #x56 
     #x49 #x49 #x49 #x49 #x58 #x58 #x58 #x49 
     #x58 #x49 #x49 #x4c #x43 #x44 #x4d #x69 
     #x69 #x69 #x69 #x69 #x69 #x69 #x76 #x76 
     #x76 #x69 #x76 #x69 #x69 #x76 #x69 #x69 
     #x69 #x69 #x78 #x78 #x78 #x69 #x78 #x69 
     #x69 #x6c #x63 #x64 #x6d #x2190 #x338 #x2192 
     #x338 #x2194 #x338 #x21d0 #x338 #x21d4 #x338 #x21d2 
     #x338 #x2203 #x338 #x2208 #x338 #x220b #x338 #x2223 
     #x338 #x2225 #x338 #x222b #x222b #x222b #x222b #x222b 
     #x222e #x222e #x222e #x222e #x222e #x223c #x338 #x2243 
     #x338 #x2245 #x338 #x2248 #x338 #x3d #x338 #x2261 
     #x338 #x224d #x338 #x3c #x338 #x3e #x338 #x2264 
     #x338 #x2265 #x338 #x2272 #x338 #x2273 #x338 #x2276 
     #x338 #x2277 #x338 #x227a #x338 #x227b #x338 #x2282 
     #x338 #x2283 #x338 #x2286 #x338 #x2287 #x338 #x22a2 
     #x338 #x22a8 #x338 #x22a9 #x338 #x22ab #x338 #x227c 
     #x338 #x227d #x338 #x2291 #x338 #x2292 #x338 #x22b2 
     #x338 #x22b3 #x338 #x22b4 #x338 #x22b5 #x338 #x3008 
     #x3009 #x31 #x32 #x33 #x34 #x35 #x36 #x37 
     #x38 #x39 #x31 #x30 #x31 #x31 #x31 #x32 
     #x31 #x33 #x31 #x34 #x31 #x35 #x31 #x36 
     #x31 #x37 #x31 #x38 #x31 #x39 #x32 #x30 
     #x28 #x31 #x29 #x28 #x32 #x29 #x28 #x33 
     #x29 #x28 #x34 #x29 #x28 #x35 #x29 #x28 
     #x36 #x29 #x28 #x37 #x29 #x28 #x38 #x29 
     #x28 #x39 #x29 #x28 #x31 #x30 #x29 #x28 
     #x31 #x31 #x29 #x28 #x31 #x32 #x29 #x28 
     #x31 #x33 #x29 #x28 #x31 #x34 #x29 #x28 
     #x31 #x35 #x29 #x28 #x31 #x36 #x29 #x28 
     #x31 #x37 #x29 #x28 #x31 #x38 #x29 #x28 
     #x31 #x39 #x29 #x28 #x32 #x30 #x29 #x31 
     #x2e #x32 #x2e #x33 #x2e #x34 #x2e #x35 
     #x2e #x36 #x2e #x37 #x2e #x38 #x2e #x39 
     #x2e #x31 #x30 #x2e #x31 #x31 #x2e #x31 
     #x32 #x2e #x31 #x33 #x2e #x31 #x34 #x2e 
     #x31 #x35 #x2e #x31 #x36 #x2e #x31 #x37 
     #x2e #x31 #x38 #x2e #x31 #x39 #x2e #x32 
     #x30 #x2e #x28 #x61 #x29 #x28 #x62 #x29 
     #x28 #x63 #x29 #x28 #x64 #x29 #x28 #x65 
     #x29 #x28 #x66 #x29 #x28 #x67 #x29 #x28 
     #x68 #x29 #x28 #x69 #x29 #x28 #x6a #x29 
     #x28 #x6b #x29 #x28 #x6c #x29 #x28 #x6d 
     #x29 #x28 #x6e #x29 #x28 #x6f #x29 #x28 
     #x70 #x29 #x28 #x71 #x29 #x28 #x72 #x29 
     #x28 #x73 #x29 #x28 #x74 #x29 #x28 #x75 
     #x29 #x28 #x76 #x29 #x28 #x77 #x29 #x28 
     #x78 #x29 #x28 #x79 #x29 #x28 #x7a #x29 
     #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x48 
     #x49 #x4a #x4b #x4c #x4d #x4e #x4f #x50 
     #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 
     #x59 #x5a #x61 #x62 #x63 #x64 #x65 #x66 
     #x67 #x68 #x69 #x6a #x6b #x6c #x6d #x6e 
     #x6f #x70 #x71 #x72 #x73 #x74 #x75 #x76 
     #x77 #x78 #x79 #x7a #x30 #x222b #x222b #x222b 
     #x222b #x3a #x3a #x3d #x3d #x3d #x3d #x3d 
     #x3d #x2add #x338 #x2d61 #x6bcd #x9f9f #x4e00 #x4e28 
     #x4e36 #x4e3f #x4e59 #x4e85 #x4e8c #x4ea0 #x4eba #x513f 
     #x5165 #x516b #x5182 #x5196 #x51ab #x51e0 #x51f5 #x5200 
     #x529b #x52f9 #x5315 #x531a #x5338 #x5341 #x535c #x5369 
     #x5382 #x53b6 #x53c8 #x53e3 #x56d7 #x571f #x58eb #x5902 
     #x590a #x5915 #x5927 #x5973 #x5b50 #x5b80 #x5bf8 #x5c0f 
     #x5c22 #x5c38 #x5c6e #x5c71 #x5ddb #x5de5 #x5df1 #x5dfe 
     #x5e72 #x5e7a #x5e7f #x5ef4 #x5efe #x5f0b #x5f13 #x5f50 
     #x5f61 #x5f73 #x5fc3 #x6208 #x6236 #x624b #x652f #x6534 
     #x6587 #x6597 #x65a4 #x65b9 #x65e0 #x65e5 #x66f0 #x6708 
     #x6728 #x6b20 #x6b62 #x6b79 #x6bb3 #x6bcb #x6bd4 #x6bdb 
     #x6c0f #x6c14 #x6c34 #x706b #x722a #x7236 #x723b #x723f 
     #x7247 #x7259 #x725b #x72ac #x7384 #x7389 #x74dc #x74e6 
     #x7518 #x751f #x7528 #x7530 #x758b #x7592 #x7676 #x767d 
     #x76ae #x76bf #x76ee #x77db #x77e2 #x77f3 #x793a #x79b8 
     #x79be #x7a74 #x7acb #x7af9 #x7c73 #x7cf8 #x7f36 #x7f51 
     #x7f8a #x7fbd #x8001 #x800c #x8012 #x8033 #x807f #x8089 
     #x81e3 #x81ea #x81f3 #x81fc #x820c #x821b #x821f #x826e 
     #x8272 #x8278 #x864d #x866b #x8840 #x884c #x8863 #x897e 
     #x898b #x89d2 #x8a00 #x8c37 #x8c46 #x8c55 #x8c78 #x8c9d 
     #x8d64 #x8d70 #x8db3 #x8eab #x8eca #x8f9b #x8fb0 #x8fb5 
     #x9091 #x9149 #x91c6 #x91cc #x91d1 #x9577 #x9580 #x961c 
     #x96b6 #x96b9 #x96e8 #x9751 #x975e #x9762 #x9769 #x97cb 
     #x97ed #x97f3 #x9801 #x98a8 #x98db #x98df #x9996 #x9999 
     #x99ac #x9aa8 #x9ad8 #x9adf #x9b25 #x9b2f #x9b32 #x9b3c 
     #x9b5a #x9ce5 #x9e75 #x9e7f #x9ea5 #x9ebb #x9ec3 #x9ecd 
     #x9ed1 #x9ef9 #x9efd #x9f0e #x9f13 #x9f20 #x9f3b #x9f4a 
     #x9f52 #x9f8d #x9f9c #x9fa0 #x20 #x3012 #x5341 #x5344 
     #x5345 #x304b #x3099 #x304d #x3099 #x304f #x3099 #x3051 
     #x3099 #x3053 #x3099 #x3055 #x3099 #x3057 #x3099 #x3059 
     #x3099 #x305b #x3099 #x305d #x3099 #x305f #x3099 #x3061 
     #x3099 #x3064 #x3099 #x3066 #x3099 #x3068 #x3099 #x306f 
     #x3099 #x306f #x309a #x3072 #x3099 #x3072 #x309a #x3075 
     #x3099 #x3075 #x309a #x3078 #x3099 #x3078 #x309a #x307b 
     #x3099 #x307b #x309a #x3046 #x3099 #x20 #x3099 #x20 
     #x309a #x309d #x3099 #x3088 #x308a #x30ab #x3099 #x30ad 
     #x3099 #x30af #x3099 #x30b1 #x3099 #x30b3 #x3099 #x30b5 
     #x3099 #x30b7 #x3099 #x30b9 #x3099 #x30bb #x3099 #x30bd 
     #x3099 #x30bf #x3099 #x30c1 #x3099 #x30c4 #x3099 #x30c6 
     #x3099 #x30c8 #x3099 #x30cf #x3099 #x30cf #x309a #x30d2 
     #x3099 #x30d2 #x309a #x30d5 #x3099 #x30d5 #x309a #x30d8 
     #x3099 #x30d8 #x309a #x30db #x3099 #x30db #x309a #x30a6 
     #x3099 #x30ef #x3099 #x30f0 #x3099 #x30f1 #x3099 #x30f2 
     #x3099 #x30fd #x3099 #x30b3 #x30c8 #x1100 #x1101 #x11aa 
     #x1102 #x11ac #x11ad #x1103 #x1104 #x1105 #x11b0 #x11b1 
     #x11b2 #x11b3 #x11b4 #x11b5 #x111a #x1106 #x1107 #x1108 
     #x1121 #x1109 #x110a #x110b #x110c #x110d #x110e #x110f 
     #x1110 #x1111 #x1112 #x1161 #x1162 #x1163 #x1164 #x1165 
     #x1166 #x1167 #x1168 #x1169 #x116a #x116b #x116c #x116d 
     #x116e #x116f #x1170 #x1171 #x1172 #x1173 #x1174 #x1175 
     #x1160 #x1114 #x1115 #x11c7 #x11c8 #x11cc #x11ce #x11d3 
     #x11d7 #x11d9 #x111c #x11dd #x11df #x111d #x111e #x1120 
     #x1122 #x1123 #x1127 #x1129 #x112b #x112c #x112d #x112e 
     #x112f #x1132 #x1136 #x1140 #x1147 #x114c #x11f1 #x11f2 
     #x1157 #x1158 #x1159 #x1184 #x1185 #x1188 #x1191 #x1192 
     #x1194 #x119e #x11a1 #x4e00 #x4e8c #x4e09 #x56db #x4e0a 
     #x4e2d #x4e0b #x7532 #x4e59 #x4e19 #x4e01 #x5929 #x5730 
     #x4eba #x28 #x1100 #x29 #x28 #x1102 #x29 #x28 
     #x1103 #x29 #x28 #x1105 #x29 #x28 #x1106 #x29 
     #x28 #x1107 #x29 #x28 #x1109 #x29 #x28 #x110b 
     #x29 #x28 #x110c #x29 #x28 #x110e #x29 #x28 
     #x110f #x29 #x28 #x1110 #x29 #x28 #x1111 #x29 
     #x28 #x1112 #x29 #x28 #x1100 #x1161 #x29 #x28 
     #x1102 #x1161 #x29 #x28 #x1103 #x1161 #x29 #x28 
     #x1105 #x1161 #x29 #x28 #x1106 #x1161 #x29 #x28 
     #x1107 #x1161 #x29 #x28 #x1109 #x1161 #x29 #x28 
     #x110b #x1161 #x29 #x28 #x110c #x1161 #x29 #x28 
     #x110e #x1161 #x29 #x28 #x110f #x1161 #x29 #x28 
     #x1110 #x1161 #x29 #x28 #x1111 #x1161 #x29 #x28 
     #x1112 #x1161 #x29 #x28 #x110c #x116e #x29 #x28 
     #x110b #x1169 #x110c #x1165 #x11ab #x29 #x28 #x110b 
     #x1169 #x1112 #x116e #x29 #x28 #x4e00 #x29 #x28 
     #x4e8c #x29 #x28 #x4e09 #x29 #x28 #x56db #x29 
     #x28 #x4e94 #x29 #x28 #x516d #x29 #x28 #x4e03 
     #x29 #x28 #x516b #x29 #x28 #x4e5d #x29 #x28 
     #x5341 #x29 #x28 #x6708 #x29 #x28 #x706b #x29 
     #x28 #x6c34 #x29 #x28 #x6728 #x29 #x28 #x91d1 
     #x29 #x28 #x571f #x29 #x28 #x65e5 #x29 #x28 
     #x682a #x29 #x28 #x6709 #x29 #x28 #x793e #x29 
     #x28 #x540d #x29 #x28 #x7279 #x29 #x28 #x8ca1 
     #x29 #x28 #x795d #x29 #x28 #x52b4 #x29 #x28 
     #x4ee3 #x29 #x28 #x547c #x29 #x28 #x5b66 #x29 
     #x28 #x76e3 #x29 #x28 #x4f01 #x29 #x28 #x8cc7 
     #x29 #x28 #x5354 #x29 #x28 #x796d #x29 #x28 
     #x4f11 #x29 #x28 #x81ea #x29 #x28 #x81f3 #x29 
     #x50 #x54 #x45 #x32 #x31 #x32 #x32 #x32 
     #x33 #x32 #x34 #x32 #x35 #x32 #x36 #x32 
     #x37 #x32 #x38 #x32 #x39 #x33 #x30 #x33 
     #x31 #x33 #x32 #x33 #x33 #x33 #x34 #x33 
     #x35 #x1100 #x1102 #x1103 #x1105 #x1106 #x1107 #x1109 
     #x110b #x110c #x110e #x110f #x1110 #x1111 #x1112 #x1100 
     #x1161 #x1102 #x1161 #x1103 #x1161 #x1105 #x1161 #x1106 
     #x1161 #x1107 #x1161 #x1109 #x1161 #x110b #x1161 #x110c 
     #x1161 #x110e #x1161 #x110f #x1161 #x1110 #x1161 #x1111 
     #x1161 #x1112 #x1161 #x110e #x1161 #x11b7 #x1100 #x1169 
     #x110c #x116e #x110b #x1174 #x110b #x116e #x4e00 #x4e8c 
     #x4e09 #x56db #x4e94 #x516d #x4e03 #x516b #x4e5d #x5341 
     #x6708 #x706b #x6c34 #x6728 #x91d1 #x571f #x65e5 #x682a 
     #x6709 #x793e #x540d #x7279 #x8ca1 #x795d #x52b4 #x79d8 
     #x7537 #x5973 #x9069 #x512a #x5370 #x6ce8 #x9805 #x4f11 
     #x5199 #x6b63 #x4e0a #x4e2d #x4e0b #x5de6 #x53f3 #x533b 
     #x5b97 #x5b66 #x76e3 #x4f01 #x8cc7 #x5354 #x591c #x33 
     #x36 #x33 #x37 #x33 #x38 #x33 #x39 #x34 
     #x30 #x34 #x31 #x34 #x32 #x34 #x33 #x34 
     #x34 #x34 #x35 #x34 #x36 #x34 #x37 #x34 
     #x38 #x34 #x39 #x35 #x30 #x31 #x6708 #x32 
     #x6708 #x33 #x6708 #x34 #x6708 #x35 #x6708 #x36 
     #x6708 #x37 #x6708 #x38 #x6708 #x39 #x6708 #x31 
     #x30 #x6708 #x31 #x31 #x6708 #x31 #x32 #x6708 
     #x48 #x67 #x65 #x72 #x67 #x65 #x56 #x4c 
     #x54 #x44 #x30a2 #x30a4 #x30a6 #x30a8 #x30aa #x30ab 
     #x30ad #x30af #x30b1 #x30b3 #x30b5 #x30b7 #x30b9 #x30bb 
     #x30bd #x30bf #x30c1 #x30c4 #x30c6 #x30c8 #x30ca #x30cb 
     #x30cc #x30cd #x30ce #x30cf #x30d2 #x30d5 #x30d8 #x30db 
     #x30de #x30df #x30e0 #x30e1 #x30e2 #x30e4 #x30e6 #x30e8 
     #x30e9 #x30ea #x30eb #x30ec #x30ed #x30ef #x30f0 #x30f1 
     #x30f2 #x30a2 #x30d1 #x30fc #x30c8 #x30a2 #x30eb #x30d5 
     #x30a1 #x30a2 #x30f3 #x30da #x30a2 #x30a2 #x30fc #x30eb 
     #x30a4 #x30cb #x30f3 #x30b0 #x30a4 #x30f3 #x30c1 #x30a6 
     #x30a9 #x30f3 #x30a8 #x30b9 #x30af #x30fc #x30c9 #x30a8 
     #x30fc #x30ab #x30fc #x30aa #x30f3 #x30b9 #x30aa #x30fc 
     #x30e0 #x30ab #x30a4 #x30ea #x30ab #x30e9 #x30c3 #x30c8 
     #x30ab #x30ed #x30ea #x30fc #x30ac #x30ed #x30f3 #x30ac 
     #x30f3 #x30de #x30ae #x30ac #x30ae #x30cb #x30fc #x30ad 
     #x30e5 #x30ea #x30fc #x30ae #x30eb #x30c0 #x30fc #x30ad 
     #x30ed #x30ad #x30ed #x30b0 #x30e9 #x30e0 #x30ad #x30ed 
     #x30e1 #x30fc #x30c8 #x30eb #x30ad #x30ed #x30ef #x30c3 
     #x30c8 #x30b0 #x30e9 #x30e0 #x30b0 #x30e9 #x30e0 #x30c8 
     #x30f3 #x30af #x30eb #x30bc #x30a4 #x30ed #x30af #x30ed 
     #x30fc #x30cd #x30b1 #x30fc #x30b9 #x30b3 #x30eb #x30ca 
     #x30b3 #x30fc #x30dd #x30b5 #x30a4 #x30af #x30eb #x30b5 
     #x30f3 #x30c1 #x30fc #x30e0 #x30b7 #x30ea #x30f3 #x30b0 
     #x30bb #x30f3 #x30c1 #x30bb #x30f3 #x30c8 #x30c0 #x30fc 
     #x30b9 #x30c7 #x30b7 #x30c9 #x30eb #x30c8 #x30f3 #x30ca 
     #x30ce #x30ce #x30c3 #x30c8 #x30cf #x30a4 #x30c4 #x30d1 
     #x30fc #x30bb #x30f3 #x30c8 #x30d1 #x30fc #x30c4 #x30d0 
     #x30fc #x30ec #x30eb #x30d4 #x30a2 #x30b9 #x30c8 #x30eb 
     #x30d4 #x30af #x30eb #x30d4 #x30b3 #x30d3 #x30eb #x30d5 
     #x30a1 #x30e9 #x30c3 #x30c9 #x30d5 #x30a3 #x30fc #x30c8 
     #x30d6 #x30c3 #x30b7 #x30a7 #x30eb #x30d5 #x30e9 #x30f3 
     #x30d8 #x30af #x30bf #x30fc #x30eb #x30da #x30bd #x30da 
     #x30cb #x30d2 #x30d8 #x30eb #x30c4 #x30da #x30f3 #x30b9 
     #x30da #x30fc #x30b8 #x30d9 #x30fc #x30bf #x30dd #x30a4 
     #x30f3 #x30c8 #x30dc #x30eb #x30c8 #x30db #x30f3 #x30dd 
     #x30f3 #x30c9 #x30db #x30fc #x30eb #x30db #x30fc #x30f3 
     #x30de #x30a4 #x30af #x30ed #x30de #x30a4 #x30eb #x30de 
     #x30c3 #x30cf #x30de #x30eb #x30af #x30de #x30f3 #x30b7 
     #x30e7 #x30f3 #x30df #x30af #x30ed #x30f3 #x30df #x30ea 
     #x30df #x30ea #x30d0 #x30fc #x30eb #x30e1 #x30ac #x30e1 
     #x30ac #x30c8 #x30f3 #x30e1 #x30fc #x30c8 #x30eb #x30e4 
     #x30fc #x30c9 #x30e4 #x30fc #x30eb #x30e6 #x30a2 #x30f3 
     #x30ea #x30c3 #x30c8 #x30eb #x30ea #x30e9 #x30eb #x30d4 
     #x30fc #x30eb #x30fc #x30d6 #x30eb #x30ec #x30e0 #x30ec 
     #x30f3 #x30c8 #x30b2 #x30f3 #x30ef #x30c3 #x30c8 #x30 
     #x70b9 #x31 #x70b9 #x32 #x70b9 #x33 #x70b9 #x34 
     #x70b9 #x35 #x70b9 #x36 #x70b9 #x37 #x70b9 #x38 
     #x70b9 #x39 #x70b9 #x31 #x30 #x70b9 #x31 #x31 
     #x70b9 #x31 #x32 #x70b9 #x31 #x33 #x70b9 #x31 
     #x34 #x70b9 #x31 #x35 #x70b9 #x31 #x36 #x70b9 
     #x31 #x37 #x70b9 #x31 #x38 #x70b9 #x31 #x39 
     #x70b9 #x32 #x30 #x70b9 #x32 #x31 #x70b9 #x32 
     #x32 #x70b9 #x32 #x33 #x70b9 #x32 #x34 #x70b9 
     #x68 #x50 #x61 #x64 #x61 #x41 #x55 #x62 
     #x61 #x72 #x6f #x56 #x70 #x63 #x64 #x6d 
     #x64 #x6d #xb2 #x64 #x6d #xb3 #x49 #x55 
     #x5e73 #x6210 #x662d #x548c #x5927 #x6b63 #x660e #x6cbb 
     #x682a #x5f0f #x4f1a #x793e #x70 #x41 #x6e #x41 
     #x3bc #x41 #x6d #x41 #x6b #x41 #x4b #x42 
     #x4d #x42 #x47 #x42 #x63 #x61 #x6c #x6b 
     #x63 #x61 #x6c #x70 #x46 #x6e #x46 #x3bc 
     #x46 #x3bc #x67 #x6d #x67 #x6b #x67 #x48 
     #x7a #x6b #x48 #x7a #x4d #x48 #x7a #x47 
     #x48 #x7a #x54 #x48 #x7a #x3bc #x2113 #x6d 
     #x2113 #x64 #x2113 #x6b #x2113 #x66 #x6d #x6e 
     #x6d #x3bc #x6d #x6d #x6d #x63 #x6d #x6b 
     #x6d #x6d #x6d #xb2 #x63 #x6d #xb2 #x6d 
     #xb2 #x6b #x6d #xb2 #x6d #x6d #xb3 #x63 
     #x6d #xb3 #x6d #xb3 #x6b #x6d #xb3 #x6d 
     #x2215 #x73 #x6d #x2215 #x73 #xb2 #x50 #x61 
     #x6b #x50 #x61 #x4d #x50 #x61 #x47 #x50 
     #x61 #x72 #x61 #x64 #x72 #x61 #x64 #x2215 
     #x73 #x72 #x61 #x64 #x2215 #x73 #xb2 #x70 
     #x73 #x6e #x73 #x3bc #x73 #x6d #x73 #x70 
     #x56 #x6e #x56 #x3bc #x56 #x6d #x56 #x6b 
     #x56 #x4d #x56 #x70 #x57 #x6e #x57 #x3bc 
     #x57 #x6d #x57 #x6b #x57 #x4d #x57 #x6b 
     #x3a9 #x4d #x3a9 #x61 #x2e #x6d #x2e #x42 
     #x71 #x63 #x63 #x63 #x64 #x43 #x2215 #x6b 
     #x67 #x43 #x6f #x2e #x64 #x42 #x47 #x79 
     #x68 #x61 #x48 #x50 #x69 #x6e #x4b #x4b 
     #x4b #x4d #x6b #x74 #x6c #x6d #x6c #x6e 
     #x6c #x6f #x67 #x6c #x78 #x6d #x62 #x6d 
     #x69 #x6c #x6d #x6f #x6c #x50 #x48 #x70 
     #x2e #x6d #x2e #x50 #x50 #x4d #x50 #x52 
     #x73 #x72 #x53 #x76 #x57 #x62 #x56 #x2215 
     #x6d #x41 #x2215 #x6d #x31 #x65e5 #x32 #x65e5 
     #x33 #x65e5 #x34 #x65e5 #x35 #x65e5 #x36 #x65e5 
     #x37 #x65e5 #x38 #x65e5 #x39 #x65e5 #x31 #x30 
     #x65e5 #x31 #x31 #x65e5 #x31 #x32 #x65e5 #x31 
     #x33 #x65e5 #x31 #x34 #x65e5 #x31 #x35 #x65e5 
     #x31 #x36 #x65e5 #x31 #x37 #x65e5 #x31 #x38 
     #x65e5 #x31 #x39 #x65e5 #x32 #x30 #x65e5 #x32 
     #x31 #x65e5 #x32 #x32 #x65e5 #x32 #x33 #x65e5 
     #x32 #x34 #x65e5 #x32 #x35 #x65e5 #x32 #x36 
     #x65e5 #x32 #x37 #x65e5 #x32 #x38 #x65e5 #x32 
     #x39 #x65e5 #x33 #x30 #x65e5 #x33 #x31 #x65e5 
     #x67 #x61 #x6c #x8c48 #x66f4 #x8eca #x8cc8 #x6ed1 
     #x4e32 #x53e5 #x9f9c #x9f9c #x5951 #x91d1 #x5587 #x5948 
     #x61f6 #x7669 #x7f85 #x863f #x87ba #x88f8 #x908f #x6a02 
     #x6d1b #x70d9 #x73de #x843d #x916a #x99f1 #x4e82 #x5375 
     #x6b04 #x721b #x862d #x9e1e #x5d50 #x6feb #x85cd #x8964 
     #x62c9 #x81d8 #x881f #x5eca #x6717 #x6d6a #x72fc #x90ce 
     #x4f86 #x51b7 #x52de #x64c4 #x6ad3 #x7210 #x76e7 #x8001 
     #x8606 #x865c #x8def #x9732 #x9b6f #x9dfa #x788c #x797f 
     #x7da0 #x83c9 #x9304 #x9e7f #x8ad6 #x58df #x5f04 #x7c60 
     #x807e #x7262 #x78ca #x8cc2 #x96f7 #x58d8 #x5c62 #x6a13 
     #x6dda #x6f0f #x7d2f #x7e37 #x964b #x52d2 #x808b #x51dc 
     #x51cc #x7a1c #x7dbe #x83f1 #x9675 #x8b80 #x62cf #x6a02 
     #x8afe #x4e39 #x5be7 #x6012 #x7387 #x7570 #x5317 #x78fb 
     #x4fbf #x5fa9 #x4e0d #x6ccc #x6578 #x7d22 #x53c3 #x585e 
     #x7701 #x8449 #x8aaa #x6bba #x8fb0 #x6c88 #x62fe #x82e5 
     #x63a0 #x7565 #x4eae #x5169 #x51c9 #x6881 #x7ce7 #x826f 
     #x8ad2 #x91cf #x52f5 #x5442 #x5973 #x5eec #x65c5 #x6ffe 
     #x792a #x95ad #x9a6a #x9e97 #x9ece #x529b #x66c6 #x6b77 
     #x8f62 #x5e74 #x6190 #x6200 #x649a #x6f23 #x7149 #x7489 
     #x79ca #x7df4 #x806f #x8f26 #x84ee #x9023 #x934a #x5217 
     #x52a3 #x54bd #x70c8 #x88c2 #x8aaa #x5ec9 #x5ff5 #x637b 
     #x6bae #x7c3e #x7375 #x4ee4 #x56f9 #x5be7 #x5dba #x601c 
     #x73b2 #x7469 #x7f9a #x8046 #x9234 #x96f6 #x9748 #x9818 
     #x4f8b #x79ae #x91b4 #x96b8 #x60e1 #x4e86 #x50da #x5bee 
     #x5c3f #x6599 #x6a02 #x71ce #x7642 #x84fc #x907c #x9f8d 
     #x6688 #x962e #x5289 #x677b #x67f3 #x6d41 #x6e9c #x7409 
     #x7559 #x786b #x7d10 #x985e #x516d #x622e #x9678 #x502b 
     #x5d19 #x6dea #x8f2a #x5f8b #x6144 #x6817 #x7387 #x9686 
     #x5229 #x540f #x5c65 #x6613 #x674e #x68a8 #x6ce5 #x7406 
     #x75e2 #x7f79 #x88cf #x88e1 #x91cc #x96e2 #x533f #x6eba 
     #x541d #x71d0 #x7498 #x85fa #x96a3 #x9c57 #x9e9f #x6797 
     #x6dcb #x81e8 #x7acb #x7b20 #x7c92 #x72c0 #x7099 #x8b58 
     #x4ec0 #x8336 #x523a #x5207 #x5ea6 #x62d3 #x7cd6 #x5b85 
     #x6d1e #x66b4 #x8f3b #x884c #x964d #x898b #x5ed3 #x5140 
     #x55c0 #x585a #x6674 #x51de #x732a #x76ca #x793c #x795e 
     #x7965 #x798f #x9756 #x7cbe #x7fbd #x8612 #x8af8 #x9038 
     #x90fd #x98ef #x98fc #x9928 #x9db4 #x4fae #x50e7 #x514d 
     #x52c9 #x52e4 #x5351 #x559d #x5606 #x5668 #x5840 #x58a8 
     #x5c64 #x5c6e #x6094 #x6168 #x618e #x61f2 #x654f #x65e2 
     #x6691 #x6885 #x6d77 #x6e1a #x6f22 #x716e #x722b #x7422 
     #x7891 #x793e #x7949 #x7948 #x7950 #x7956 #x795d #x798d 
     #x798e #x7a40 #x7a81 #x7bc0 #x7df4 #x7e09 #x7e41 #x7f72 
     #x8005 #x81ed #x8279 #x8279 #x8457 #x8910 #x8996 #x8b01 
     #x8b39 #x8cd3 #x8d08 #x8fb6 #x9038 #x96e3 #x97ff #x983b 
     #x4e26 #x51b5 #x5168 #x4f80 #x5145 #x5180 #x52c7 #x52fa 
     #x559d #x5555 #x5599 #x55e2 #x585a #x58b3 #x5944 #x5954 
     #x5a62 #x5b28 #x5ed2 #x5ed9 #x5f69 #x5fad #x60d8 #x614e 
     #x6108 #x618e #x6160 #x61f2 #x6234 #x63c4 #x641c #x6452 
     #x6556 #x6674 #x6717 #x671b #x6756 #x6b79 #x6bba #x6d41 
     #x6edb #x6ecb #x6f22 #x701e #x716e #x77a7 #x7235 #x72af 
     #x732a #x7471 #x7506 #x753b #x761d #x761f #x76ca #x76db 
     #x76f4 #x774a #x7740 #x78cc #x7ab1 #x7bc0 #x7c7b #x7d5b 
     #x7df4 #x7f3e #x8005 #x8352 #x83ef #x8779 #x8941 #x8986 
     #x8996 #x8abf #x8af8 #x8acb #x8b01 #x8afe #x8aed #x8b39 
     #x8b8a #x8d08 #x8f38 #x9072 #x9199 #x9276 #x967c #x96e3 
     #x9756 #x97db #x97ff #x980b #x983b #x9b12 #x9f9c #x2284a 
     #x22844 #x233d5 #x3b9d #x4018 #x4039 #x25249 #x25cd0 #x27ed3 
     #x9f43 #x9f8e #x66 #x66 #x66 #x69 #x66 #x6c 
     #x66 #x66 #x69 #x66 #x66 #x6c #x17f #x74 
     #x73 #x74 #x574 #x576 #x574 #x565 #x574 #x56b 
     #x57e #x576 #x574 #x56d #x5d9 #x5b4 #x5f2 #x5b7 
     #x5e2 #x5d0 #x5d3 #x5d4 #x5db #x5dc #x5dd #x5e8 
     #x5ea #x2b #x5e9 #x5c1 #x5e9 #x5c2 #xfb49 #x5c1 
     #xfb49 #x5c2 #x5d0 #x5b7 #x5d0 #x5b8 #x5d0 #x5bc 
     #x5d1 #x5bc #x5d2 #x5bc #x5d3 #x5bc #x5d4 #x5bc 
     #x5d5 #x5bc #x5d6 #x5bc #x5d8 #x5bc #x5d9 #x5bc 
     #x5da #x5bc #x5db #x5bc #x5dc #x5bc #x5de #x5bc 
     #x5e0 #x5bc #x5e1 #x5bc #x5e3 #x5bc #x5e4 #x5bc 
     #x5e6 #x5bc #x5e7 #x5bc #x5e8 #x5bc #x5e9 #x5bc 
     #x5ea #x5bc #x5d5 #x5b9 #x5d1 #x5bf #x5db #x5bf 
     #x5e4 #x5bf #x5d0 #x5dc #x671 #x671 #x67b #x67b 
     #x67b #x67b #x67e #x67e #x67e #x67e #x680 #x680 
     #x680 #x680 #x67a #x67a #x67a #x67a #x67f #x67f 
     #x67f #x67f #x679 #x679 #x679 #x679 #x6a4 #x6a4 
     #x6a4 #x6a4 #x6a6 #x6a6 #x6a6 #x6a6 #x684 #x684 
     #x684 #x684 #x683 #x683 #x683 #x683 #x686 #x686 
     #x686 #x686 #x687 #x687 #x687 #x687 #x68d #x68d 
     #x68c #x68c #x68e #x68e #x688 #x688 #x698 #x698 
     #x691 #x691 #x6a9 #x6a9 #x6a9 #x6a9 #x6af #x6af 
     #x6af #x6af #x6b3 #x6b3 #x6b3 #x6b3 #x6b1 #x6b1 
     #x6b1 #x6b1 #x6ba #x6ba #x6bb #x6bb #x6bb #x6bb 
     #x6c0 #x6c0 #x6c1 #x6c1 #x6c1 #x6c1 #x6be #x6be 
     #x6be #x6be #x6d2 #x6d2 #x6d3 #x6d3 #x6ad #x6ad 
     #x6ad #x6ad #x6c7 #x6c7 #x6c6 #x6c6 #x6c8 #x6c8 
     #x677 #x6cb #x6cb #x6c5 #x6c5 #x6c9 #x6c9 #x6d0 
     #x6d0 #x6d0 #x6d0 #x649 #x649 #x626 #x627 #x626 
     #x627 #x626 #x6d5 #x626 #x6d5 #x626 #x648 #x626 
     #x648 #x626 #x6c7 #x626 #x6c7 #x626 #x6c6 #x626 
     #x6c6 #x626 #x6c8 #x626 #x6c8 #x626 #x6d0 #x626 
     #x6d0 #x626 #x6d0 #x626 #x649 #x626 #x649 #x626 
     #x649 #x6cc #x6cc #x6cc #x6cc #x626 #x62c #x626 
     #x62d #x626 #x645 #x626 #x649 #x626 #x64a #x628 
     #x62c #x628 #x62d #x628 #x62e #x628 #x645 #x628 
     #x649 #x628 #x64a #x62a #x62c #x62a #x62d #x62a 
     #x62e #x62a #x645 #x62a #x649 #x62a #x64a #x62b 
     #x62c #x62b #x645 #x62b #x649 #x62b #x64a #x62c 
     #x62d #x62c #x645 #x62d #x62c #x62d #x645 #x62e 
     #x62c #x62e #x62d #x62e #x645 #x633 #x62c #x633 
     #x62d #x633 #x62e #x633 #x645 #x635 #x62d #x635 
     #x645 #x636 #x62c #x636 #x62d #x636 #x62e #x636 
     #x645 #x637 #x62d #x637 #x645 #x638 #x645 #x639 
     #x62c #x639 #x645 #x63a #x62c #x63a #x645 #x641 
     #x62c #x641 #x62d #x641 #x62e #x641 #x645 #x641 
     #x649 #x641 #x64a #x642 #x62d #x642 #x645 #x642 
     #x649 #x642 #x64a #x643 #x627 #x643 #x62c #x643 
     #x62d #x643 #x62e #x643 #x644 #x643 #x645 #x643 
     #x649 #x643 #x64a #x644 #x62c #x644 #x62d #x644 
     #x62e #x644 #x645 #x644 #x649 #x644 #x64a #x645 
     #x62c #x645 #x62d #x645 #x62e #x645 #x645 #x645 
     #x649 #x645 #x64a #x646 #x62c #x646 #x62d #x646 
     #x62e #x646 #x645 #x646 #x649 #x646 #x64a #x647 
     #x62c #x647 #x645 #x647 #x649 #x647 #x64a #x64a 
     #x62c #x64a #x62d #x64a #x62e #x64a #x645 #x64a 
     #x649 #x64a #x64a #x630 #x670 #x631 #x670 #x649 
     #x670 #x20 #x64c #x651 #x20 #x64d #x651 #x20 
     #x64e #x651 #x20 #x64f #x651 #x20 #x650 #x651 
     #x20 #x651 #x670 #x626 #x631 #x626 #x632 #x626 
     #x645 #x626 #x646 #x626 #x649 #x626 #x64a #x628 
     #x631 #x628 #x632 #x628 #x645 #x628 #x646 #x628 
     #x649 #x628 #x64a #x62a #x631 #x62a #x632 #x62a 
     #x645 #x62a #x646 #x62a #x649 #x62a #x64a #x62b 
     #x631 #x62b #x632 #x62b #x645 #x62b #x646 #x62b 
     #x649 #x62b #x64a #x641 #x649 #x641 #x64a #x642 
     #x649 #x642 #x64a #x643 #x627 #x643 #x644 #x643 
     #x645 #x643 #x649 #x643 #x64a #x644 #x645 #x644 
     #x649 #x644 #x64a #x645 #x627 #x645 #x645 #x646 
     #x631 #x646 #x632 #x646 #x645 #x646 #x646 #x646 
     #x649 #x646 #x64a #x649 #x670 #x64a #x631 #x64a 
     #x632 #x64a #x645 #x64a #x646 #x64a #x649 #x64a 
     #x64a #x626 #x62c #x626 #x62d #x626 #x62e #x626 
     #x645 #x626 #x647 #x628 #x62c #x628 #x62d #x628 
     #x62e #x628 #x645 #x628 #x647 #x62a #x62c #x62a 
     #x62d #x62a #x62e #x62a #x645 #x62a #x647 #x62b 
     #x645 #x62c #x62d #x62c #x645 #x62d #x62c #x62d 
     #x645 #x62e #x62c #x62e #x645 #x633 #x62c #x633 
     #x62d #x633 #x62e #x633 #x645 #x635 #x62d #x635 
     #x62e #x635 #x645 #x636 #x62c #x636 #x62d #x636 
     #x62e #x636 #x645 #x637 #x62d #x638 #x645 #x639 
     #x62c #x639 #x645 #x63a #x62c #x63a #x645 #x641 
     #x62c #x641 #x62d #x641 #x62e #x641 #x645 #x642 
     #x62d #x642 #x645 #x643 #x62c #x643 #x62d #x643 
     #x62e #x643 #x644 #x643 #x645 #x644 #x62c #x644 
     #x62d #x644 #x62e #x644 #x645 #x644 #x647 #x645 
     #x62c #x645 #x62d #x645 #x62e #x645 #x645 #x646 
     #x62c #x646 #x62d #x646 #x62e #x646 #x645 #x646 
     #x647 #x647 #x62c #x647 #x645 #x647 #x670 #x64a 
     #x62c #x64a #x62d #x64a #x62e #x64a #x645 #x64a 
     #x647 #x626 #x645 #x626 #x647 #x628 #x645 #x628 
     #x647 #x62a #x645 #x62a #x647 #x62b #x645 #x62b 
     #x647 #x633 #x645 #x633 #x647 #x634 #x645 #x634 
     #x647 #x643 #x644 #x643 #x645 #x644 #x645 #x646 
     #x645 #x646 #x647 #x64a #x645 #x64a #x647 #x640 
     #x64e #x651 #x640 #x64f #x651 #x640 #x650 #x651 
     #x637 #x649 #x637 #x64a #x639 #x649 #x639 #x64a 
     #x63a #x649 #x63a #x64a #x633 #x649 #x633 #x64a 
     #x634 #x649 #x634 #x64a #x62d #x649 #x62d #x64a 
     #x62c #x649 #x62c #x64a #x62e #x649 #x62e #x64a 
     #x635 #x649 #x635 #x64a #x636 #x649 #x636 #x64a 
     #x634 #x62c #x634 #x62d #x634 #x62e #x634 #x645 
     #x634 #x631 #x633 #x631 #x635 #x631 #x636 #x631 
     #x637 #x649 #x637 #x64a #x639 #x649 #x639 #x64a 
     #x63a #x649 #x63a #x64a #x633 #x649 #x633 #x64a 
     #x634 #x649 #x634 #x64a #x62d #x649 #x62d #x64a 
     #x62c #x649 #x62c #x64a #x62e #x649 #x62e #x64a 
     #x635 #x649 #x635 #x64a #x636 #x649 #x636 #x64a 
     #x634 #x62c #x634 #x62d #x634 #x62e #x634 #x645 
     #x634 #x631 #x633 #x631 #x635 #x631 #x636 #x631 
     #x634 #x62c #x634 #x62d #x634 #x62e #x634 #x645 
     #x633 #x647 #x634 #x647 #x637 #x645 #x633 #x62c 
     #x633 #x62d #x633 #x62e #x634 #x62c #x634 #x62d 
     #x634 #x62e #x637 #x645 #x638 #x645 #x627 #x64b 
     #x627 #x64b #x62a #x62c #x645 #x62a #x62d #x62c 
     #x62a #x62d #x62c #x62a #x62d #x645 #x62a #x62e 
     #x645 #x62a #x645 #x62c #x62a #x645 #x62d #x62a 
     #x645 #x62e #x62c #x645 #x62d #x62c #x645 #x62d 
     #x62d #x645 #x64a #x62d #x645 #x649 #x633 #x62d 
     #x62c #x633 #x62c #x62d #x633 #x62c #x649 #x633 
     #x645 #x62d #x633 #x645 #x62d #x633 #x645 #x62c 
     #x633 #x645 #x645 #x633 #x645 #x645 #x635 #x62d 
     #x62d #x635 #x62d #x62d #x635 #x645 #x645 #x634 
     #x62d #x645 #x634 #x62d #x645 #x634 #x62c #x64a 
     #x634 #x645 #x62e #x634 #x645 #x62e #x634 #x645 
     #x645 #x634 #x645 #x645 #x636 #x62d #x649 #x636 
     #x62e #x645 #x636 #x62e #x645 #x637 #x645 #x62d 
     #x637 #x645 #x62d #x637 #x645 #x645 #x637 #x645 
     #x64a #x639 #x62c #x645 #x639 #x645 #x645 #x639 
     #x645 #x645 #x639 #x645 #x649 #x63a #x645 #x645 
     #x63a #x645 #x64a #x63a #x645 #x649 #x641 #x62e 
     #x645 #x641 #x62e #x645 #x642 #x645 #x62d #x642 
     #x645 #x645 #x644 #x62d #x645 #x644 #x62d #x64a 
     #x644 #x62d #x649 #x644 #x62c #x62c #x644 #x62c 
     #x62c #x644 #x62e #x645 #x644 #x62e #x645 #x644 
     #x645 #x62d #x644 #x645 #x62d #x645 #x62d #x62c 
     #x645 #x62d #x645 #x645 #x62d #x64a #x645 #x62c 
     #x62d #x645 #x62c #x645 #x645 #x62e #x62c #x645 
     #x62e #x645 #x645 #x62c #x62e #x647 #x645 #x62c 
     #x647 #x645 #x645 #x646 #x62d #x645 #x646 #x62d 
     #x649 #x646 #x62c #x645 #x646 #x62c #x645 #x646 
     #x62c #x649 #x646 #x645 #x64a #x646 #x645 #x649 
     #x64a #x645 #x645 #x64a #x645 #x645 #x628 #x62e 
     #x64a #x62a #x62c #x64a #x62a #x62c #x649 #x62a 
     #x62e #x64a #x62a #x62e #x649 #x62a #x645 #x64a 
     #x62a #x645 #x649 #x62c #x645 #x64a #x62c #x62d 
     #x649 #x62c #x645 #x649 #x633 #x62e #x649 #x635 
     #x62d #x64a #x634 #x62d #x64a #x636 #x62d #x64a 
     #x644 #x62c #x64a #x644 #x645 #x64a #x64a #x62d 
     #x64a #x64a #x62c #x64a #x64a #x645 #x64a #x645 
     #x645 #x64a #x642 #x645 #x64a #x646 #x62d #x64a 
     #x642 #x645 #x62d #x644 #x62d #x645 #x639 #x645 
     #x64a #x643 #x645 #x64a #x646 #x62c #x62d #x645 
     #x62e #x64a #x644 #x62c #x645 #x643 #x645 #x645 
     #x644 #x62c #x645 #x646 #x62c #x62d #x62c #x62d 
     #x64a #x62d #x62c #x64a #x645 #x62c #x64a #x641 
     #x645 #x64a #x628 #x62d #x64a #x643 #x645 #x645 
     #x639 #x62c #x645 #x635 #x645 #x645 #x633 #x62e 
     #x64a #x646 #x62c #x64a #x635 #x644 #x6d2 #x642 
     #x644 #x6d2 #x627 #x644 #x644 #x647 #x627 #x643 
     #x628 #x631 #x645 #x62d #x645 #x62f #x635 #x644 
     #x639 #x645 #x631 #x633 #x648 #x644 #x639 #x644 
     #x64a #x647 #x648 #x633 #x644 #x645 #x635 #x644 
     #x649 #x635 #x644 #x649 #x20 #x627 #x644 #x644 
     #x647 #x20 #x639 #x644 #x64a #x647 #x20 #x648 
     #x633 #x644 #x645 #x62c #x644 #x20 #x62c #x644 
     #x627 #x644 #x647 #x631 #x6cc #x627 #x644 #x2c 
     #x3001 #x3002 #x3a #x3b #x21 #x3f #x3016 #x3017 
     #x2026 #x2025 #x2014 #x2013 #x5f #x5f #x28 #x29 
     #x7b #x7d #x3014 #x3015 #x3010 #x3011 #x300a #x300b 
     #x3008 #x3009 #x300c #x300d #x300e #x300f #x5b #x5d 
     #x203e #x203e #x203e #x203e #x5f #x5f #x5f #x2c 
     #x3001 #x2e #x3b #x3a #x3f #x21 #x2014 #x28 
     #x29 #x7b #x7d #x3014 #x3015 #x23 #x26 #x2a 
     #x2b #x2d #x3c #x3e #x3d #x5c #x24 #x25 
     #x40 #x20 #x64b #x640 #x64b #x20 #x64c #x20 
     #x64d #x20 #x64e #x640 #x64e #x20 #x64f #x640 
     #x64f #x20 #x650 #x640 #x650 #x20 #x651 #x640 
     #x651 #x20 #x652 #x640 #x652 #x621 #x622 #x622 
     #x623 #x623 #x624 #x624 #x625 #x625 #x626 #x626 
     #x626 #x626 #x627 #x627 #x628 #x628 #x628 #x628 
     #x629 #x629 #x62a #x62a #x62a #x62a #x62b #x62b 
     #x62b #x62b #x62c #x62c #x62c #x62c #x62d #x62d 
     #x62d #x62d #x62e #x62e #x62e #x62e #x62f #x62f 
     #x630 #x630 #x631 #x631 #x632 #x632 #x633 #x633 
     #x633 #x633 #x634 #x634 #x634 #x634 #x635 #x635 
     #x635 #x635 #x636 #x636 #x636 #x636 #x637 #x637 
     #x637 #x637 #x638 #x638 #x638 #x638 #x639 #x639 
     #x639 #x639 #x63a #x63a #x63a #x63a #x641 #x641 
     #x641 #x641 #x642 #x642 #x642 #x642 #x643 #x643 
     #x643 #x643 #x644 #x644 #x644 #x644 #x645 #x645 
     #x645 #x645 #x646 #x646 #x646 #x646 #x647 #x647 
     #x647 #x647 #x648 #x648 #x649 #x649 #x64a #x64a 
     #x64a #x64a #x644 #x622 #x644 #x622 #x644 #x623 
     #x644 #x623 #x644 #x625 #x644 #x625 #x644 #x627 
     #x644 #x627 #x21 #x22 #x23 #x24 #x25 #x26 
     #x27 #x28 #x29 #x2a #x2b #x2c #x2d #x2e 
     #x2f #x30 #x31 #x32 #x33 #x34 #x35 #x36 
     #x37 #x38 #x39 #x3a #x3b #x3c #x3d #x3e 
     #x3f #x40 #x41 #x42 #x43 #x44 #x45 #x46 
     #x47 #x48 #x49 #x4a #x4b #x4c #x4d #x4e 
     #x4f #x50 #x51 #x52 #x53 #x54 #x55 #x56 
     #x57 #x58 #x59 #x5a #x5b #x5c #x5d #x5e 
     #x5f #x60 #x61 #x62 #x63 #x64 #x65 #x66 
     #x67 #x68 #x69 #x6a #x6b #x6c #x6d #x6e 
     #x6f #x70 #x71 #x72 #x73 #x74 #x75 #x76 
     #x77 #x78 #x79 #x7a #x7b #x7c #x7d #x7e 
     #x2985 #x2986 #x3002 #x300c #x300d #x3001 #x30fb #x30f2 
     #x30a1 #x30a3 #x30a5 #x30a7 #x30a9 #x30e3 #x30e5 #x30e7 
     #x30c3 #x30fc #x30a2 #x30a4 #x30a6 #x30a8 #x30aa #x30ab 
     #x30ad #x30af #x30b1 #x30b3 #x30b5 #x30b7 #x30b9 #x30bb 
     #x30bd #x30bf #x30c1 #x30c4 #x30c6 #x30c8 #x30ca #x30cb 
     #x30cc #x30cd #x30ce #x30cf #x30d2 #x30d5 #x30d8 #x30db 
     #x30de #x30df #x30e0 #x30e1 #x30e2 #x30e4 #x30e6 #x30e8 
     #x30e9 #x30ea #x30eb #x30ec #x30ed #x30ef #x30f3 #x3099 
     #x309a #x3164 #x3131 #x3132 #x3133 #x3134 #x3135 #x3136 
     #x3137 #x3138 #x3139 #x313a #x313b #x313c #x313d #x313e 
     #x313f #x3140 #x3141 #x3142 #x3143 #x3144 #x3145 #x3146 
     #x3147 #x3148 #x3149 #x314a #x314b #x314c #x314d #x314e 
     #x314f #x3150 #x3151 #x3152 #x3153 #x3154 #x3155 #x3156 
     #x3157 #x3158 #x3159 #x315a #x315b #x315c #x315d #x315e 
     #x315f #x3160 #x3161 #x3162 #x3163 #xa2 #xa3 #xac 
     #xaf #xa6 #xa5 #x20a9 #x2502 #x2190 #x2191 #x2192 
     #x2193 #x25a0 #x25cb #x1d157 #x1d165 #x1d158 #x1d165 #x1d15f 
     #x1d16e #x1d15f #x1d16f #x1d15f #x1d170 #x1d15f #x1d171 #x1d15f 
     #x1d172 #x1d1b9 #x1d165 #x1d1ba #x1d165 #x1d1bb #x1d16e #x1d1bc 
     #x1d16e #x1d1bb #x1d16f #x1d1bc #x1d16f #x41 #x42 #x43 
     #x44 #x45 #x46 #x47 #x48 #x49 #x4a #x4b 
     #x4c #x4d #x4e #x4f #x50 #x51 #x52 #x53 
     #x54 #x55 #x56 #x57 #x58 #x59 #x5a #x61 
     #x62 #x63 #x64 #x65 #x66 #x67 #x68 #x69 
     #x6a #x6b #x6c #x6d #x6e #x6f #x70 #x71 
     #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 
     #x7a #x41 #x42 #x43 #x44 #x45 #x46 #x47 
     #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f 
     #x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 
     #x58 #x59 #x5a #x61 #x62 #x63 #x64 #x65 
     #x66 #x67 #x69 #x6a #x6b #x6c #x6d #x6e 
     #x6f #x70 #x71 #x72 #x73 #x74 #x75 #x76 
     #x77 #x78 #x79 #x7a #x41 #x42 #x43 #x44 
     #x45 #x46 #x47 #x48 #x49 #x4a #x4b #x4c 
     #x4d #x4e #x4f #x50 #x51 #x52 #x53 #x54 
     #x55 #x56 #x57 #x58 #x59 #x5a #x61 #x62 
     #x63 #x64 #x65 #x66 #x67 #x68 #x69 #x6a 
     #x6b #x6c #x6d #x6e #x6f #x70 #x71 #x72 
     #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7a 
     #x41 #x43 #x44 #x47 #x4a #x4b #x4e #x4f 
     #x50 #x51 #x53 #x54 #x55 #x56 #x57 #x58 
     #x59 #x5a #x61 #x62 #x63 #x64 #x66 #x68 
     #x69 #x6a #x6b #x6c #x6d #x6e #x70 #x71 
     #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 
     #x7a #x41 #x42 #x43 #x44 #x45 #x46 #x47 
     #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f 
     #x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 
     #x58 #x59 #x5a #x61 #x62 #x63 #x64 #x65 
     #x66 #x67 #x68 #x69 #x6a #x6b #x6c #x6d 
     #x6e #x6f #x70 #x71 #x72 #x73 #x74 #x75 
     #x76 #x77 #x78 #x79 #x7a #x41 #x42 #x44 
     #x45 #x46 #x47 #x4a #x4b #x4c #x4d #x4e 
     #x4f #x50 #x51 #x53 #x54 #x55 #x56 #x57 
     #x58 #x59 #x61 #x62 #x63 #x64 #x65 #x66 
     #x67 #x68 #x69 #x6a #x6b #x6c #x6d #x6e 
     #x6f #x70 #x71 #x72 #x73 #x74 #x75 #x76 
     #x77 #x78 #x79 #x7a #x41 #x42 #x44 #x45 
     #x46 #x47 #x49 #x4a #x4b #x4c #x4d #x4f 
     #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x61 
     #x62 #x63 #x64 #x65 #x66 #x67 #x68 #x69 
     #x6a #x6b #x6c #x6d #x6e #x6f #x70 #x71 
     #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 
     #x7a #x41 #x42 #x43 #x44 #x45 #x46 #x47 
     #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f 
     #x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 
     #x58 #x59 #x5a #x61 #x62 #x63 #x64 #x65 
     #x66 #x67 #x68 #x69 #x6a #x6b #x6c #x6d 
     #x6e #x6f #x70 #x71 #x72 #x73 #x74 #x75 
     #x76 #x77 #x78 #x79 #x7a #x41 #x42 #x43 
     #x44 #x45 #x46 #x47 #x48 #x49 #x4a #x4b 
     #x4c #x4d #x4e #x4f #x50 #x51 #x52 #x53 
     #x54 #x55 #x56 #x57 #x58 #x59 #x5a #x61 
     #x62 #x63 #x64 #x65 #x66 #x67 #x68 #x69 
     #x6a #x6b #x6c #x6d #x6e #x6f #x70 #x71 
     #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 
     #x7a #x41 #x42 #x43 #x44 #x45 #x46 #x47 
     #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f 
     #x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 
     #x58 #x59 #x5a #x61 #x62 #x63 #x64 #x65 
     #x66 #x67 #x68 #x69 #x6a #x6b #x6c #x6d 
     #x6e #x6f #x70 #x71 #x72 #x73 #x74 #x75 
     #x76 #x77 #x78 #x79 #x7a #x41 #x42 #x43 
     #x44 #x45 #x46 #x47 #x48 #x49 #x4a #x4b 
     #x4c #x4d #x4e #x4f #x50 #x51 #x52 #x53 
     #x54 #x55 #x56 #x57 #x58 #x59 #x5a #x61 
     #x62 #x63 #x64 #x65 #x66 #x67 #x68 #x69 
     #x6a #x6b #x6c #x6d #x6e #x6f #x70 #x71 
     #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 
     #x7a #x41 #x42 #x43 #x44 #x45 #x46 #x47 
     #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f 
     #x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 
     #x58 #x59 #x5a #x61 #x62 #x63 #x64 #x65 
     #x66 #x67 #x68 #x69 #x6a #x6b #x6c #x6d 
     #x6e #x6f #x70 #x71 #x72 #x73 #x74 #x75 
     #x76 #x77 #x78 #x79 #x7a #x41 #x42 #x43 
     #x44 #x45 #x46 #x47 #x48 #x49 #x4a #x4b 
     #x4c #x4d #x4e #x4f #x50 #x51 #x52 #x53 
     #x54 #x55 #x56 #x57 #x58 #x59 #x5a #x61 
     #x62 #x63 #x64 #x65 #x66 #x67 #x68 #x69 
     #x6a #x6b #x6c #x6d #x6e #x6f #x70 #x71 
     #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 
     #x7a #x131 #x237 #x391 #x392 #x393 #x394 #x395 
     #x396 #x397 #x398 #x399 #x39a #x39b #x39c #x39d 
     #x39e #x39f #x3a0 #x3a1 #x3f4 #x3a3 #x3a4 #x3a5 
     #x3a6 #x3a7 #x3a8 #x3a9 #x2207 #x3b1 #x3b2 #x3b3 
     #x3b4 #x3b5 #x3b6 #x3b7 #x3b8 #x3b9 #x3ba #x3bb 
     #x3bc #x3bd #x3be #x3bf #x3c0 #x3c1 #x3c2 #x3c3 
     #x3c4 #x3c5 #x3c6 #x3c7 #x3c8 #x3c9 #x2202 #x3f5 
     #x3d1 #x3f0 #x3d5 #x3f1 #x3d6 #x391 #x392 #x393 
     #x394 #x395 #x396 #x397 #x398 #x399 #x39a #x39b 
     #x39c #x39d #x39e #x39f #x3a0 #x3a1 #x3f4 #x3a3 
     #x3a4 #x3a5 #x3a6 #x3a7 #x3a8 #x3a9 #x2207 #x3b1 
     #x3b2 #x3b3 #x3b4 #x3b5 #x3b6 #x3b7 #x3b8 #x3b9 
     #x3ba #x3bb #x3bc #x3bd #x3be #x3bf #x3c0 #x3c1 
     #x3c2 #x3c3 #x3c4 #x3c5 #x3c6 #x3c7 #x3c8 #x3c9 
     #x2202 #x3f5 #x3d1 #x3f0 #x3d5 #x3f1 #x3d6 #x391 
     #x392 #x393 #x394 #x395 #x396 #x397 #x398 #x399 
     #x39a #x39b #x39c #x39d #x39e #x39f #x3a0 #x3a1 
     #x3f4 #x3a3 #x3a4 #x3a5 #x3a6 #x3a7 #x3a8 #x3a9 
     #x2207 #x3b1 #x3b2 #x3b3 #x3b4 #x3b5 #x3b6 #x3b7 
     #x3b8 #x3b9 #x3ba #x3bb #x3bc #x3bd #x3be #x3bf 
     #x3c0 #x3c1 #x3c2 #x3c3 #x3c4 #x3c5 #x3c6 #x3c7 
     #x3c8 #x3c9 #x2202 #x3f5 #x3d1 #x3f0 #x3d5 #x3f1 
     #x3d6 #x391 #x392 #x393 #x394 #x395 #x396 #x397 
     #x398 #x399 #x39a #x39b #x39c #x39d #x39e #x39f 
     #x3a0 #x3a1 #x3f4 #x3a3 #x3a4 #x3a5 #x3a6 #x3a7 
     #x3a8 #x3a9 #x2207 #x3b1 #x3b2 #x3b3 #x3b4 #x3b5 
     #x3b6 #x3b7 #x3b8 #x3b9 #x3ba #x3bb #x3bc #x3bd 
     #x3be #x3bf #x3c0 #x3c1 #x3c2 #x3c3 #x3c4 #x3c5 
     #x3c6 #x3c7 #x3c8 #x3c9 #x2202 #x3f5 #x3d1 #x3f0 
     #x3d5 #x3f1 #x3d6 #x391 #x392 #x393 #x394 #x395 
     #x396 #x397 #x398 #x399 #x39a #x39b #x39c #x39d 
     #x39e #x39f #x3a0 #x3a1 #x3f4 #x3a3 #x3a4 #x3a5 
     #x3a6 #x3a7 #x3a8 #x3a9 #x2207 #x3b1 #x3b2 #x3b3 
     #x3b4 #x3b5 #x3b6 #x3b7 #x3b8 #x3b9 #x3ba #x3bb 
     #x3bc #x3bd #x3be #x3bf #x3c0 #x3c1 #x3c2 #x3c3 
     #x3c4 #x3c5 #x3c6 #x3c7 #x3c8 #x3c9 #x2202 #x3f5 
     #x3d1 #x3f0 #x3d5 #x3f1 #x3d6 #x3dc #x3dd #x30 
     #x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38 
     #x39 #x30 #x31 #x32 #x33 #x34 #x35 #x36 
     #x37 #x38 #x39 #x30 #x31 #x32 #x33 #x34 
     #x35 #x36 #x37 #x38 #x39 #x30 #x31 #x32 
     #x33 #x34 #x35 #x36 #x37 #x38 #x39 #x30 
     #x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38 
     #x39 #x4e3d #x4e38 #x4e41 #x20122 #x4f60 #x4fae #x4fbb 
     #x5002 #x507a #x5099 #x50e7 #x50cf #x349e #x2063a #x514d 
     #x5154 #x5164 #x5177 #x2051c #x34b9 #x5167 #x518d #x2054b 
     #x5197 #x51a4 #x4ecc #x51ac #x51b5 #x291df #x51f5 #x5203 
     #x34df #x523b #x5246 #x5272 #x5277 #x3515 #x52c7 #x52c9 
     #x52e4 #x52fa #x5305 #x5306 #x5317 #x5349 #x5351 #x535a 
     #x5373 #x537d #x537f #x537f #x537f #x20a2c #x7070 #x53ca 
     #x53df #x20b63 #x53eb #x53f1 #x5406 #x549e #x5438 #x5448 
     #x5468 #x54a2 #x54f6 #x5510 #x5553 #x5563 #x5584 #x5584 
     #x5599 #x55ab #x55b3 #x55c2 #x5716 #x5606 #x5717 #x5651 
     #x5674 #x5207 #x58ee #x57ce #x57f4 #x580d #x578b #x5832 
     #x5831 #x58ac #x214e4 #x58f2 #x58f7 #x5906 #x591a #x5922 
     #x5962 #x216a8 #x216ea #x59ec #x5a1b #x5a27 #x59d8 #x5a66 
     #x36ee #x36fc #x5b08 #x5b3e #x5b3e #x219c8 #x5bc3 #x5bd8 
     #x5be7 #x5bf3 #x21b18 #x5bff #x5c06 #x5f53 #x5c22 #x3781 
     #x5c60 #x5c6e #x5cc0 #x5c8d #x21de4 #x5d43 #x21de6 #x5d6e 
     #x5d6b #x5d7c #x5de1 #x5de2 #x382f #x5dfd #x5e28 #x5e3d 
     #x5e69 #x3862 #x22183 #x387c #x5eb0 #x5eb3 #x5eb6 #x5eca 
     #x2a392 #x5efe #x22331 #x22331 #x8201 #x5f22 #x5f22 #x38c7 
     #x232b8 #x261da #x5f62 #x5f6b #x38e3 #x5f9a #x5fcd #x5fd7 
     #x5ff9 #x6081 #x393a #x391c #x6094 #x226d4 #x60c7 #x6148 
     #x614c #x614e #x614c #x617a #x618e #x61b2 #x61a4 #x61af 
     #x61de #x61f2 #x61f6 #x6210 #x621b #x625d #x62b1 #x62d4 
     #x6350 #x22b0c #x633d #x62fc #x6368 #x6383 #x63e4 #x22bf1 
     #x6422 #x63c5 #x63a9 #x3a2e #x6469 #x647e #x649d #x6477 
     #x3a6c #x654f #x656c #x2300a #x65e3 #x66f8 #x6649 #x3b19 
     #x6691 #x3b08 #x3ae4 #x5192 #x5195 #x6700 #x669c #x80ad 
     #x43d9 #x6717 #x671b #x6721 #x675e #x6753 #x233c3 #x3b49 
     #x67fa #x6785 #x6852 #x6885 #x2346d #x688e #x681f #x6914 
     #x3b9d #x6942 #x69a3 #x69ea #x6aa8 #x236a3 #x6adb #x3c18 
     #x6b21 #x238a7 #x6b54 #x3c4e #x6b72 #x6b9f #x6bba #x6bbb 
     #x23a8d #x21d0b #x23afa #x6c4e #x23cbc #x6cbf #x6ccd #x6c67 
     #x6d16 #x6d3e #x6d77 #x6d41 #x6d69 #x6d78 #x6d85 #x23d1e 
     #x6d34 #x6e2f #x6e6e #x3d33 #x6ecb #x6ec7 #x23ed1 #x6df9 
     #x6f6e #x23f5e #x23f8e #x6fc6 #x7039 #x701e #x701b #x3d96 
     #x704a #x707d #x7077 #x70ad #x20525 #x7145 #x24263 #x719c 
     #x243ab #x7228 #x7235 #x7250 #x24608 #x7280 #x7295 #x24735 
     #x24814 #x737a #x738b #x3eac #x73a5 #x3eb8 #x3eb8 #x7447 
     #x745c #x7471 #x7485 #x74ca #x3f1b #x7524 #x24c36 #x753e 
     #x24c92 #x7570 #x2219f #x7610 #x24fa1 #x24fb8 #x25044 #x3ffc 
     #x4008 #x76f4 #x250f3 #x250f2 #x25119 #x25133 #x771e #x771f 
     #x771f #x774a #x4039 #x778b #x4046 #x4096 #x2541d #x784e 
     #x788c #x78cc #x40e3 #x25626 #x7956 #x2569a #x256c5 #x798f 
     #x79eb #x412f #x7a40 #x7a4a #x7a4f #x2597c #x25aa7 #x25aa7 
     #x7aee #x4202 #x25bab #x7bc6 #x7bc9 #x4227 #x25c80 #x7cd2 
     #x42a0 #x7ce8 #x7ce3 #x7d00 #x25f86 #x7d63 #x4301 #x7dc7 
     #x7e02 #x7e45 #x4334 #x26228 #x26247 #x4359 #x262d9 #x7f7a 
     #x2633e #x7f95 #x7ffa #x8005 #x264da #x26523 #x8060 #x265a8 
     #x8070 #x2335f #x43d5 #x80b2 #x8103 #x440b #x813e #x5ab5 
     #x267a7 #x267b5 #x23393 #x2339c #x8201 #x8204 #x8f9e #x446b 
     #x8291 #x828b #x829d #x52b3 #x82b1 #x82b3 #x82bd #x82e6 
     #x26b3c #x82e5 #x831d #x8363 #x83ad #x8323 #x83bd #x83e7 
     #x8457 #x8353 #x83ca #x83cc #x83dc #x26c36 #x26d6b #x26cd5 
     #x452b #x84f1 #x84f3 #x8516 #x273ca #x8564 #x26f2c #x455d 
     #x4561 #x26fb1 #x270d2 #x456b #x8650 #x865c #x8667 #x8669 
     #x86a9 #x8688 #x870e #x86e2 #x8779 #x8728 #x876b #x8786 
     #x45d7 #x87e1 #x8801 #x45f9 #x8860 #x8863 #x27667 #x88d7 
     #x88de #x4635 #x88fa #x34bb #x278ae #x27966 #x46be #x46c7 
     #x8aa0 #x8aed #x8b8a #x8c55 #x27ca8 #x8cab #x8cc1 #x8d1b 
     #x8d77 #x27f2f #x20804 #x8dcb #x8dbc #x8df0 #x208de #x8ed4 
     #x8f38 #x285d2 #x285ed #x9094 #x90f1 #x9111 #x2872e #x911b 
     #x9238 #x92d7 #x92d8 #x927c #x93f9 #x9415 #x28bfa #x958b 
     #x4995 #x95b7 #x28d77 #x49e6 #x96c3 #x5db2 #x9723 #x29145 
     #x2921a #x4a6e #x4a76 #x97e0 #x2940a #x4ab2 #x29496 #x980b 
     #x980b #x9829 #x295b6 #x98e2 #x4b33 #x9929 #x99a7 #x99c2 
     #x99fe #x4bce #x29b30 #x9b12 #x9c40 #x9cfd #x4cce #x4ced 
     #x9d67 #x2a0ce #x4cf8 #x2a105 #x2a20e #x2a291 #x9ebb #x4d56 
     #x9ef9 #x9efe #x9f05 #x9f0f #x9f16 #x9f3b #x2a600 ))

; This bytevector contains all Unicode scalar values
; (with two bytes per scalar value, big-endian)
; that can compose with a previous scalar value
; under canonical composition.
;
; This table contains 114 elements.

(define composition-modifiers
  ;'#vu8(
  (list->bytevector '(
        #x3 #x0 #x3 #x1 #x3 #x2 #x3 #x3 
        #x3 #x4 #x3 #x6 #x3 #x7 #x3 #x8 
        #x3 #x9 #x3 #xa #x3 #xb #x3 #xc 
        #x3 #xf #x3 #x11 #x3 #x13 #x3 #x14 
        #x3 #x1b #x3 #x23 #x3 #x24 #x3 #x25 
        #x3 #x26 #x3 #x27 #x3 #x28 #x3 #x2d 
        #x3 #x2e #x3 #x30 #x3 #x31 #x3 #x38 
        #x3 #x42 #x3 #x45 #x6 #x53 #x6 #x54 
        #x6 #x55 #x9 #x3c #x9 #xbe #x9 #xd7 
        #xb #x3e #xb #x56 #xb #x57 #xb #xbe 
        #xb #xd7 #xc #x56 #xc #xc2 #xc #xd5 
        #xc #xd6 #xd #x3e #xd #x57 #xd #xca 
        #xd #xcf #xd #xdf #xf #x72 #xf #x74 
        #xf #x80 #x10 #x2e #x1b #x35 #x30 #x99 
        #x30 #x9a ))
)

; This vector encodes all canonical compositions.
; Each element corresponds to the corresponding
; element of composition-modifiers, and consists
; of a list of two bytevectors.
; The first bytevector contains the scalar values
; that, when followed by the corresponding modifier,
; compose to form the corresponding scalar value
; in the second bytevector.
;
; This table contains 57 elements.
; The bytevectors within it contain 3728 elements.

(define canonical-compositions
  `#(
     (,(list->bytevector '(
           #x0 #x41 #x0 #x45 #x0 #x49 #x0 #x4e 
           #x0 #x4f #x0 #x55 #x0 #x57 #x0 #x59 
           #x0 #x61 #x0 #x65 #x0 #x69 #x0 #x6e 
           #x0 #x6f #x0 #x75 #x0 #x77 #x0 #x79 
           #x0 #xa8 #x0 #xc2 #x0 #xca #x0 #xd4 
           #x0 #xdc #x0 #xe2 #x0 #xea #x0 #xf4 
           #x0 #xfc #x1 #x2 #x1 #x3 #x1 #x12 
           #x1 #x13 #x1 #x4c #x1 #x4d #x1 #xa0 
           #x1 #xa1 #x1 #xaf #x1 #xb0 #x3 #x91 
           #x3 #x95 #x3 #x97 #x3 #x99 #x3 #x9f 
           #x3 #xa5 #x3 #xa9 #x3 #xb1 #x3 #xb5 
           #x3 #xb7 #x3 #xb9 #x3 #xbf #x3 #xc5 
           #x3 #xc9 #x3 #xca #x3 #xcb #x4 #x15 
           #x4 #x18 #x4 #x35 #x4 #x38 #x1f #x0 
           #x1f #x1 #x1f #x8 #x1f #x9 #x1f #x10 
           #x1f #x11 #x1f #x18 #x1f #x19 #x1f #x20 
           #x1f #x21 #x1f #x28 #x1f #x29 #x1f #x30 
           #x1f #x31 #x1f #x38 #x1f #x39 #x1f #x40 
           #x1f #x41 #x1f #x48 #x1f #x49 #x1f #x50 
           #x1f #x51 #x1f #x59 #x1f #x60 #x1f #x61 
           #x1f #x68 #x1f #x69 #x1f #xbf #x1f #xfe ))
      ,(list->bytevector '(
           #x0 #xc0 #x0 #xc8 #x0 #xcc #x1 #xf8 
           #x0 #xd2 #x0 #xd9 #x1e #x80 #x1e #xf2 
           #x0 #xe0 #x0 #xe8 #x0 #xec #x1 #xf9 
           #x0 #xf2 #x0 #xf9 #x1e #x81 #x1e #xf3 
           #x1f #xed #x1e #xa6 #x1e #xc0 #x1e #xd2 
           #x1 #xdb #x1e #xa7 #x1e #xc1 #x1e #xd3 
           #x1 #xdc #x1e #xb0 #x1e #xb1 #x1e #x14 
           #x1e #x15 #x1e #x50 #x1e #x51 #x1e #xdc 
           #x1e #xdd #x1e #xea #x1e #xeb #x1f #xba 
           #x1f #xc8 #x1f #xca #x1f #xda #x1f #xf8 
           #x1f #xea #x1f #xfa #x1f #x70 #x1f #x72 
           #x1f #x74 #x1f #x76 #x1f #x78 #x1f #x7a 
           #x1f #x7c #x1f #xd2 #x1f #xe2 #x4 #x0 
           #x4 #xd #x4 #x50 #x4 #x5d #x1f #x2 
           #x1f #x3 #x1f #xa #x1f #xb #x1f #x12 
           #x1f #x13 #x1f #x1a #x1f #x1b #x1f #x22 
           #x1f #x23 #x1f #x2a #x1f #x2b #x1f #x32 
           #x1f #x33 #x1f #x3a #x1f #x3b #x1f #x42 
           #x1f #x43 #x1f #x4a #x1f #x4b #x1f #x52 
           #x1f #x53 #x1f #x5b #x1f #x62 #x1f #x63 
           #x1f #x6a #x1f #x6b #x1f #xcd #x1f #xdd )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x43 #x0 #x45 #x0 #x47 
           #x0 #x49 #x0 #x4b #x0 #x4c #x0 #x4d 
           #x0 #x4e #x0 #x4f #x0 #x50 #x0 #x52 
           #x0 #x53 #x0 #x55 #x0 #x57 #x0 #x59 
           #x0 #x5a #x0 #x61 #x0 #x63 #x0 #x65 
           #x0 #x67 #x0 #x69 #x0 #x6b #x0 #x6c 
           #x0 #x6d #x0 #x6e #x0 #x6f #x0 #x70 
           #x0 #x72 #x0 #x73 #x0 #x75 #x0 #x77 
           #x0 #x79 #x0 #x7a #x0 #xa8 #x0 #xc2 
           #x0 #xc5 #x0 #xc6 #x0 #xc7 #x0 #xca 
           #x0 #xcf #x0 #xd4 #x0 #xd5 #x0 #xd8 
           #x0 #xdc #x0 #xe2 #x0 #xe5 #x0 #xe6 
           #x0 #xe7 #x0 #xea #x0 #xef #x0 #xf4 
           #x0 #xf5 #x0 #xf8 #x0 #xfc #x1 #x2 
           #x1 #x3 #x1 #x12 #x1 #x13 #x1 #x4c 
           #x1 #x4d #x1 #x68 #x1 #x69 #x1 #xa0 
           #x1 #xa1 #x1 #xaf #x1 #xb0 #x3 #x8 
           #x3 #x91 #x3 #x95 #x3 #x97 #x3 #x99 
           #x3 #x9f #x3 #xa5 #x3 #xa9 #x3 #xb1 
           #x3 #xb5 #x3 #xb7 #x3 #xb9 #x3 #xbf 
           #x3 #xc5 #x3 #xc9 #x3 #xca #x3 #xcb 
           #x3 #xd2 #x4 #x13 #x4 #x1a #x4 #x33 
           #x4 #x3a #x1f #x0 #x1f #x1 #x1f #x8 
           #x1f #x9 #x1f #x10 #x1f #x11 #x1f #x18 
           #x1f #x19 #x1f #x20 #x1f #x21 #x1f #x28 
           #x1f #x29 #x1f #x30 #x1f #x31 #x1f #x38 
           #x1f #x39 #x1f #x40 #x1f #x41 #x1f #x48 
           #x1f #x49 #x1f #x50 #x1f #x51 #x1f #x59 
           #x1f #x60 #x1f #x61 #x1f #x68 #x1f #x69 
           #x1f #xbf #x1f #xfe ))
      ,(list->bytevector '(
           #x0 #xc1 #x1 #x6 #x0 #xc9 #x1 #xf4 
           #x0 #xcd #x1e #x30 #x1 #x39 #x1e #x3e 
           #x1 #x43 #x0 #xd3 #x1e #x54 #x1 #x54 
           #x1 #x5a #x0 #xda #x1e #x82 #x0 #xdd 
           #x1 #x79 #x0 #xe1 #x1 #x7 #x0 #xe9 
           #x1 #xf5 #x0 #xed #x1e #x31 #x1 #x3a 
           #x1e #x3f #x1 #x44 #x0 #xf3 #x1e #x55 
           #x1 #x55 #x1 #x5b #x0 #xfa #x1e #x83 
           #x0 #xfd #x1 #x7a #x3 #x85 #x1e #xa4 
           #x1 #xfa #x1 #xfc #x1e #x8 #x1e #xbe 
           #x1e #x2e #x1e #xd0 #x1e #x4c #x1 #xfe 
           #x1 #xd7 #x1e #xa5 #x1 #xfb #x1 #xfd 
           #x1e #x9 #x1e #xbf #x1e #x2f #x1e #xd1 
           #x1e #x4d #x1 #xff #x1 #xd8 #x1e #xae 
           #x1e #xaf #x1e #x16 #x1e #x17 #x1e #x52 
           #x1e #x53 #x1e #x78 #x1e #x79 #x1e #xda 
           #x1e #xdb #x1e #xe8 #x1e #xe9 #x3 #x44 
           #x3 #x86 #x3 #x88 #x3 #x89 #x3 #x8a 
           #x3 #x8c #x3 #x8e #x3 #x8f #x3 #xac 
           #x3 #xad #x3 #xae #x3 #xaf #x3 #xcc 
           #x3 #xcd #x3 #xce #x3 #x90 #x3 #xb0 
           #x3 #xd3 #x4 #x3 #x4 #xc #x4 #x53 
           #x4 #x5c #x1f #x4 #x1f #x5 #x1f #xc 
           #x1f #xd #x1f #x14 #x1f #x15 #x1f #x1c 
           #x1f #x1d #x1f #x24 #x1f #x25 #x1f #x2c 
           #x1f #x2d #x1f #x34 #x1f #x35 #x1f #x3c 
           #x1f #x3d #x1f #x44 #x1f #x45 #x1f #x4c 
           #x1f #x4d #x1f #x54 #x1f #x55 #x1f #x5d 
           #x1f #x64 #x1f #x65 #x1f #x6c #x1f #x6d 
           #x1f #xce #x1f #xde )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x43 #x0 #x45 #x0 #x47 
           #x0 #x48 #x0 #x49 #x0 #x4a #x0 #x4f 
           #x0 #x53 #x0 #x55 #x0 #x57 #x0 #x59 
           #x0 #x5a #x0 #x61 #x0 #x63 #x0 #x65 
           #x0 #x67 #x0 #x68 #x0 #x69 #x0 #x6a 
           #x0 #x6f #x0 #x73 #x0 #x75 #x0 #x77 
           #x0 #x79 #x0 #x7a #x1e #xa0 #x1e #xa1 
           #x1e #xb8 #x1e #xb9 #x1e #xcc #x1e #xcd ))
      ,(list->bytevector '(
           #x0 #xc2 #x1 #x8 #x0 #xca #x1 #x1c 
           #x1 #x24 #x0 #xce #x1 #x34 #x0 #xd4 
           #x1 #x5c #x0 #xdb #x1 #x74 #x1 #x76 
           #x1e #x90 #x0 #xe2 #x1 #x9 #x0 #xea 
           #x1 #x1d #x1 #x25 #x0 #xee #x1 #x35 
           #x0 #xf4 #x1 #x5d #x0 #xfb #x1 #x75 
           #x1 #x77 #x1e #x91 #x1e #xac #x1e #xad 
           #x1e #xc6 #x1e #xc7 #x1e #xd8 #x1e #xd9 )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x45 #x0 #x49 #x0 #x4e 
           #x0 #x4f #x0 #x55 #x0 #x56 #x0 #x59 
           #x0 #x61 #x0 #x65 #x0 #x69 #x0 #x6e 
           #x0 #x6f #x0 #x75 #x0 #x76 #x0 #x79 
           #x0 #xc2 #x0 #xca #x0 #xd4 #x0 #xe2 
           #x0 #xea #x0 #xf4 #x1 #x2 #x1 #x3 
           #x1 #xa0 #x1 #xa1 #x1 #xaf #x1 #xb0 ))
      ,(list->bytevector '(
           #x0 #xc3 #x1e #xbc #x1 #x28 #x0 #xd1 
           #x0 #xd5 #x1 #x68 #x1e #x7c #x1e #xf8 
           #x0 #xe3 #x1e #xbd #x1 #x29 #x0 #xf1 
           #x0 #xf5 #x1 #x69 #x1e #x7d #x1e #xf9 
           #x1e #xaa #x1e #xc4 #x1e #xd6 #x1e #xab 
           #x1e #xc5 #x1e #xd7 #x1e #xb4 #x1e #xb5 
           #x1e #xe0 #x1e #xe1 #x1e #xee #x1e #xef )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x45 #x0 #x47 #x0 #x49 
           #x0 #x4f #x0 #x55 #x0 #x59 #x0 #x61 
           #x0 #x65 #x0 #x67 #x0 #x69 #x0 #x6f 
           #x0 #x75 #x0 #x79 #x0 #xc4 #x0 #xc6 
           #x0 #xd5 #x0 #xd6 #x0 #xdc #x0 #xe4 
           #x0 #xe6 #x0 #xf5 #x0 #xf6 #x0 #xfc 
           #x1 #xea #x1 #xeb #x2 #x26 #x2 #x27 
           #x2 #x2e #x2 #x2f #x3 #x91 #x3 #x99 
           #x3 #xa5 #x3 #xb1 #x3 #xb9 #x3 #xc5 
           #x4 #x18 #x4 #x23 #x4 #x38 #x4 #x43 
           #x1e #x36 #x1e #x37 #x1e #x5a #x1e #x5b ))
      ,(list->bytevector '(
           #x1 #x0 #x1 #x12 #x1e #x20 #x1 #x2a 
           #x1 #x4c #x1 #x6a #x2 #x32 #x1 #x1 
           #x1 #x13 #x1e #x21 #x1 #x2b #x1 #x4d 
           #x1 #x6b #x2 #x33 #x1 #xde #x1 #xe2 
           #x2 #x2c #x2 #x2a #x1 #xd5 #x1 #xdf 
           #x1 #xe3 #x2 #x2d #x2 #x2b #x1 #xd6 
           #x1 #xec #x1 #xed #x1 #xe0 #x1 #xe1 
           #x2 #x30 #x2 #x31 #x1f #xb9 #x1f #xd9 
           #x1f #xe9 #x1f #xb1 #x1f #xd1 #x1f #xe1 
           #x4 #xe2 #x4 #xee #x4 #xe3 #x4 #xef 
           #x1e #x38 #x1e #x39 #x1e #x5c #x1e #x5d )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x45 #x0 #x47 #x0 #x49 
           #x0 #x4f #x0 #x55 #x0 #x61 #x0 #x65 
           #x0 #x67 #x0 #x69 #x0 #x6f #x0 #x75 
           #x2 #x28 #x2 #x29 #x3 #x91 #x3 #x99 
           #x3 #xa5 #x3 #xb1 #x3 #xb9 #x3 #xc5 
           #x4 #x10 #x4 #x15 #x4 #x16 #x4 #x18 
           #x4 #x23 #x4 #x30 #x4 #x35 #x4 #x36 
           #x4 #x38 #x4 #x43 #x1e #xa0 #x1e #xa1 ))
      ,(list->bytevector '(
           #x1 #x2 #x1 #x14 #x1 #x1e #x1 #x2c 
           #x1 #x4e #x1 #x6c #x1 #x3 #x1 #x15 
           #x1 #x1f #x1 #x2d #x1 #x4f #x1 #x6d 
           #x1e #x1c #x1e #x1d #x1f #xb8 #x1f #xd8 
           #x1f #xe8 #x1f #xb0 #x1f #xd0 #x1f #xe0 
           #x4 #xd0 #x4 #xd6 #x4 #xc1 #x4 #x19 
           #x4 #xe #x4 #xd1 #x4 #xd7 #x4 #xc2 
           #x4 #x39 #x4 #x5e #x1e #xb6 #x1e #xb7 )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x42 #x0 #x43 #x0 #x44 
           #x0 #x45 #x0 #x46 #x0 #x47 #x0 #x48 
           #x0 #x49 #x0 #x4d #x0 #x4e #x0 #x4f 
           #x0 #x50 #x0 #x52 #x0 #x53 #x0 #x54 
           #x0 #x57 #x0 #x58 #x0 #x59 #x0 #x5a 
           #x0 #x61 #x0 #x62 #x0 #x63 #x0 #x64 
           #x0 #x65 #x0 #x66 #x0 #x67 #x0 #x68 
           #x0 #x6d #x0 #x6e #x0 #x6f #x0 #x70 
           #x0 #x72 #x0 #x73 #x0 #x74 #x0 #x77 
           #x0 #x78 #x0 #x79 #x0 #x7a #x1 #x5a 
           #x1 #x5b #x1 #x60 #x1 #x61 #x1 #x7f 
           #x1e #x62 #x1e #x63 ))
      ,(list->bytevector '(
           #x2 #x26 #x1e #x2 #x1 #xa #x1e #xa 
           #x1 #x16 #x1e #x1e #x1 #x20 #x1e #x22 
           #x1 #x30 #x1e #x40 #x1e #x44 #x2 #x2e 
           #x1e #x56 #x1e #x58 #x1e #x60 #x1e #x6a 
           #x1e #x86 #x1e #x8a #x1e #x8e #x1 #x7b 
           #x2 #x27 #x1e #x3 #x1 #xb #x1e #xb 
           #x1 #x17 #x1e #x1f #x1 #x21 #x1e #x23 
           #x1e #x41 #x1e #x45 #x2 #x2f #x1e #x57 
           #x1e #x59 #x1e #x61 #x1e #x6b #x1e #x87 
           #x1e #x8b #x1e #x8f #x1 #x7c #x1e #x64 
           #x1e #x65 #x1e #x66 #x1e #x67 #x1e #x9b 
           #x1e #x68 #x1e #x69 )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x45 #x0 #x48 #x0 #x49 
           #x0 #x4f #x0 #x55 #x0 #x57 #x0 #x58 
           #x0 #x59 #x0 #x61 #x0 #x65 #x0 #x68 
           #x0 #x69 #x0 #x6f #x0 #x74 #x0 #x75 
           #x0 #x77 #x0 #x78 #x0 #x79 #x0 #xd5 
           #x0 #xf5 #x1 #x6a #x1 #x6b #x3 #x99 
           #x3 #xa5 #x3 #xb9 #x3 #xc5 #x3 #xd2 
           #x4 #x6 #x4 #x10 #x4 #x15 #x4 #x16 
           #x4 #x17 #x4 #x18 #x4 #x1e #x4 #x23 
           #x4 #x27 #x4 #x2b #x4 #x2d #x4 #x30 
           #x4 #x35 #x4 #x36 #x4 #x37 #x4 #x38 
           #x4 #x3e #x4 #x43 #x4 #x47 #x4 #x4b 
           #x4 #x4d #x4 #x56 #x4 #xd8 #x4 #xd9 
           #x4 #xe8 #x4 #xe9 ))
      ,(list->bytevector '(
           #x0 #xc4 #x0 #xcb #x1e #x26 #x0 #xcf 
           #x0 #xd6 #x0 #xdc #x1e #x84 #x1e #x8c 
           #x1 #x78 #x0 #xe4 #x0 #xeb #x1e #x27 
           #x0 #xef #x0 #xf6 #x1e #x97 #x0 #xfc 
           #x1e #x85 #x1e #x8d #x0 #xff #x1e #x4e 
           #x1e #x4f #x1e #x7a #x1e #x7b #x3 #xaa 
           #x3 #xab #x3 #xca #x3 #xcb #x3 #xd4 
           #x4 #x7 #x4 #xd2 #x4 #x1 #x4 #xdc 
           #x4 #xde #x4 #xe4 #x4 #xe6 #x4 #xf0 
           #x4 #xf4 #x4 #xf8 #x4 #xec #x4 #xd3 
           #x4 #x51 #x4 #xdd #x4 #xdf #x4 #xe5 
           #x4 #xe7 #x4 #xf1 #x4 #xf5 #x4 #xf9 
           #x4 #xed #x4 #x57 #x4 #xda #x4 #xdb 
           #x4 #xea #x4 #xeb )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x45 #x0 #x49 #x0 #x4f 
           #x0 #x55 #x0 #x59 #x0 #x61 #x0 #x65 
           #x0 #x69 #x0 #x6f #x0 #x75 #x0 #x79 
           #x0 #xc2 #x0 #xca #x0 #xd4 #x0 #xe2 
           #x0 #xea #x0 #xf4 #x1 #x2 #x1 #x3 
           #x1 #xa0 #x1 #xa1 #x1 #xaf #x1 #xb0 ))
      ,(list->bytevector '(
           #x1e #xa2 #x1e #xba #x1e #xc8 #x1e #xce 
           #x1e #xe6 #x1e #xf6 #x1e #xa3 #x1e #xbb 
           #x1e #xc9 #x1e #xcf #x1e #xe7 #x1e #xf7 
           #x1e #xa8 #x1e #xc2 #x1e #xd4 #x1e #xa9 
           #x1e #xc3 #x1e #xd5 #x1e #xb2 #x1e #xb3 
           #x1e #xde #x1e #xdf #x1e #xec #x1e #xed )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x55 #x0 #x61 #x0 #x75 
           #x0 #x77 #x0 #x79 ))
      ,(list->bytevector '(
           #x0 #xc5 #x1 #x6e #x0 #xe5 #x1 #x6f 
           #x1e #x98 #x1e #x99 )))
     (,(list->bytevector '(
           #x0 #x4f #x0 #x55 #x0 #x6f #x0 #x75 
           #x4 #x23 #x4 #x43 ))
      ,(list->bytevector '(
           #x1 #x50 #x1 #x70 #x1 #x51 #x1 #x71 
           #x4 #xf2 #x4 #xf3 )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x43 #x0 #x44 #x0 #x45 
           #x0 #x47 #x0 #x48 #x0 #x49 #x0 #x4b 
           #x0 #x4c #x0 #x4e #x0 #x4f #x0 #x52 
           #x0 #x53 #x0 #x54 #x0 #x55 #x0 #x5a 
           #x0 #x61 #x0 #x63 #x0 #x64 #x0 #x65 
           #x0 #x67 #x0 #x68 #x0 #x69 #x0 #x6a 
           #x0 #x6b #x0 #x6c #x0 #x6e #x0 #x6f 
           #x0 #x72 #x0 #x73 #x0 #x74 #x0 #x75 
           #x0 #x7a #x0 #xdc #x0 #xfc #x1 #xb7 
           #x2 #x92 ))
      ,(list->bytevector '(
           #x1 #xcd #x1 #xc #x1 #xe #x1 #x1a 
           #x1 #xe6 #x2 #x1e #x1 #xcf #x1 #xe8 
           #x1 #x3d #x1 #x47 #x1 #xd1 #x1 #x58 
           #x1 #x60 #x1 #x64 #x1 #xd3 #x1 #x7d 
           #x1 #xce #x1 #xd #x1 #xf #x1 #x1b 
           #x1 #xe7 #x2 #x1f #x1 #xd0 #x1 #xf0 
           #x1 #xe9 #x1 #x3e #x1 #x48 #x1 #xd2 
           #x1 #x59 #x1 #x61 #x1 #x65 #x1 #xd4 
           #x1 #x7e #x1 #xd9 #x1 #xda #x1 #xee 
           #x1 #xef )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x45 #x0 #x49 #x0 #x4f 
           #x0 #x52 #x0 #x55 #x0 #x61 #x0 #x65 
           #x0 #x69 #x0 #x6f #x0 #x72 #x0 #x75 
           #x4 #x74 #x4 #x75 ))
      ,(list->bytevector '(
           #x2 #x0 #x2 #x4 #x2 #x8 #x2 #xc 
           #x2 #x10 #x2 #x14 #x2 #x1 #x2 #x5 
           #x2 #x9 #x2 #xd #x2 #x11 #x2 #x15 
           #x4 #x76 #x4 #x77 )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x45 #x0 #x49 #x0 #x4f 
           #x0 #x52 #x0 #x55 #x0 #x61 #x0 #x65 
           #x0 #x69 #x0 #x6f #x0 #x72 #x0 #x75 ))
      ,(list->bytevector '(
           #x2 #x2 #x2 #x6 #x2 #xa #x2 #xe 
           #x2 #x12 #x2 #x16 #x2 #x3 #x2 #x7 
           #x2 #xb #x2 #xf #x2 #x13 #x2 #x17 )))
     (,(list->bytevector '(
           #x3 #x91 #x3 #x95 #x3 #x97 #x3 #x99 
           #x3 #x9f #x3 #xa9 #x3 #xb1 #x3 #xb5 
           #x3 #xb7 #x3 #xb9 #x3 #xbf #x3 #xc1 
           #x3 #xc5 #x3 #xc9 ))
      ,(list->bytevector '(
           #x1f #x8 #x1f #x18 #x1f #x28 #x1f #x38 
           #x1f #x48 #x1f #x68 #x1f #x0 #x1f #x10 
           #x1f #x20 #x1f #x30 #x1f #x40 #x1f #xe4 
           #x1f #x50 #x1f #x60 )))
     (,(list->bytevector '(
           #x3 #x91 #x3 #x95 #x3 #x97 #x3 #x99 
           #x3 #x9f #x3 #xa1 #x3 #xa5 #x3 #xa9 
           #x3 #xb1 #x3 #xb5 #x3 #xb7 #x3 #xb9 
           #x3 #xbf #x3 #xc1 #x3 #xc5 #x3 #xc9 ))
      ,(list->bytevector '(
           #x1f #x9 #x1f #x19 #x1f #x29 #x1f #x39 
           #x1f #x49 #x1f #xec #x1f #x59 #x1f #x69 
           #x1f #x1 #x1f #x11 #x1f #x21 #x1f #x31 
           #x1f #x41 #x1f #xe5 #x1f #x51 #x1f #x61 )))
     (,(list->bytevector '(
           #x0 #x4f #x0 #x55 #x0 #x6f #x0 #x75 ))
      ,(list->bytevector '(
           #x1 #xa0 #x1 #xaf #x1 #xa1 #x1 #xb0 )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x42 #x0 #x44 #x0 #x45 
           #x0 #x48 #x0 #x49 #x0 #x4b #x0 #x4c 
           #x0 #x4d #x0 #x4e #x0 #x4f #x0 #x52 
           #x0 #x53 #x0 #x54 #x0 #x55 #x0 #x56 
           #x0 #x57 #x0 #x59 #x0 #x5a #x0 #x61 
           #x0 #x62 #x0 #x64 #x0 #x65 #x0 #x68 
           #x0 #x69 #x0 #x6b #x0 #x6c #x0 #x6d 
           #x0 #x6e #x0 #x6f #x0 #x72 #x0 #x73 
           #x0 #x74 #x0 #x75 #x0 #x76 #x0 #x77 
           #x0 #x79 #x0 #x7a #x1 #xa0 #x1 #xa1 
           #x1 #xaf #x1 #xb0 ))
      ,(list->bytevector '(
           #x1e #xa0 #x1e #x4 #x1e #xc #x1e #xb8 
           #x1e #x24 #x1e #xca #x1e #x32 #x1e #x36 
           #x1e #x42 #x1e #x46 #x1e #xcc #x1e #x5a 
           #x1e #x62 #x1e #x6c #x1e #xe4 #x1e #x7e 
           #x1e #x88 #x1e #xf4 #x1e #x92 #x1e #xa1 
           #x1e #x5 #x1e #xd #x1e #xb9 #x1e #x25 
           #x1e #xcb #x1e #x33 #x1e #x37 #x1e #x43 
           #x1e #x47 #x1e #xcd #x1e #x5b #x1e #x63 
           #x1e #x6d #x1e #xe5 #x1e #x7f #x1e #x89 
           #x1e #xf5 #x1e #x93 #x1e #xe2 #x1e #xe3 
           #x1e #xf0 #x1e #xf1 )))
     (,(list->bytevector '(
           #x0 #x55 #x0 #x75 ))
      ,(list->bytevector '(
           #x1e #x72 #x1e #x73 )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x61 ))
      ,(list->bytevector '(
           #x1e #x0 #x1e #x1 )))
     (,(list->bytevector '(
           #x0 #x53 #x0 #x54 #x0 #x73 #x0 #x74 ))
      ,(list->bytevector '(
           #x2 #x18 #x2 #x1a #x2 #x19 #x2 #x1b )))
     (,(list->bytevector '(
           #x0 #x43 #x0 #x44 #x0 #x45 #x0 #x47 
           #x0 #x48 #x0 #x4b #x0 #x4c #x0 #x4e 
           #x0 #x52 #x0 #x53 #x0 #x54 #x0 #x63 
           #x0 #x64 #x0 #x65 #x0 #x67 #x0 #x68 
           #x0 #x6b #x0 #x6c #x0 #x6e #x0 #x72 
           #x0 #x73 #x0 #x74 ))
      ,(list->bytevector '(
           #x0 #xc7 #x1e #x10 #x2 #x28 #x1 #x22 
           #x1e #x28 #x1 #x36 #x1 #x3b #x1 #x45 
           #x1 #x56 #x1 #x5e #x1 #x62 #x0 #xe7 
           #x1e #x11 #x2 #x29 #x1 #x23 #x1e #x29 
           #x1 #x37 #x1 #x3c #x1 #x46 #x1 #x57 
           #x1 #x5f #x1 #x63 )))
     (,(list->bytevector '(
           #x0 #x41 #x0 #x45 #x0 #x49 #x0 #x4f 
           #x0 #x55 #x0 #x61 #x0 #x65 #x0 #x69 
           #x0 #x6f #x0 #x75 ))
      ,(list->bytevector '(
           #x1 #x4 #x1 #x18 #x1 #x2e #x1 #xea 
           #x1 #x72 #x1 #x5 #x1 #x19 #x1 #x2f 
           #x1 #xeb #x1 #x73 )))
     (,(list->bytevector '(
           #x0 #x44 #x0 #x45 #x0 #x4c #x0 #x4e 
           #x0 #x54 #x0 #x55 #x0 #x64 #x0 #x65 
           #x0 #x6c #x0 #x6e #x0 #x74 #x0 #x75 ))
      ,(list->bytevector '(
           #x1e #x12 #x1e #x18 #x1e #x3c #x1e #x4a 
           #x1e #x70 #x1e #x76 #x1e #x13 #x1e #x19 
           #x1e #x3d #x1e #x4b #x1e #x71 #x1e #x77 )))
     (,(list->bytevector '(
           #x0 #x48 #x0 #x68 ))
      ,(list->bytevector '(
           #x1e #x2a #x1e #x2b )))
     (,(list->bytevector '(
           #x0 #x45 #x0 #x49 #x0 #x55 #x0 #x65 
           #x0 #x69 #x0 #x75 ))
      ,(list->bytevector '(
           #x1e #x1a #x1e #x2c #x1e #x74 #x1e #x1b 
           #x1e #x2d #x1e #x75 )))
     (,(list->bytevector '(
           #x0 #x42 #x0 #x44 #x0 #x4b #x0 #x4c 
           #x0 #x4e #x0 #x52 #x0 #x54 #x0 #x5a 
           #x0 #x62 #x0 #x64 #x0 #x68 #x0 #x6b 
           #x0 #x6c #x0 #x6e #x0 #x72 #x0 #x74 
           #x0 #x7a ))
      ,(list->bytevector '(
           #x1e #x6 #x1e #xe #x1e #x34 #x1e #x3a 
           #x1e #x48 #x1e #x5e #x1e #x6e #x1e #x94 
           #x1e #x7 #x1e #xf #x1e #x96 #x1e #x35 
           #x1e #x3b #x1e #x49 #x1e #x5f #x1e #x6f 
           #x1e #x95 )))
     (,(list->bytevector '(
           #x0 #x3c #x0 #x3d #x0 #x3e #x21 #x90 
           #x21 #x92 #x21 #x94 #x21 #xd0 #x21 #xd2 
           #x21 #xd4 #x22 #x3 #x22 #x8 #x22 #xb 
           #x22 #x23 #x22 #x25 #x22 #x3c #x22 #x43 
           #x22 #x45 #x22 #x48 #x22 #x4d #x22 #x61 
           #x22 #x64 #x22 #x65 #x22 #x72 #x22 #x73 
           #x22 #x76 #x22 #x77 #x22 #x7a #x22 #x7b 
           #x22 #x7c #x22 #x7d #x22 #x82 #x22 #x83 
           #x22 #x86 #x22 #x87 #x22 #x91 #x22 #x92 
           #x22 #xa2 #x22 #xa8 #x22 #xa9 #x22 #xab 
           #x22 #xb2 #x22 #xb3 #x22 #xb4 #x22 #xb5 ))
      ,(list->bytevector '(
           #x22 #x6e #x22 #x60 #x22 #x6f #x21 #x9a 
           #x21 #x9b #x21 #xae #x21 #xcd #x21 #xcf 
           #x21 #xce #x22 #x4 #x22 #x9 #x22 #xc 
           #x22 #x24 #x22 #x26 #x22 #x41 #x22 #x44 
           #x22 #x47 #x22 #x49 #x22 #x6d #x22 #x62 
           #x22 #x70 #x22 #x71 #x22 #x74 #x22 #x75 
           #x22 #x78 #x22 #x79 #x22 #x80 #x22 #x81 
           #x22 #xe0 #x22 #xe1 #x22 #x84 #x22 #x85 
           #x22 #x88 #x22 #x89 #x22 #xe2 #x22 #xe3 
           #x22 #xac #x22 #xad #x22 #xae #x22 #xaf 
           #x22 #xea #x22 #xeb #x22 #xec #x22 #xed )))
     (,(list->bytevector '(
           #x0 #xa8 #x3 #xb1 #x3 #xb7 #x3 #xb9 
           #x3 #xc5 #x3 #xc9 #x3 #xca #x3 #xcb 
           #x1f #x0 #x1f #x1 #x1f #x8 #x1f #x9 
           #x1f #x20 #x1f #x21 #x1f #x28 #x1f #x29 
           #x1f #x30 #x1f #x31 #x1f #x38 #x1f #x39 
           #x1f #x50 #x1f #x51 #x1f #x59 #x1f #x60 
           #x1f #x61 #x1f #x68 #x1f #x69 #x1f #xbf 
           #x1f #xfe ))
      ,(list->bytevector '(
           #x1f #xc1 #x1f #xb6 #x1f #xc6 #x1f #xd6 
           #x1f #xe6 #x1f #xf6 #x1f #xd7 #x1f #xe7 
           #x1f #x6 #x1f #x7 #x1f #xe #x1f #xf 
           #x1f #x26 #x1f #x27 #x1f #x2e #x1f #x2f 
           #x1f #x36 #x1f #x37 #x1f #x3e #x1f #x3f 
           #x1f #x56 #x1f #x57 #x1f #x5f #x1f #x66 
           #x1f #x67 #x1f #x6e #x1f #x6f #x1f #xcf 
           #x1f #xdf )))
     (,(list->bytevector '(
           #x3 #x91 #x3 #x97 #x3 #xa9 #x3 #xac 
           #x3 #xae #x3 #xb1 #x3 #xb7 #x3 #xc9 
           #x3 #xce #x1f #x0 #x1f #x1 #x1f #x2 
           #x1f #x3 #x1f #x4 #x1f #x5 #x1f #x6 
           #x1f #x7 #x1f #x8 #x1f #x9 #x1f #xa 
           #x1f #xb #x1f #xc #x1f #xd #x1f #xe 
           #x1f #xf #x1f #x20 #x1f #x21 #x1f #x22 
           #x1f #x23 #x1f #x24 #x1f #x25 #x1f #x26 
           #x1f #x27 #x1f #x28 #x1f #x29 #x1f #x2a 
           #x1f #x2b #x1f #x2c #x1f #x2d #x1f #x2e 
           #x1f #x2f #x1f #x60 #x1f #x61 #x1f #x62 
           #x1f #x63 #x1f #x64 #x1f #x65 #x1f #x66 
           #x1f #x67 #x1f #x68 #x1f #x69 #x1f #x6a 
           #x1f #x6b #x1f #x6c #x1f #x6d #x1f #x6e 
           #x1f #x6f #x1f #x70 #x1f #x74 #x1f #x7c 
           #x1f #xb6 #x1f #xc6 #x1f #xf6 ))
      ,(list->bytevector '(
           #x1f #xbc #x1f #xcc #x1f #xfc #x1f #xb4 
           #x1f #xc4 #x1f #xb3 #x1f #xc3 #x1f #xf3 
           #x1f #xf4 #x1f #x80 #x1f #x81 #x1f #x82 
           #x1f #x83 #x1f #x84 #x1f #x85 #x1f #x86 
           #x1f #x87 #x1f #x88 #x1f #x89 #x1f #x8a 
           #x1f #x8b #x1f #x8c #x1f #x8d #x1f #x8e 
           #x1f #x8f #x1f #x90 #x1f #x91 #x1f #x92 
           #x1f #x93 #x1f #x94 #x1f #x95 #x1f #x96 
           #x1f #x97 #x1f #x98 #x1f #x99 #x1f #x9a 
           #x1f #x9b #x1f #x9c #x1f #x9d #x1f #x9e 
           #x1f #x9f #x1f #xa0 #x1f #xa1 #x1f #xa2 
           #x1f #xa3 #x1f #xa4 #x1f #xa5 #x1f #xa6 
           #x1f #xa7 #x1f #xa8 #x1f #xa9 #x1f #xaa 
           #x1f #xab #x1f #xac #x1f #xad #x1f #xae 
           #x1f #xaf #x1f #xb2 #x1f #xc2 #x1f #xf2 
           #x1f #xb7 #x1f #xc7 #x1f #xf7 )))
     (,(list->bytevector '(
           #x6 #x27 ))
      ,(list->bytevector '(
           #x6 #x22 )))
     (,(list->bytevector '(
           #x6 #x27 #x6 #x48 #x6 #x4a #x6 #xc1 
           #x6 #xd2 #x6 #xd5 ))
      ,(list->bytevector '(
           #x6 #x23 #x6 #x24 #x6 #x26 #x6 #xc2 
           #x6 #xd3 #x6 #xc0 )))
     (,(list->bytevector '(
           #x6 #x27 ))
      ,(list->bytevector '(
           #x6 #x25 )))
     (,(list->bytevector '(
           #x9 #x28 #x9 #x30 #x9 #x33 ))
      ,(list->bytevector '(
           #x9 #x29 #x9 #x31 #x9 #x34 )))
     (,(list->bytevector '(
           #x9 #xc7 ))
      ,(list->bytevector '(
           #x9 #xcb )))
     (,(list->bytevector '(
           #x9 #xc7 ))
      ,(list->bytevector '(
           #x9 #xcc )))
     (,(list->bytevector '(
           #xb #x47 ))
      ,(list->bytevector '(
           #xb #x4b )))
     (,(list->bytevector '(
           #xb #x47 ))
      ,(list->bytevector '(
           #xb #x48 )))
     (,(list->bytevector '(
           #xb #x47 ))
      ,(list->bytevector '(
           #xb #x4c )))
     (,(list->bytevector '(
           #xb #xc6 #xb #xc7 ))
      ,(list->bytevector '(
           #xb #xca #xb #xcb )))
     (,(list->bytevector '(
           #xb #x92 #xb #xc6 ))
      ,(list->bytevector '(
           #xb #x94 #xb #xcc )))
     (,(list->bytevector '(
           #xc #x46 ))
      ,(list->bytevector '(
           #xc #x48 )))
     (,(list->bytevector '(
           #xc #xc6 ))
      ,(list->bytevector '(
           #xc #xca )))
     (,(list->bytevector '(
           #xc #xbf #xc #xc6 #xc #xca ))
      ,(list->bytevector '(
           #xc #xc0 #xc #xc7 #xc #xcb )))
     (,(list->bytevector '(
           #xc #xc6 ))
      ,(list->bytevector '(
           #xc #xc8 )))
     (,(list->bytevector '(
           #xd #x46 #xd #x47 ))
      ,(list->bytevector '(
           #xd #x4a #xd #x4b )))
     (,(list->bytevector '(
           #xd #x46 ))
      ,(list->bytevector '(
           #xd #x4c )))
     (,(list->bytevector '(
           #xd #xd9 #xd #xdc ))
      ,(list->bytevector '(
           #xd #xda #xd #xdd )))
     (,(list->bytevector '(
           #xd #xd9 ))
      ,(list->bytevector '(
           #xd #xdc )))
     (,(list->bytevector '(
           #xd #xd9 ))
      ,(list->bytevector '(
           #xd #xde )))
     (,(list->bytevector '(
           #xf #x71 ))
      ,(list->bytevector '(
           #xf #x73 )))
     (,(list->bytevector '(
           #xf #x71 ))
      ,(list->bytevector '(
           #xf #x75 )))
     (,(list->bytevector '(
           #xf #x71 ))
      ,(list->bytevector '(
           #xf #x81 )))
     (,(list->bytevector '(
           #x10 #x25 ))
      ,(list->bytevector '(
           #x10 #x26 )))
     (,(list->bytevector '(
           #x1b #x5 #x1b #x7 #x1b #x9 #x1b #xb 
           #x1b #xd #x1b #x11 #x1b #x3a #x1b #x3c 
           #x1b #x3e #x1b #x3f #x1b #x42 ))
      ,(list->bytevector '(
           #x1b #x6 #x1b #x8 #x1b #xa #x1b #xc 
           #x1b #xe #x1b #x12 #x1b #x3b #x1b #x3d 
           #x1b #x40 #x1b #x41 #x1b #x43 )))
     (,(list->bytevector '(
           #x30 #x46 #x30 #x4b #x30 #x4d #x30 #x4f 
           #x30 #x51 #x30 #x53 #x30 #x55 #x30 #x57 
           #x30 #x59 #x30 #x5b #x30 #x5d #x30 #x5f 
           #x30 #x61 #x30 #x64 #x30 #x66 #x30 #x68 
           #x30 #x6f #x30 #x72 #x30 #x75 #x30 #x78 
           #x30 #x7b #x30 #x9d #x30 #xa6 #x30 #xab 
           #x30 #xad #x30 #xaf #x30 #xb1 #x30 #xb3 
           #x30 #xb5 #x30 #xb7 #x30 #xb9 #x30 #xbb 
           #x30 #xbd #x30 #xbf #x30 #xc1 #x30 #xc4 
           #x30 #xc6 #x30 #xc8 #x30 #xcf #x30 #xd2 
           #x30 #xd5 #x30 #xd8 #x30 #xdb #x30 #xef 
           #x30 #xf0 #x30 #xf1 #x30 #xf2 #x30 #xfd ))
      ,(list->bytevector '(
           #x30 #x94 #x30 #x4c #x30 #x4e #x30 #x50 
           #x30 #x52 #x30 #x54 #x30 #x56 #x30 #x58 
           #x30 #x5a #x30 #x5c #x30 #x5e #x30 #x60 
           #x30 #x62 #x30 #x65 #x30 #x67 #x30 #x69 
           #x30 #x70 #x30 #x73 #x30 #x76 #x30 #x79 
           #x30 #x7c #x30 #x9e #x30 #xf4 #x30 #xac 
           #x30 #xae #x30 #xb0 #x30 #xb2 #x30 #xb4 
           #x30 #xb6 #x30 #xb8 #x30 #xba #x30 #xbc 
           #x30 #xbe #x30 #xc0 #x30 #xc2 #x30 #xc5 
           #x30 #xc7 #x30 #xc9 #x30 #xd0 #x30 #xd3 
           #x30 #xd6 #x30 #xd9 #x30 #xdc #x30 #xf7 
           #x30 #xf8 #x30 #xf9 #x30 #xfa #x30 #xfe )))
     (,(list->bytevector '(
           #x30 #x6f #x30 #x72 #x30 #x75 #x30 #x78 
           #x30 #x7b #x30 #xcf #x30 #xd2 #x30 #xd5 
           #x30 #xd8 #x30 #xdb ))
      ,(list->bytevector '(
           #x30 #x71 #x30 #x74 #x30 #x77 #x30 #x7a 
           #x30 #x7d #x30 #xd1 #x30 #xd4 #x30 #xd7 
           #x30 #xda #x30 #xdd )))
))

;)

; numbers.scm - Sassy's number predicates
; Copyright (C) 2005 Jonathan Kraut

; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.

; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.

; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

; Contact:
; Jonathan Kraut
; 4130 43 ST #C2
; Sunnyside, NY 11104
; jak76@columbia.edu

; see file COPYING in the top of Sassy's distribution directory


; module numbers
; import srfi-60
; import-syntax meta-lambda
; export all

; also loads "other/srfi-56-pieces.scm"

(define s-byte  #f)
(define s-word  #f)
(define s-dword #f)
(define s-qword #f)
(define u-byte  #f)
(define u-word  #f)
(define u-dword #f)
(define u-qword #f)

(let ((signed-x (lambda (bitfield)
		  (lambda (number)
		    (and (integer? number)
			 (let ((tester (logand number bitfield)))
			   (or (zero? tester) (= tester bitfield)))))))
      (unsigned-x (lambda (bitfield)
		    (lambda (number)
		      (and (integer? number)
			   (= bitfield (logior number bitfield)))))))
  (define s-byte-x  (signed-x (- (expt 2 7))))
  (define s-word-x  (signed-x (- (expt 2 15))))
  (define s-dword-x (signed-x (- (expt 2 31))))
  (define s-qword-x (signed-x (- (expt 2 63))))
  (define u-byte-x  (unsigned-x (- (expt 2 8) 1)))
  (define u-word-x  (unsigned-x (- (expt 2 16) 1)))
  (define u-dword-x (unsigned-x (- (expt 2 32) 1)))
  (define u-qword-x (unsigned-x (- (expt 2 64) 1)))
  (let ((num-x (lambda (pred key)
		 (meta-lambda
		  (or ,@pred
		      (and ,key pred))))))
    (set! s-byte  (memoize (num-x s-byte-x  'byte)))
    (set! s-word  (memoize (num-x s-word-x  'word)))
    (set! s-dword (memoize (num-x s-dword-x 'dword)))
    (set! s-qword (memoize (num-x s-qword-x 'qword)))
    (set! u-byte  (memoize (num-x u-byte-x  'byte)))
    (set! u-word  (memoize (num-x u-word-x  'word)))
    (set! u-dword (memoize (num-x u-dword-x 'dword)))
    (set! u-qword (memoize (num-x u-qword-x 'qword)))))


(define (u/s-byte  x) (or (s-byte x)  (u-byte x)))
(define (u/s-word  x) (or (s-word x)  (u-word x)))
(define (u/s-dword x) (or (s-dword x) (u-dword x) (real? x)))
(define (u/s-qword x) (or (s-qword x) (u-qword x) (real? x)))


 ; The byte-list returned is little-endian
(define (number->byte-list number size)
  (cond ((integer? number) (integer->byte-list number size))
	((real? number)
	 (cond ((= 4 size) (float32->byte-list number))
	       ((= 8 size) (float64->byte-list number))
	       (else (error "bad size for float" number size))))
	(else (error "not a number sassy can assemble" number))))


 ; The following all return little-endian byte-lists

 ; Very few scheme implementations provide something like
 ; integer->bytes or float->bytes. Those that do (including slib)
 ; return a string, so I would have write:
 ; (map char->integer (string->list (integer/float->bytes ...)))
 ; which is less efficient for sassy. So I'm using these instead...

(define (integer->byte-list orig-int orig-size)
  (let iter ((int orig-int) (size orig-size))
    (if (zero? size)
	(if (or (zero? orig-int)
		(and (positive? orig-int) (zero? int))
		(and (negative? orig-int) (= -1 int)))
	    '()
	    (error "integer too big for field width" orig-int orig-size))
	(cons (logand int 255) (iter (ash int -8) (- size 1))))))

; (load "other/srfi-56-pieces.scm")

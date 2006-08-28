; text-block.scm - an internal data type for Sassy
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


; module text-blocks
; import push-stacks srfi-9
; export all

(define-record-type text-block
  (make-text-block a b c d e f g h i j k l)
  text-block? 
  (a t-text)
  (b t-reloc t-reloc-set!)
  (c t-res   t-res-set!)
  (d t-unres t-unres-set!)
  (e t-mark  t-mark-set!)
  (f t-label t-label-set!)
  (g t-env   t-env-set!)
  (h t-win   t-win-set!)
  (i t-lose  t-lose-set!)
  (j t-outp)
  (k t-seg-flag   t-seg-flag-set!)
  (l t-addr-flag  t-addr-flag-set!))
  
     

(define (push-t-reloc! t i) (t-reloc-set! t (cons i (t-reloc t))))
(define (push-t-res!   t i) (t-res-set!   t (cons i (t-res   t))))
(define (push-t-unres! t i) (t-unres-set! t (cons i (t-unres t))))
(define (push-t-mark!  t i) (t-mark-set!  t (cons i (t-mark  t))))
(define (pop-t-mark!   t)   (let ((z (t-mark t)))
			      (if (not (null? z))
				  (begin (t-mark-set! t (cdr z))
					 (car z))
				  #f)))
(define (push-t-label! t i) (t-label-set! t (cons i (t-label t))))
(define (push-t-env! t env) (t-env-set! t (cons env (t-env t))))
			     
(define (t-make outp)
  (make-text-block (make-pushup-stack) '() '() '() '() '() '() 0 0 outp #f #f))

(define (t-save! t)
  (let ((restore-text-stack (push-stack-save (t-text t)))
	(orig-reloc (t-reloc t))
	(orig-res   (t-res t))
	(orig-unres (t-unres t))
	(orig-mark  (t-mark t))
	(orig-label (t-label t))
	(orig-env   (t-env t)))
    (lambda ()
      (restore-text-stack)
      (t-reloc-set! t orig-reloc)
      (t-res-set!   t orig-res)
      (t-unres-set! t orig-unres)
      (t-mark-set!  t orig-mark)
      (t-label-set! t orig-label)
      (t-env-set!   t orig-env))))





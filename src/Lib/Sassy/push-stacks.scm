; push-stacks.scm - A stack-like data-type
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


; module sassy-push-stacks
; export all

(define make-pushdown-stack #f)
(define make-pushup-stack   #f)

(let ((make-push-stack
       (lambda (direc)
	 (define size      0)
	 (define items    '())
	 (define pointer  '())
	 (define down-stack-base '())
	 
	 (define (cycle lst siz)
	   (do ((ls lst (cdr ls))
		(c  siz (+ c 1)))
	       ((null? (cdr ls)) (set! size c) ls)))

	 (define push-gs
	   (if (eqv? 'up direc)
	       (lambda (itm-or-pr)
		 (and (not (pair? itm-or-pr))
		      (set! itm-or-pr (list itm-or-pr)))
		 (if (null? pointer)
		     (begin (set! items itm-or-pr)
			    (set! pointer (cycle itm-or-pr (+ size 1)))
			    items)
		     (begin (set-cdr! pointer itm-or-pr)
			    (let ((old (cdr pointer)))
			      (set! pointer (cycle pointer size))
			      old))))
	       (lambda (itm-or-pr)
		 (let ((push-one (lambda ()  ; fast path for non-pairs
				   (set! items (cons itm-or-pr items))
				   (set! pointer items)
				   (set! size (+ size 1)))))
		 (if (null? pointer)
		     (if (not (pair? itm-or-pr))
			 (begin (push-one)
				(set! down-stack-base pointer)
				pointer)
			 (begin (set! items itm-or-pr)
				(set! down-stack-base
				      (cycle itm-or-pr (+ size 1)))
				(set! pointer items)
				pointer))
		     (if (not (pair? itm-or-pr))
			 (begin (push-one)
				pointer)
			 (begin (set-cdr! (cycle itm-or-pr (+ size 1)) items)
				(set! items itm-or-pr)
				(set! pointer itm-or-pr)
				pointer)))))))
	 
	 (define (patch-gs pnt lst)
	   (do ((rst lst (cdr rst))
		(loc pnt (cdr loc)))
	       ((null? rst))
	     (set-car! loc (car rst))))

	 (define previous '())

	 (define append-gs
	   (if (eqv? 'up direc)
	       (lambda (stk2)
		 (if (and #f (memq stk2 previous))
		     (error "tried to append! the same stacks twice" stk2)
		     (begin (set! previous (cons stk2 previous))
			    (set! size (+ size (stk2 'size)))
			    (if (null? pointer)
				(set! items (stk2 'items))
				(set-cdr! pointer (stk2 'items)))
			    (let ((p (if (eqv? 'up (stk2 'direc))
					 (stk2 'pointer)
					 (stk2 'down-base))))
			      (if (and (not (eq? pointer p))
				       (not (null? p)))
				  (set! pointer p))))))
	       (lambda (stk2)
		 (if (and #f (memq stk2 previous))
		     (error "tried to append! the same stacks twice" stk2)
		     (begin (set! previous (cons stk2 previous))
			    (set! size (+ size (stk2 'size)))
			    (if (null? pointer)
				(begin (set! items (stk2 'items))
				       (set! pointer items))
				(set-cdr! down-stack-base (stk2 'items)))
			    (let ((d (if (eqv? 'up (stk2 'direc))
					 (stk2 'pointer)
					 (stk2 'down-base))))
			      (if (and (not (eq? down-stack-base d))
				       (not (null? d)))
				  (set! down-stack-base d))))))))

	 (lambda args
	   (case (car args)
	     ((push) (push-gs (cadr args)))
	     ((size) size)
	     ((patch) (if (pair? (caddr args))
			  (patch-gs (cadr args) (caddr args))
			  (set-car! (cadr args) (caddr args))))
	     ((append) (append-gs (cadr args)))
	     ((set-previous) (set! previous (cons (cadr args) previous)))
	     ((pointer) pointer)
	     ((down-base) down-stack-base)
	     ((items) items)
	     ((save) (let ((os size) (op pointer) (oi items))
			     (lambda ()
			       (set! size os)
			       (set! pointer op)
			       (set! items oi)
			       (if (and (not (null? pointer)) (eqv? direc 'up))
				   (set-cdr! pointer '())))))
	     ((push-proc) (let ((t (push-gs (cadr args))))
			    (lambda (new) (patch-gs t new))))
	     ((direc) direc)
	     ((empty) (null? items)))))))


  (set! make-pushdown-stack (lambda () (make-push-stack 'up)))
  (set! make-pushup-stack   (lambda () (make-push-stack 'down))))

(define (push-stack-push stk itm) (stk 'push itm))
(define (push-stack-pointer stk) (stk 'pointer))
(define (push-stack-items stk) (stk 'items))
(define (push-stack-items/bytes stk) 
  (list->u8vector (push-stack-items stk)))
(define (push-stack-patch stk pnt itm) (stk 'patch pnt itm))
(define (push-stack-push->patcher stk itm) (stk 'push-proc itm))
(define (push-stack-save stk) (stk 'save))
(define (push-stack-direction stk) (stk 'direc))
(define (push-stack-size stk) (stk 'size))
(define (push-stack-append! stk1 stk2)
  (stk2 'set-previous stk1)
  (stk1 'append stk2))
(define (push-stack-empty? stk) (stk 'empty))
(define push-stack-align
  (let ((align-to (lambda (count align)
		    (let ((diff (modulo count align)))
		      (if (zero? diff)
			  0
			  (- align diff))))))
    (lambda (stk align fill . offset)
      (let ((amount (align-to (+ (stk 'size)
				 (if (null? offset) 0 (car offset)))
			      align)))
	(if (pair? fill)
	    (error "can not fill a push-stack with a pair" fill)
	    (when (not (zero? amount))
		  (stk 'push (make-list amount fill))))))))


; Syntax that tends to occur in SRFIs written by Olin Shivers :-)
; 2004-01-02 / lth and others, see below
;
; $Id$

(library (larceny shivers-syntax)

  (export :optional check-arg let-optionals*)

  (import (rnrs base))

(define-syntax :optional
  (syntax-rules ()
    ((:optional x default)
     (let ((x x))
       (if (pair? x)
	   (if (not (null? (cdr x)))
	       (error "Too many arguments in :OPTIONAL.")
	       (car x))
	   default)))
    ((:optional x default check)
     (let ((x x))
       (if (pair? x)
	   (cond ((not (null? (cdr x)))
		  (error "Too many arguments in :OPTIONAL."))
		 ((not (check (car x)))
		  (error "Value in :OPTIONAL does not check out OK: " (car x)))
		 (else
		  (car x)))
	   default)))))

(define-syntax check-arg
  (syntax-rules ()
    ((check-arg pred val caller) val)))


; From: "Anton van Straaten" <anton@appsolutions.com>
; Subject: Re: sow to specify default values for optional parameters
; Newsgroups: comp.lang.scheme
; Date: Thu, 31 Jul 2003 16:15:33 GMT
;
; Supposedly from the reference implementation of SRFI 43.
; Superseded by code below.

; (define-syntax let-optionals*
;   (syntax-rules ()
;     ((_ ?rest () ?e1 ?e2 ...)
;      (begin ?e1 ?e2 ...))
;     ((_ ?rest ((?var ?default) . ?more) ?e1 ?e2 ...)
;      (let* ((rest ?rest)
;             (?var (if (null? rest) ?default (car rest)))
;             (next-rest (if (null? rest) '() (cdr rest))))
;        (let-optionals* next-rest ?more ?e1 ?e2 ...)))))


;;; Time-stamp: <02/07/15 09:25:27 solsona>
;;;
;;; Usually, I would add a copyright notice, and the announce that
;;; this code is under the LGPL licence.  This code is been copied
;;; from Scheme48, and Scsh, and thus I will append here their
;;; Copyright notice:
;;;
;;; Copyright (c) 1993-1999 Richard Kelsey and Jonathan Rees
;;; Copyright (c) 1994-1999 by Olin Shivers and Brian D. Carlstrom.
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;
;;; Author: Francisco Solsona <solsona@acm.org>

;; NOTE: This is the `less-efficient version of LET-OPTIONALS*'.
;;       Once I understand the more efficient one, as for to
;;       adapt it to PLT Scheme, I will.  Sorry, Olin Shivers,
;;       wrote a far to complex thing for me to grasp. :-{

;; 	(LET-OPTIONALS* arg-list (opt-clause1 ... opt-clauseN [rest])
;;       body ...)
;; where
;;     <opt-clause> ::= (var default [arg-check supplied?])
;;                  |   ((var1 ... varN) external-arg-parser)
;;
;; LET-OPTIONALS* has LET* scope -- each arg clause sees the bindings of
;; the previous clauses. LET-OPTIONALS has LET scope -- each arg clause
;; sees the outer scope (an ARG-CHECK expression sees the outer scope
;; *plus* the variable being bound by that clause, by necessity).

(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* arg (opt-clause ...) body ...)
     (let ((rest arg))
       (%let-optionals* rest (opt-clause ...) body ...)))))

(define-syntax %let-optionals*
  (syntax-rules ()
    ((%let-optionals* arg (((var ...) xparser) opt-clause ...) body ...)
     (call-with-values (lambda () (xparser arg))
       (lambda (rest var ...)
	 (%let-optionals* rest (opt-clause ...) body ...))))
			
    ((%let-optionals* arg ((var default) opt-clause ...) body ...)
     (call-with-values (lambda () (if (null? arg) (values default '())
				      (values (car arg) (cdr arg))))
       (lambda (var rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test) opt-clause ...) body ...)
     (call-with-values (lambda ()
			 (if (null? arg) (values default '())
			     (let ((var (car arg)))
			       (if test (values var (cdr arg))
				   (error "arg failed LET-OPT test" var)))))
       (lambda (var rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test supplied?) opt-clause ...) body ...)
     (call-with-values (lambda ()
			 (if (null? arg) (values default #f '())
			     (let ((var (car arg)))
			       (if test (values var #t (cdr arg))
				   (error "arg failed LET-OPT test" var)))))
       (lambda (var supplied? rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg (rest) body ...)
     (let ((rest arg)) body ...))

    ((%let-optionals* arg () body ...)
     (if (null? arg) (begin body ...)
	 (error "Too many arguments in let-opt" arg)))))

; end LET-OPTIONALS* code

)

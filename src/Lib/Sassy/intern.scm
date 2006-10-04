; intern.scm - private api functions for Sassy
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


; module intern
; import api push-stacks srfi-69
; export all




; Looks up symbol-name (a scheme symbol) in the symbol-table of
; sassy-output. If no record exists for that name, it creates a fresh
; one, in the table. Then for each item in the list of field-value
; pairs, it sets the corresponding field of the sassy-symbol to the
; value (or in the case of the 'unres field, adds the value to the
; list stored there). The pairs must be proper lists. The result is
; the sassy-symbol that was modified.

; Anytime a new offset is given via the field-pair '(offset <value>),
; all the back-patchers stored in the unres field of the sassy-symbol are
; applied to the <value>.
(define (sassy-symbol-set! sassy-output symbol-name . list-of-field-pairs)
  (let ((exists (sassy-symbol-exists-env? sassy-output symbol-name)))
    (when (not exists)
	  (set! exists (make-sassy-symbol symbol-name 'local #f #f #f '() #f))
	  (let iter ((t (sassy-symbol-table sassy-output)))
	    (if (hash-table? (car t))
		(hash-table-set! (car t) symbol-name exists)
		(iter (cdr t)))))
    (for-each
     (lambda (field-pair)
       (case (car field-pair)
	 ((name)    (sassy-symbol-name-set!    exists (cadr field-pair)))
	 ((scope)   (sassy-symbol-scope-set!   exists (cadr field-pair)))
	 ((section) (sassy-symbol-section-set! exists (cadr field-pair)))
	 ((size)    (sassy-symbol-size-set!    exists (cadr field-pair)))
	 ((offset)
	  (sassy-symbol-offset-set! exists (cadr field-pair))
	  (for-each (lambda (back-patcher)
		      (back-patcher (cadr field-pair)
				    (sassy-symbol-section exists)))
		    (sassy-symbol-unres exists)))
	 ((unres)
	  (sassy-symbol-unres-set!
	   exists (cons (cadr field-pair) (sassy-symbol-unres exists))))
	 ((exp) (sassy-symbol-exp-set! exists (cadr field-pair)))))
     list-of-field-pairs)
    exists))

(define (sassy-symbol-ensure sassy-output symbol-name)
  (or (sassy-symbol-exists-env? sassy-output symbol-name)
      (let ((new (make-sassy-symbol symbol-name 'local #f #f #f '() #f)))
	(let iter ((t (sassy-symbol-table sassy-output)))
	  (if (hash-table? (car t))
	      (begin (hash-table-set! (car t) symbol-name new)
		     new)
	      (iter (cdr t)))))))

; fast path cases used internally
; instead blah-foo-set! these are all blah-set-foo!
(define (sassy-symbol-set-scope! sassy-output name scope)
  (let ((sym (sassy-symbol-ensure sassy-output name)))
    (sassy-symbol-scope-set! sym scope)
    sym))

(define (sassy-symbol-set-sect-off! so name sect off)
  (let ((sym (sassy-symbol-ensure so name)))
    (sassy-symbol-section-set! sym sect)
    (sassy-symbol-offset-set! sym off)
    (for-each (lambda (back-patcher)
		(back-patcher off (sassy-symbol-section sym)))
	      (sassy-symbol-unres sym))
    sym))

(define (sassy-symbol-set-off! so name off)
  (let ((sym (sassy-symbol-ensure so name)))
    (sassy-symbol-offset-set! sym off)
    (for-each (lambda (back-patcher)
		(back-patcher off (sassy-symbol-section sym)))
	      (sassy-symbol-unres sym))
    sym))

(define (sassy-symbol-set-size! so name size)
  (let ((sym (sassy-symbol-ensure so name)))
    (sassy-symbol-size-set! sym size)
    sym))

(define (sassy-symbol-set-unres! so name unres)
  (let ((sym (sassy-symbol-ensure so name)))
    (sassy-symbol-unres-set! sym (cons unres (sassy-symbol-unres sym)))
    sym))

(define (sassy-symbol-set-sect! so name sect)
  (let ((sym (sassy-symbol-ensure so name)))
    (sassy-symbol-section-set! sym sect)
    sym))

  

(define sassy-symbol-exists-env? sassy-symbol-exists?)


; (define (sassy-symbol-exists-env? sassy-output name)
;   (let iter ((rst (sassy-symbol-table sassy-output)))
;     (cond ((hash-table? (car rst))
; 	   (hash-table-ref (car rst) name (lambda () #f)))
; 	  ((eq? name (sassy-symbol-name (car rst))) (car rst))
; 	  (else (iter (cdr rst))))))

(define (sassy-symbol-defined? sassy-output name)
  (let ((maybe (sassy-symbol-exists-env? sassy-output name)))
    (cond ((not maybe) #f)
	  ((eq? 'import (sassy-symbol-scope maybe)) #t)
	  ((sassy-symbol-offset maybe) #t)
	  (else #f))))

(define (sassy-symbol-def-error sassy-output name)
  (or (not (sassy-symbol-defined? sassy-output name))
      (error "re-definition of a previously defined/imported symbol" name)))

(define new-block
  (let ((c 0))
    (lambda () ; should use native gensym
      (let ((n (string->symbol (string-append "%!%!%!block" 
					      (number->string c)))))
	(set! c (+ c 1))
	n))))

; extra-proc is a proc of one argument that does something with each
; new sassy-symbol record, or #f
(define (setup-locals locals outp extra-proc)
  (let* ((newb (new-block))
	 (old-env (sassy-symbol-table outp))
	 (restore! (lambda ()
		     (sassy-symbol-table-set! outp old-env))))
    (sassy-symbol-table-set!
     outp
     (let iter ((rest locals))
       (if (null? rest)
	   old-env
	   (let ((new-sym (make-sassy-symbol
			   (valid-label (car rest)) newb #f #f #f '() #f)))
	     (if extra-proc
		 (extra-proc new-sym))
	     (cons new-sym (iter (cdr rest)))))))
    restore!))

(define (quoted-label x)
  (and (pair? x)
       (eq? 'quote (car x))
       (let ((x (cdr x)))
	  (and (pair? x)
	       (null? (cdr x))
	       (let ((x (car x)))
		 (and (symbol? x) x))))))

(define valid-label0
  (let ((keywords '(seq begin inv if iter while with-win
			with-lose with-win-lose esc
			mark leap label)))
    (lambda (x)
      (cond ((and (symbol? x) (not (member x keywords))) x)
	    ((quoted-label x))
	    (else #f)))))

(define (valid-label x)
  (or (valid-label0 x)
      (error "sassy: invalid label" x)))

(define (get-reloc-target target outp)
  (if (symbol? target)
      (let ((s (sassy-symbol-exists-env? outp target)))
	(if s
	    (case (sassy-symbol-scope s)
	      ((local import export) target)
	      (else #f))
	    target))
      #f))

(define (get-reloc-target-sect target outp current)
  (if (symbol? target)
      (let ((s (sassy-symbol-exists-env? outp target)))
	(and s (sassy-symbol-section s)))
       current))

(define (check-label-size size cur-byte-size key label)
  (if (not (= size cur-byte-size))
      (error
       "sassy: wrong data size for label or custom reloc under "
       `(bits ,(* 8 cur-byte-size)) (list key label))))

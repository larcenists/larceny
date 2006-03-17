; parse.scm - Sassy's top level parser
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


; module parse
; import macros api text numbers opcodes push-stacks operands intern
; import-syntax meta-lambda
; export all

(define parse-directives
  
  (let ()

    (define (process-bits int output)
      (if (or (= 16 int) (= 32 int))
	  (sassy-bits-set! output int)
	  (error "sassy: bad bits" int)))
    
    (define (process-org text-base output)
      (if (and (integer? text-base)
	       (zero? (sassy-text-size output))
	       (positive? text-base))
	  (sassy-text-org-set!  output text-base)
	  (error "sassy: bad org" text-base)))
    
    (define (process-entry entry-label output)
      (if (symbol? entry-label)
	  (begin (sassy-symbol-set! output entry-label '(scope export))
		 (sassy-entry-point-set! output entry-label))
	  (error "sassy: bad entry" entry-label)))

    (define (process-include include-list output)
      (for-each
       (lambda (x)
	 (cond ((string? x) (parse-directives (read-file x) output))
	       ((symbol? x) (parse-directives (eval x
						    (interaction-environment))
					      output))
	       (else (error "sassy: bad include" x))))
       include-list))
  
    (define (process-scopes scope-list scope output)
      (for-each (lambda (x)
		  (if (eq? 'import scope)
		      (sassy-symbol-def-error output x))
		  (if (symbol? x)
		      (sassy-symbol-set! output x `(scope ,scope))
		      (error "sassy: bad scope" scope x)))
		scope-list))

    (define (align-to count align)
      (let ((diff (modulo count align)))
	(if (zero? diff)
	    0
	    (- align diff))))

    (define aligner
      (let ((power-of-2 (lambda (x)
			  (and (integer? x) (positive? x)
			       (zero? (logand x (- x 1)))
			       x))))
	(meta-lambda
	 (and 'align power-of-2))))

    (define (process-heap heap-list output)
      (letrec
	  ((heap-sizer
	    (meta-lambda
	     (or (and 'bytes  u-dword)
		 (and 'words  u-dword (lambda (units) (* units 2)))
		 (and 'dwords u-dword (lambda (units) (* units 4)))
		 (and 'qwords u-dword (lambda (units) (* units 8))))))
	   (heap-item
	    (meta-lambda
	     (or
	      (and ,@aligner (lambda (align)
			       (let ((size (sassy-heap-size output)))
				 (sassy-heap-size-set!
				  output (+ size (align-to size align)))
				 (when (> align (sassy-heap-align output))
				       (sassy-heap-align-set! output align)))))
	      (and ,@heap-sizer (lambda (sizer)
				  (sassy-heap-size-set!
				   output (+ sizer (sassy-heap-size output)))))
	      (and 'label valid-label __
		   (lambda (label . rst)
		     (let ((current-size (sassy-heap-size output)))
		       (sassy-symbol-def-error output label)
		       (sassy-symbol-set! output label '(section heap)
					  `(offset ,current-size) '(size 0))
		       (for-each heap-item rst)
		       (sassy-symbol-set! output label
					  `(size ,(- (sassy-heap-size output)
						     current-size))))))
	      (and 'begin (* heap-item))
	      (else (lambda (h) (error "sassy: bad heap item" h)))))))
	(for-each heap-item heap-list)))

    (define (process-text text-list output)
      (letrec ((text-item
		(meta-lambda
		 (or
		  (and ,@aligner (lambda (align)
				   (push-stack-align (sassy-text-stack output)
						     align #x90
						     (sassy-text-org output))
				   (if (> align (sassy-text-align output))
				       (sassy-text-align-set! output align))))
 		  (and 'label valid-label __
		       (lambda (label . opcodes-or-prims)
			 (sassy-symbol-def-error output label)
			 (sassy-symbol-set!
			  output label
			  '(section text)
			  `(offset ,(+ (sassy-text-org output)
				       (sassy-text-size output))))
			 (sassy-symbol-set!
			  output label
			  `(size ,(handle-text-block `(begin ,@opcodes-or-prims)
				   output (t-make))))))
		  (else (lambda (opcode-or-prim)
			  (handle-text-block opcode-or-prim output
					     (t-make))))))))
	(for-each text-item text-list)))

    (define (sassy-reloc-set! output name section offset type patcher)
      (sassy-reloc-list-set!
       output (cons (make-sassy-reloc name section offset type patcher)
		    (sassy-reloc-list output))))

    (define (process-data data-list output)
      (letrec
	  ((current-byte-size (/ (sassy-bits output) 8))
	   (char/str/num
	    (lambda (item size)
	      (let ((data-stack (sassy-data-stack output)))
		(cond ((char? item)
		       (push-stack-push data-stack (char->integer item))
		       (push-stack-align data-stack size 0))
		      ((string? item)
		       (push-stack-push data-stack
					(map char->integer (string->list item)))
		       (push-stack-align data-stack size 0))
		      ((number? item)
		       (push-stack-push data-stack
					(number->byte-list item size)))
		      (else (lambda (i) (error "sassy: bad data" i)))))))
	   
	   (handle-data-symbol
	    (lambda (type target value)
	      (when (eqv? 'rel type)
		    (error "no rel relocations in data section right now"
			   (list 'reloc type target value)))
	      (when (eqv? '$here target)
		    (set! target (sassy-data-size output)))
	      (let* ((offset (sassy-data-size output))
		     (target-value (cond ((sassy-symbol-exists-env?
					   output target)
					  => 
					  (lambda (x) (sassy-symbol-offset x)))
					 (else target)))
		     (a-reloc (make-sassy-reloc
			       (get-reloc-target target output)
			       'data offset type #f value current-byte-size))
		     (patcher (let ((p (push-stack-push->patcher
					(sassy-data-stack output)
					(number->byte-list value
							   current-byte-size))))
				(lambda (new)
				  (p (number->byte-list new current-byte-size))
				  (sassy-reloc-value-set! a-reloc new)))))
		(sassy-reloc-patcher-set! a-reloc patcher)
		(sassy-reloc-list-set! output
				       (cons a-reloc (sassy-reloc-list output)))
		(if (number? target-value)
		    (patcher (+ target-value value))
		    (sassy-symbol-set!
		     output target
		     `(unres ,(lambda (n) (patcher (+ n value)))))))))
	   (data4
	    (meta-lambda
	     (or
	      (and ,@symbol? (lambda (label)
			       (check-label-size 4 current-byte-size 'dwords
						 label)
			       (handle-data-symbol 'abs label 0)))
	      (and ,@custom-reloc (lambda (a-reloc)
				    (check-label-size 4 current-byte-size
						      'dwords a-reloc)
				    (apply handle-data-symbol (cdr a-reloc))))
	      (else (lambda (data) (char/str/num data 4))))))
	   (data2
	    (meta-lambda
	     (or
	      (and ,@symbol? (lambda (label)
			       (check-label-size 2 current-byte-size 'words
						 label)
			       (handle-data-symbol 'abs label 0)))
	      (and ,@custom-reloc (lambda (a-reloc)
				    (check-label-size 2 current-byte-size
						      'words a-reloc)
				    (apply handle-data-symbol (cdr a-reloc))))
	      (else (lambda (data) (char/str/num data 2))))))
	   (data-item
	    (meta-lambda
	     (or
	      (and ,@aligner (lambda (align)
			       (push-stack-align (sassy-data-stack output)
						 align 0)
			       (if (> align (sassy-data-align output))
				   (sassy-data-align-set! output align))))
	      (and 'label valid-label __
		   (lambda (label . things)
		     (sassy-symbol-def-error output label)
		     (let ((offset (sassy-data-size output)))
		       (sassy-symbol-set! output label '(section data)
					  `(offset ,offset))
		       (for-each data-item things)
		       (sassy-symbol-set! output label
					  `(size ,(- (sassy-data-size output)
						     offset))))))
	      (and 'locals pair? __
		   (lambda (locals . body)
		     (let ((reset! (setup-locals locals output #f)))
		       (for-each data-item body)
		       (reset!))))
	      (and 'dwords __ (lambda datas (for-each data4 datas)))
	      (and 'bytes  __ (lambda datas (for-each
					     (lambda (x) (char/str/num x 1))
					     datas)))
	      (and 'words  __ (lambda datas (for-each data2 datas)))
	      (and 'qwords __ (lambda datas (for-each
					     (lambda (x) (char/str/num x 8))
					     datas)))
	      (and 'begin (* data-item))
	      (else (lambda (i) (error "sassy: bad data items" i)))))))
	(for-each data-item data-list)))

    (lambda (directives-list output)
      (letrec
	  ((parse-expand (lambda (itm) (parse (sassy-expand itm))))
	   (parse
	     (meta-lambda
	      (or
	       ,@'void
	       (and 'text    __ (lambda lst (process-text lst output)))
	       (and 'heap    __ (lambda lst (process-heap lst output)))
	       (and 'data    __ (lambda lst (process-data lst output)))
	       (and 'import  __ (lambda lst (process-scopes
					     lst 'import output)))
	       (and 'export  __ (lambda lst (process-scopes
					     lst 'export output)))
	       (and 'include __ (lambda lst (process-include lst output)))
;	       (and 'direcs  __ (lambda lst (parse-directives lst output)))
	       (and 'entry    ? (lambda (symb) (process-entry symb output)))
	       (and 'org      ? (lambda (int ) (process-org int output)))
	       (and 'bits     ? (lambda (int ) (process-bits int output)))
	       (and 'begin (* parse-expand))
	       (else (lambda (err) (error "sassy: bad directive" err)))))))
	(for-each parse-expand directives-list)))))

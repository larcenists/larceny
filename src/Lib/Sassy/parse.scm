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
; export all

; These two are used here, in text.scm, and in macros.scm
(define (sassy-label-form? itm)
  (and (pair? itm)
       (eq? 'label (car itm))
       (not (null? (cdr itm)))
       (valid-label (cadr itm))))

(define (sassy-locals-form? itm)
  (and (pair? itm)
       (eq? 'locals (car itm))
       (not (null? (cdr itm)))
       (pair? (cadr itm))))


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
      (cond ((valid-label0 entry-label) =>
	     (lambda (entry-label)
	       (sassy-symbol-set-scope! output entry-label 'export)
	       (sassy-entry-point-set! output entry-label)))
	    (else (error "sassy: bad entry" entry-label))))

    (define (process-include include-list output level expand?)
      (for-each
       (lambda (x)
	 (cond ((string? x)
		(parse-directives (read-file x) output level expand?))
	       ((symbol? x)
		(parse-directives (eval x (interaction-environment))
				  output
				  level
				  expand?))
	       (else (error "sassy: bad include" x))))
       include-list))
  
    (define (process-scopes scope-list scope output)
      (for-each (lambda (x)
		  (if (eq? 'import scope)
		      (sassy-symbol-def-error output x))
		  (cond ((valid-label0 x) =>
			 (lambda (x)
			   (sassy-symbol-set-scope! output x scope)))
			(else (error "sassy: bad scope" scope x))))
		scope-list))

    (define (align-to count align)
      (let ((diff (modulo count align)))
	(if (zero? diff)
	    0
	    (- align diff))))
    
    
    (define (aligner itm)
      (and (eq? 'align (car itm))
	   (not (null? (cdr itm)))
	   (cond ((or (null? (cddr itm))
                      (null? (cdddr itm)))
                  (let ((x (cadr itm))
                        (offset (if (null? (cddr itm)) 0 (caddr itm))))
                    (and (integer? x) (positive? x)
                         (zero? (logand x (- x 1)))
                         (list x offset))))
                 (else #f))))


    (define (heap-items-err x)
      (error "sassy: bad heap item" x))

    (define (process-heap heap-list output level)

      (define (heap-sizer x)
	(cond ((and (pair? x) (not (null? (cdr x))) (null? (cddr x))
		    (u-dword (cadr x)))
	       => (lambda (units)
		    (case (car x)
		      ((bytes) units)
		      ((words)  (* units 2))
		      ((dwords) (* units 4))
		      ((qwords) (* units 8))
		      (else #f))))
	      (else #f)))

      (define (heap-item itm)
	(cond
	 ((aligner itm) =>
	  (lambda (align+offset)
	    (let* ((align  (car align+offset))
		   (offset (cadr align+offset))
		   (size (sassy-heap-size output))
		   (new-size (+ size (align-to size align) offset)))
	      (if (< new-size size)
		  (error
		   "sassy: alignment offset illegally contracts heap" itm)
		  (begin
		    (sassy-heap-size-set! output new-size)
		    (when (> align (sassy-heap-align output))
			  (sassy-heap-align-set! output align)))))))

	 ((heap-sizer itm) =>
	  (lambda (sizer)
	    (sassy-heap-size-set! output (+ sizer (sassy-heap-size output)))))

	 ((sassy-label-form? itm) =>
	  (lambda (label)
	  (let (;(label (cadr itm))
		(rst   (cddr itm))
		(current-size (sassy-heap-size output)))
	    (sassy-symbol-def-error output label)
	    (sassy-symbol-set-sect-off! output label 'heap current-size)
	    (for-each heap-item rst)
	    (sassy-symbol-set-size! output label
				    (- (sassy-heap-size output)
				       current-size)))))

	 ((not (pair? itm)) (heap-items-err itm))

	 (else
	  (case (car itm)
	    ((heap) (for-each heap-item (cdr itm)))
	    ((data) (process-data (cdr itm) output (+ level 1)))
	    ((text) (process-text (cdr itm) output (+ level 1)))
	    ((begin) (for-each heap-item (cdr itm)))
	    (else (heap-items-err itm))))))

      (if (>= level 2)
	  (error "sassy: too many nested switches into a heap directive"
		 `(heap ,@heap-list))
	  (for-each heap-item heap-list)))


    (define (process-text text-list output level)

      (define (text-item itm)

	(cond
	 ((aligner itm) =>
	  (lambda (align+offset)
            (let ((align (car align+offset))
                  (local-offset (cadr align+offset)))
              (push-stack-align (sassy-text-stack output) align #x90
                                (sassy-text-org output)
				local-offset)
              (if (> align (sassy-text-align output))
                  (sassy-text-align-set! output align)))))

	 ((sassy-label-form? itm) =>
	  (lambda (label)
	  (let (;(label (cadr itm))
		(opcodes-or-prims (cddr itm)))
	    (sassy-symbol-def-error output label)
	    (sassy-symbol-set-sect-off!
	     output label 'text (+ (sassy-text-org output)
				   (sassy-text-size output)))
	    (sassy-symbol-set-size!
	     output label
	     (handle-text-block `(begin ,@opcodes-or-prims)
				(t-make output) level)))))

	 ((not (pair? itm)) (error "sassy: bad text item" itm))
	 (else
	  (case (car itm)
	    ((text) (for-each text-item (cdr itm)))
	    ((data) (process-data (cdr itm) output (+ level 1)))
	    ((heap) (process-heap (cdr itm) output (+ level 1)))
	    (else (handle-text-block itm (t-make output) level))))))

      (if (>= level 2)
	  (error "sassy: too many nested switches into a text directive"
		 `(text ,@text-list))
	  (let iter ((rst text-list))
	    (if (not (null? rst))
		(call-with-values
		    (lambda () (span (lambda (x)
				       (and (pair? x)
					    (let ((c (car x)))
					      (or (opcode? c)
						  (eq? c 'label)))))
				     rst))
		  (lambda (block rest)
		    (cond
		     ((null? block)
		      (text-item (car rest))
		      (iter (cdr rest)))

		     ((null? (cdr block))
		      (text-item (car block))
		      (when (not (null? rest))
			    (text-item (car rest))
			    (iter (cdr rest))))

		     ((null? rest)
		      (text-item (cons 'begin block)))

		     (else
		      (text-item (cons 'begin block))
		      (text-item (car rest))
		      (iter (cdr rest))))))))))


    (define (data-items-err x)
      (error "sassy: bad data items" x))


    (define (process-data data-list output level)

      (define current-byte-size (/ (sassy-bits output) 8))

      (define (char/str/num item size)
	(let ((data-stack (sassy-data-stack output)))
	  (cond ((char? item)
		 (push-stack-push data-stack (char->integer item))
		 (push-stack-align data-stack size 0))
		((string? item)
		 (if (not (zero? (string-length item)))
		     (push-stack-push data-stack (map char->integer
						      (string->list item))))
		 (push-stack-align data-stack size 0))
		((number? item)
		 (push-stack-push data-stack (number->byte-list item size)))
		((symbol item)  =>
		 (lambda (item)
		   (error "sassy: wrong size for label or custom reloc"
			  `(,(case size ((1) 'bytes) ((8) 'qwords) (else #f))
			    ,item))))
		(else (error "sassy: bad data" item)))))

      (define (handle-data-symbol type target value)
	(when (eqv? 'rel type)
	      (error "no rel relocations in data section right now"
		     (list 'reloc type target value)))
	(when (eqv? '$here target) (set! target (sassy-data-size output)))
	(let* ((offset (sassy-data-size output))
	       (target-value (cond ((sassy-symbol-exists-env? output target)
				    => (lambda (x) (sassy-symbol-offset x)))
				   (else target)))
	       (a-reloc (make-sassy-reloc
			 (get-reloc-target target output)
			 'data offset type #f value current-byte-size
			 (get-reloc-target-sect target output 'data)))
	       (patcher (let ((p (push-stack-push->patcher
				  (sassy-data-stack output)
				  (integer->byte-list value
						     current-byte-size))))
			  (lambda (new)
			    (p (integer->byte-list new current-byte-size))
			    (sassy-reloc-value-set! a-reloc new)))))
	  (sassy-reloc-patcher-set! a-reloc patcher)
	  (sassy-reloc-list-set!
	   output (cons a-reloc (sassy-reloc-list output)))
	  (if (number? target-value)
	      (patcher (+ target-value value))
	      (sassy-symbol-set-unres!
	       output target (lambda (n sect)
			       (sassy-reloc-target-section-set! a-reloc sect)
			       (patcher (+ n value)))))))

      (define (data itm size size-w)
	(cond ((valid-label0 itm) =>
	       (lambda (itm)
		 (check-label-size size current-byte-size size-w itm)
		 (handle-data-symbol 'abs itm 0)))

	      ((custom-reloc itm) =>
	       (lambda (a-reloc)
		 (check-label-size size current-byte-size size-w a-reloc)
		 (handle-data-symbol (cadr a-reloc)
				     (caddr a-reloc)
				     (cadddr a-reloc))))
	      (else (char/str/num itm size))))

      (define (data-item itm)

	(cond
	 ((aligner itm) =>
	  (lambda (align+offset)
            (let ((align  (car align+offset))
                  (local-offset (cadr align+offset)))
              (push-stack-align (sassy-data-stack output)
				align 0 0 local-offset)
              (if (> align (sassy-data-align output))
                  (sassy-data-align-set! output align)))))

	 ((sassy-label-form? itm) =>
	  (lambda (label)
	  (let (;(label (cadr itm))
		(things (cddr itm)))
	    (sassy-symbol-def-error output label)
	    (let ((offset (sassy-data-size output)))
	      (sassy-symbol-set-sect-off! output label 'data offset)
	      (for-each data-item things)
	      (sassy-symbol-set-size!
	       output label (- (sassy-data-size output) offset))))))

	 ((sassy-locals-form? itm)
	  (let* ((locals (map valid-label0 (cadr itm)))
		 (reset! (setup-locals locals output #f)))
	    (for-each data-item (cddr itm))
	    (reset!)))

	 ((not (pair? itm)) (data-items-err itm))

	 (else
	  (case (car itm)
	    ((dwords) (for-each (lambda (d) (data d 4 'dwords)) (cdr itm)))
	    ((bytes)  (for-each (lambda (d) (char/str/num d 1)) (cdr itm)))
	    ((words)  (for-each (lambda (d) (data d 2 'words))  (cdr itm)))
	    ((qwords) (for-each (lambda (d) (char/str/num d 8)) (cdr itm)))
	    ((begin)  (for-each data-item (cdr itm)))
	    ((data)   (for-each data-item (cdr itm)))
	    ((text)   (process-text (cdr itm) output (+ level 1)))
	    ((heap)   (process-heap (cdr itm) output (+ level 1)))
	    (else (data-items-err itm))))))

      (if (>= level 2)
	  (error "sassy: too many nested switches into a data directive"
		 `(data ,@data-list))
	  (for-each data-item data-list)))
    

    (lambda (directives-list output level expand?)

      (define (bad-dir-err x)
	(error "sassy: bad directive" x))

      (define (parse itm)
	(really-parse (if expand? (sassy-expand itm) itm)))

      (define (really-parse next)
	(if (eq? next 'void)
	    #t
	    (case (car next)
	      ((text)    (process-text (cdr next) output level))
	      ((heap)    (process-heap (cdr next) output level))
	      ((data)    (process-data (cdr next) output level))
	      ((import)  (process-scopes  (cdr next) 'import output))
	      ((export)  (process-scopes  (cdr next) 'export output))
	      ((include) (process-include (cdr next) output level expand?))
	      ((begin)   (for-each really-parse (cdr next)))
	      (else
	       (if (or (null? (cdr next)) (not (null? (cddr next))))
		   (bad-dir-err next)
		   (case (car next)
		     ((entry) (process-entry (cadr next) output))
		     ((org)   (process-org   (cadr next) output))
		     ((bits)  (process-bits  (cadr next) output))
		     (else (bad-dir-err next))))))))
      
      (for-each parse directives-list))))

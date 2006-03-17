; macros.scm - Sassy's macro system
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


; module macros
; import srfi-69 intern
; import-syntax meta-lambda
; export sassy-expand

(define sassy-expand
  (let
      ((sassy-internal-macros ; permanent macros
	(alist->hash-table
	 `((<       . ,(lambda (a b) `(seq (cmp ,a ,b)  l!)))
	   (<=      . ,(lambda (a b) `(seq (cmp ,a ,b) le!)))
	   (>       . ,(lambda (a b) `(seq (cmp ,a ,b)  g!)))
	   (>=      . ,(lambda (a b) `(seq (cmp ,a ,b) ge!)))
	   (=       . ,(lambda (a b) `(seq (cmp ,a ,b)  e!)))
	   (!=      . ,(lambda (a b) `(seq (cmp ,a ,b) ne!)))
	   (zero?   . ,(lambda (x)   `(seq (test ,x ,x) z!)))
	   (asciiz  . ,(lambda (x)   `(bytes ,x 0)))
	   (alt     . ,(lambda x `(inv (seq ,@(map (lambda (y)
						     `(inv ,y))
						   x)))))
	   (times   . ,(lambda (num e)
			 (cons 'begin (make-list num e))))
	   (until   . ,(lambda (test body) `(while (inv ,test) ,body)))

	   (cs:     . ,(lambda rst `(cs (& ,@rst))))
	   (ds:     . ,(lambda rst `(ds (& ,@rst))))
	   (ss:     . ,(lambda rst `(ss (& ,@rst))))
	   (es:     . ,(lambda rst `(es (& ,@rst))))
	   (fs:     . ,(lambda rst `(fs (& ,@rst))))
	   (gs:     . ,(lambda rst `(gs (& ,@rst))))
	   
	   (_global_offset_table_ . ,(string->symbol "_GLOBAL_OFFSET_TABLE_"))
	   (get-got  . (seq (call $eip)
			    (pop ebx)
			    (add ebx (reloc gotpc _global_offset_table_ 3))))
	   (got-offset   . ,(lambda (sym . vals)
			      `(reloc gotoff ,sym ,(if (null? vals)
						       0
						       (apply + vals)))))
	   (got      . ,(lambda (symbol) `(reloc got32 ,symbol)))
	   (plt      . ,(lambda (symbol) `(reloc plt32 ,symbol)))
	   (sym      . ,(lambda (symbol) `(reloc sym32 ,symbol)))))))

    (letrec
	;sassy-user-macros is updated with a new hash-table every time
	;sassy is called, but since it keeps the last table around,
	;you can call sassy-expand yourself to see how something got
	;expanded
	((sassy-user-macros (make-hash-table))

	 (make-sassy-macro
	  (meta-lambda
	   (or (and 'lambda __  (lambda x
				  (eval `(lambda ,@x)
					(interaction-environment))))
	       ,@?)))
	 
	 (macro? (lambda (x)
		   (and (symbol? x)
			(or (hash-table-ref
			     sassy-user-macros x (lambda () #f))
			    (hash-table-ref
			     sassy-internal-macros x (lambda () #f))))))

	 (call-macro (lambda (macro-call args)
		       (expand (if (procedure? macro-call)
				   (apply macro-call args)
				   (cons macro-call args)))))
	 (do-scheme-call (lambda (scheme-call)
			   (expand
			    (eval scheme-call
				  (interaction-environment)))))

	 (symbol-or-expand (lambda (x) (if (pair? x) (expand x) x)))
	 (atom? (lambda (x) (not (pair? x))))

	 (expand
	  (meta-lambda
	   (or (and ,@macro? (lambda (constant) (expand constant)))
	       ,@atom?
	       (and 'label symbol-or-expand (* expand)
		    (lambda (label rest)
		      `(label ,label ,@rest)))
	       (and 'locals ((* symbol-or-expand)) (* expand)
		    (lambda (decs rest)
		      `(locals ,decs ,@rest)))
	       (and '! ? (lambda (scheme-call) (do-scheme-call scheme-call)))
	       (and 'macro symbol? ?
		    (lambda (macro-name macro-body)
		      (hash-table-set! sassy-user-macros macro-name
				       (make-sassy-macro macro-body))
		      'void))
	       (and macro? (* expand) (lambda (macro-call args)
					(call-macro macro-call args)))
	       (and ((and '! ?)) (* expand)
		    (lambda (scheme-call tail)
		      (let ((new-head (do-scheme-call scheme-call)))
			(cond ((macro? new-head) =>
			       (lambda (mac)
				 (call-macro mac tail)))
			      ((procedure? new-head)
			       (call-macro new-head tail))
			      (else (cons new-head tail))))))
	       (and ? (* expand) (lambda (head tail) (cons head tail)))
	       ))))
	       
      (lambda (list-or-hashtable)
	(if (hash-table? list-or-hashtable)
	    (set! sassy-user-macros list-or-hashtable)
	    (expand list-or-hashtable))))))

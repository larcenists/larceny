; text.scm - Sassy's compiler, based on COMFY-65
; (see http://home.pipeline.com/~hbaker1/sigplannotices/CFYCMP1.LSP)
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


; module text
; import push-stacks operands opcodes numbers api text-block
; import-syntax
; export all


(define handle-text-block 

  (let ()

  (define the-assertions
    (alist->hash-table
     '((o!  . 0)                        (no!  . 1)
       (b!  . 2)  (c!   . 2) (nae! . 2) (nb!  . 3)  (nc!  . 3) (ae! . 3)
       (e!  . 4)  (z!   . 4)            (ne!  . 5)  (nz!  . 5)
       (be! . 6)  (na!  . 6)            (nbe! . 7)  (a!   . 7)
       (s!  . 8)                        (ns!  . 9)
       (p!  . 10) (pe!  . 10)           (np!  . 11) (po!  . 11)
       (l!  . 12) (nge! . 12)           (nl!  . 13) (ge!  . 13)
       (le! . 14) (ng!  . 14)           (g!   . 15) (nle! . 15))))

  (define get-assert-name
    (let ((the-names '#(jo jno jb jnb je jne jbe jnbe
			   js jns jp jpo jl jnl jle jg)))
      (lambda (cc)
	(vector-ref the-names cc))))

  (lambda (text-item textb level)

  (define outp (t-outp textb))

  (let ()

  (define rel-adjust (if (= 16 (sassy-bits outp))
			 3
			 5))
  (define current-byte-size (/ (sassy-bits outp) 8))

  (define (fix-body-labels! new-text-size list-of-label-pairs)
    (for-each
     (lambda (label-pair)
       (case (cadr label-pair)
	 ((local import export)
	  (sassy-symbol-set-off!
	   outp (car label-pair) (- new-text-size (caddr label-pair))))))
     list-of-label-pairs))

  (define (fix-block-labels! new-text-size list-of-label-pairs env)
    (for-each
     (lambda (exists)
       (let* ((scope (sassy-symbol-scope exists))
	      (name  (sassy-symbol-name  exists))
	      (offs  (let iter ((rst list-of-label-pairs))
		       (cond ((null? rst) #f)
			     ((and (eq? scope (cadr (car rst)))
				   (eq? name  (car  (car rst))))
			      (caddr (car rst)))
			     (else (iter (cdr rst)))))))
	 (if offs
	     (let ((new-offs (- new-text-size offs)))
	       (sassy-symbol-offset-set! exists new-offs)
	       (for-each (lambda (back-patcher)
			   (back-patcher new-offs (sassy-symbol-section
						   exists)))
			 (sassy-symbol-unres exists))))))
     env))

  (define (fix-relocations! new-text-size list-of-new-relocs)
    (for-each
     (lambda (new-reloc)
       (sassy-reloc-offset-set!
	new-reloc (- new-text-size (sassy-reloc-offset new-reloc)))
       (sassy-reloc-list-set! outp (cons new-reloc (sassy-reloc-list outp))))
     list-of-new-relocs))

  (define (fix-backward-refs! new-text-size list-of-patcher-pairs)
    ; patcher-pair: car = flipped eip, cdr = patcher-procedure
    (for-each
     (lambda (patcher-pair)
       ((cdr patcher-pair) (- new-text-size (car patcher-pair))))
     list-of-patcher-pairs))


  (define (make-block-f-ref-patchers! new-text-size list-of-unres-lists env)
    (for-each
     (lambda (unres-list)
       (let ((exists (let iter ((rst env))
		       (cond ((null? rst) #f)
			     ((and (eq? (car unres-list)
					(sassy-symbol-name (car rst)))
				   (eq? (cadddr unres-list)
					(sassy-symbol-scope (car rst))))
			      (car rst))
			     (else (iter (cdr rst)))))))
	 (when exists
	       (sassy-symbol-unres-set!
		exists
		(cons ((caddr unres-list) (- new-text-size (cadr unres-list)))
		      (sassy-symbol-unres exists))))))
     list-of-unres-lists))
  
  (define (make-forward-ref-patchers! new-text-size list-of-unres-lists)
    ; unres-list: car = symbol, cadr flipped eip, caddr = patcher-generator
    ; cadddr scope
    (for-each
     (lambda (unres-list)
       (let ((scope (cadddr unres-list)))
	 (when (or (case scope
		     ((import local export) #t)
		     (else #f))
		   (not (cadddr unres-list)))
	       (sassy-symbol-set-unres!
		outp (car unres-list)
		((caddr unres-list) (- new-text-size (cadr unres-list)))))))
     list-of-unres-lists))

  (define (assertion? x)
    (if (pair? x)
	#f
	(hash-table-ref the-assertions x (lambda () #f))))

  (define (flip x) ; flip an assertion cc-code
    (if (even? x) (+ x 1) (- x 1)))

  (define (gen-short-assert cc amount)
    (let ((stack (t-text textb)))
      (push-stack-push stack (cons (+ #x70 cc) (integer->byte-list amount 1)))
      (push-stack-size stack)))

  (define (gen-near-assert cc amount)
    (let ((stack (t-text textb)))
      (push-stack-push stack (cons #x0f (cons (+ #x80 cc)
					      (integer->byte-list
					       amount
					       current-byte-size))))
      (push-stack-size stack)))

  (define (gen-assert cc amount)
    (if (s-byte amount)
	(gen-short-assert cc amount)
	(gen-near-assert cc amount)))

  (define (gen-short-jmp amount)
    (let ((stack (t-text textb)))
      (push-stack-push stack (cons #xeb (integer->byte-list amount 1)))
      (push-stack-size stack)))

  (define (gen-near-jmp amount)
    (let ((stack (t-text textb)))
      (push-stack-push stack (cons #xe9 (integer->byte-list
					 amount
					 current-byte-size)))
      (push-stack-size stack)))

  (define (gen-jmp amount)
    (if (s-byte amount)
	(gen-short-jmp amount)
	(gen-near-jmp  amount)))


  (define (emit-direct itm win lose)
    (t-win-set!  textb win)
    (t-lose-set! textb lose)
    (emit-direct2 (car itm) (opcode? (car itm)) (cdr itm) textb))
  

  ; Eeek!! Optimize cc-branches for size. May have to rework to work
  ; nicely with P4 static branch prediction.
  (define (gen-opt-jcc cc win lose)
    (let* ((current  (push-stack-size (t-text textb))))
      (cond ((and (number? win) (number? lose))
	     (let ((win-rel  (- current win))
		   (lose-rel (- current lose)))
	       (cond ((and (zero? win-rel) (zero? lose-rel)) win)
		     ((= win lose)     (gen-jmp win-rel))
		     ((zero? lose-rel) (gen-assert cc win-rel))
		     ((zero? win-rel)  (gen-assert (flip cc) lose-rel))
		     ((and (s-byte lose-rel) (s-byte (+ win-rel 2)))
		      (gen-short-jmp lose-rel)
		      (gen-short-assert cc (+ win-rel 2)))
		     ((and (s-byte win-rel) (s-byte (+ lose-rel 2)))
		      (gen-short-jmp win-rel)
		      (gen-short-assert (flip cc) (+ lose-rel 2)))
		     ((s-byte (+ lose-rel rel-adjust))
		      (gen-near-jmp win-rel)
		      (gen-short-assert (flip cc) (+ lose-rel rel-adjust)))
		     (else (gen-opt-jmp lose win lose)
			   (gen-assert
			    cc (- (push-stack-size (t-text textb)) win))))))
	    ((and (or (symbol win) (number? win))
		  (or (symbol lose) (number? lose)))
	     (cond ((equal? win lose) (emit-direct `(jmp ,win) win lose))
		   ((and (symbol win) (symbol lose))
		    (emit-direct `(jmp ,lose) win lose)
		    (emit-direct `(,(get-assert-name cc) ,win) win lose))
		   ((and (symbol win) (= lose current))
		    (emit-direct `(,(get-assert-name cc) ,win) win lose))
		   ((symbol win)
		    (emit-direct `(jmp ,win) win lose)
		    (gen-assert (flip cc) (+ (- current lose) rel-adjust)))
		   ((and (symbol lose) (= win current))
		    (emit-direct `(,(get-assert-name (flip cc)) ,lose)
				 win lose))

		   (else (emit-direct `(jmp ,lose) win lose)
			 (gen-assert cc (+ (- current win) rel-adjust)))))
	    ((number? win)
	     (emit-direct lose win lose)
	     (gen-assert cc (- (push-stack-size (t-text textb)) win)))
	    ((number? lose)
	     (emit-direct win win lose)
	     (gen-assert (flip cc) (- (push-stack-size (t-text textb)) lose)))
	    ((symbol win)
	     (emit-direct lose win lose)
	     (emit-direct `(,(get-assert-name cc) ,win) win lose))
	    ((symbol lose)
	     (emit-direct win win lose)
	     (emit-direct `(,(get-assert-name (flip cc)) ,lose) win lose))
	    ((equal? win lose) (emit-direct win win lose))
	    (else (emit-direct lose win lose)
		  (let ((new-lose (push-stack-size (t-text textb))))
		    (emit-direct win win lose)
		    (gen-assert (flip cc) (- (push-stack-size (t-text textb))
					    new-lose)))))))
	    
  (define (gen-opt-jmp to win lose)
    (cond ((symbol to) => (lambda x (emit-direct (cons 'jmp x) win lose)))
	  (else (let ((current (push-stack-size (t-text textb))))
		  (if (= to current)
		      current
		      (gen-jmp (- current to)))))))

  ; Iterative closure to get the backward branches right. Same with
  ; handle-while. Looks good on the page, but can be exponential in
  ; time when there are inner loops. Maybe a user option should
  ; exist to do this Baker's way (one pass only), but with no jump
  ; size optimization. Maybe it won't matter anyway too much. TBD.
  (define (handle-iter exp win lose)
    (let ((reset! (t-save! textb))
	  (old-env (sassy-symbol-table outp)))
      (let iter ((new (compile exp
			       (+ 2 (push-stack-size (t-text textb)))
			       lose))
		 (count (+ 3 (push-stack-size (t-text textb)))))
	(if (not (= count new))
	    (begin (reset!)
		   (sassy-symbol-table-set! outp old-env)
		   (iter (compile `(with-win $win ,exp) new lose)
			 new))
	    new))))

  (define (handle-while test body win lose)
    (let ((reset! (t-save! textb))
	  (old-env (sassy-symbol-table outp)))
      (let iter ((new (compile body
			       (compile `(mark ,test)
					(+ 2 (push-stack-size
					      (t-text textb)))
					win)
			       lose))
		 (count (+ 3 (push-stack-size (t-text textb)))))
	(if (not (= count new))
	    (begin (reset!)
		   (sassy-symbol-table-set! outp old-env)
		   (iter (compile body 
				  (compile `(mark ,test) new win)
				  lose)
			 new))
	    (compile '(leap $win) new lose)))))

  ; The core dispatch procedure - this is where the Comfy65 based
  ; stuff happens.
  (define (compile exp win lose)

    (define (text-error x)
      (error "sassy: bad text item" x))

    (define (symbol2 x)
      (let ((z (symbol x)))
	(and z (not (memq z '($win $lose))) z)))

    (define (branch-or-compile e)
      (if (and (pair? e)
	       (or (eqv? (car e) 'jmp)
		   (eqv? (car e) 'ret)))
	  (emit-direct e win lose)
	  (really-compile e)))


    (define (parse-with-x x k)
      (if (not (null? x))
	  (if (null? (cdr x))
	      (k (append x '((seq))))
	      (if (and (list? x) (= 2 (length x)))
		  (k x)
		  (text-error (cons 'with-win x))))
	  (text-error (cons 'with-win x))))
    
    (define (pwl x)
      (cond ((assertion? (car x)) =>
	     (lambda (cc)
	       (compile (cadr x) win (gen-opt-jcc cc win lose))))
	    ((symbol2 (car x)) =>
	     (lambda (new-lose)
	       (compile (cadr x) win new-lose)))
	    (else (compile (cadr x) win (branch-or-compile (car x))))))


    (define (pww x)
      (cond ((assertion? (car x)) =>
	     (lambda (cc)
	       (compile (cadr x) (gen-opt-jcc cc win lose) lose)))
	    ((symbol2 (car x)) =>
	     (lambda (new-win)
	       (compile (cadr x) new-win lose)))
	    (else (compile (cadr x) (branch-or-compile (car x)) lose))))


    (define (pwwl x)
      (cond ((assertion? (car x)) =>
	     (lambda (cc1)
	       (cond ((assertion? (cadr x)) =>
		      (lambda (cc2)
			(let ((new-lose (gen-opt-jcc cc2 win lose)))
			  (compile (caddr x) (gen-opt-jcc cc1 win lose)
				   new-lose))))
		     ((symbol2 (cadr x)) =>
		      (lambda (new-lose)
			(compile (caddr x) (gen-opt-jcc cc1 win lose)
				 new-lose)))
		     (else
		      (let ((new-lose (branch-or-compile (cadr x))))
			(compile (caddr x) (gen-opt-jcc cc1 win lose)
				 new-lose))))))

	    ((symbol2 (car x)) =>
	     (lambda (new-win)
	       (cond ((assertion? (cadr x)) =>
		      (lambda (cc)
			(compile (caddr x) new-win (gen-opt-jcc cc win lose))))

		     ((symbol2 (cadr x)) =>
		      (lambda (new-lose)
			(compile (caddr x) new-win new-lose)))

		     (else (compile (caddr x) new-win
				    (branch-or-compile (cadr x)))))))

	    (else (let ((win-b (car x)))
		    (cond ((assertion? (cadr x)) =>
			   (lambda (cc)
			     (let ((new-lose (gen-opt-jcc cc win lose)))
				   (compile (caddr x) (branch-or-compile win-b)
					    new-lose))))

			  ((symbol2 (cadr x)) =>
			   (lambda (new-lose)
			     (compile (caddr x)
				      (branch-or-compile win-b)
				      new-lose)))

			  (else (let ((new-lose (branch-or-compile (cadr x))))
				   (compile (caddr x)
					    (branch-or-compile win-b)
					    new-lose))))))))
    
    (define (really-compile itm)
      (cond ((and (pair? itm) (opcode? (car itm))) =>
	     (lambda (opcode)
	       (if (or (symbol win)
		       (not (= win (push-stack-size (t-text textb)))))
		   (gen-opt-jmp win win lose))
	       (t-win-set!  textb win)
	       (t-lose-set! textb lose)
	       (emit-direct2 (car itm) opcode (cdr itm) textb)))

	    ((and (pair? itm) (eq? 'begin (car itm)))
	     (cond ((null? (cdr itm)) win)
		   ((null? (cddr itm)) (really-compile (cadr itm)))
		   (else (let ((w (really-compile (cons 'begin (cddr itm)))))
			   (compile (cadr itm) w w)))))

	    ((sassy-label-form? itm) =>
	     (lambda (label)
	     (let (;(label (cadr itm))
		   (body  (cddr itm))
		   (size  (push-stack-size (t-text textb))))
	       (sassy-symbol-def-error outp label)
	       (let* ((scope (sassy-symbol-scope
			      (sassy-symbol-set-sect! outp label 'text)))
		      (new-size (really-compile (cons 'begin body))))
		 (sassy-symbol-set-size! outp label (- new-size size))
		 (let ((pnt (push-stack-size (t-text textb))))
		   (push-t-label! textb (list label scope pnt))
		   pnt)))))

	    ((sassy-locals-form? itm)
	     (let* ((locals (map valid-label0 (cadr itm)))
		    (body   (cddr itm))
		    (reset! (setup-locals locals outp
					  (lambda (new-sym)
					    (push-t-env! textb new-sym)))))
		 (really-compile (cons 'begin body))
		 (reset!)
		 (push-stack-size (t-text textb))))

	    ((assertion? itm) => (lambda (cc) (gen-opt-jcc cc win lose)))
	    ((eq? '$eip itm)  (push-stack-size (t-text textb)))
	    ((eq? '$win itm) win)
	    ((eq? '$lose itm) lose)

	    ((not (pair? itm)) (text-error itm))
	    
	    (else
	     (case (car itm)
	       ((seq) (cond ((null? (cdr itm)) win)
			    ((null? (cddr itm)) (really-compile (cadr itm)))
			    (else (compile
				   (cadr itm)
				   (really-compile (cons 'seq (cddr itm)))
				   lose))))

	       ((inv) (if (and (not (null? (cdr itm))) (null? (cddr itm)))
			  (compile (cadr itm) lose win)
			  (text-error itm)))

	       ((if) (if (and (list? itm) (= 4 (length itm)))
			 (let* ((loser (really-compile (cadddr itm)))
				(winner (really-compile (caddr itm))))
			   (compile (cadr itm) winner loser))
			 (text-error itm)))

	       ((iter) (if (and (not (null? (cdr itm))) (null? (cddr itm)))
			   (handle-iter (cadr itm) win lose)
			   (text-error itm)))

	       ((while) (if (and (list? itm) (= 3 (length itm)))
			    (handle-while (cadr itm) (caddr itm) win lose)
			    (text-error itm)))

	       ((text) (really-compile (cons 'begin (cdr itm))))
	       ((heap) (begin (parse-directives (list itm) outp (+ level 1))
			      win))
	       ((data) (begin (parse-directives (list itm) outp (+ level 1))
			      win))
	       ((esc)  (if (and (list? itm) (= 3 (length itm))
				(pair? (cadr itm)))
			   (let ((list-of-escapes (cadr itm))
				 (body (caddr itm)))
			     (really-compile body)
			     (for-each (lambda (escape)
					 (emit-direct escape win lose))
				       (reverse list-of-escapes))
			     (push-stack-size (t-text textb)))
			   (text-error itm)))

	       ((mark) (if (and (not (null? (cdr itm))) (null? (cddr itm)))
			   (let ((z (really-compile (cadr itm))))
			     (push-t-mark! textb z)
			     z)
			   (text-error itm)))

	       ((leap) (if (and (not (null? (cdr itm))) (null? (cddr itm)))
			   (let ((z (really-compile (cadr itm))))
			     (or (pop-t-mark! textb) z))
			   (text-error itm)))

	       ((with-win)  (parse-with-x (cdr itm) pww))
	       ((with-lose) (parse-with-x (cdr itm) pwl))

	       ((with-win-lose)
		(if (list? itm)
		    (let ((x (length (cdr itm))))
		      (cond ((= x 2) (pwwl (append (cdr itm) '((seq)))))
			    ((= x 3) (pwwl (cdr itm)))
			    (else (text-error itm))))))
	       (else (text-error itm))))))

    (really-compile exp))


  (let ((win (compile text-item 0 0)))
    (when (not (= win (push-stack-size (t-text textb)))) ; in case there was
	  (gen-opt-jmp win win 0))                       ; a top-level "leap"
    (let ((new-text-size (+ (sassy-text-size outp)
			    (sassy-text-org outp)
			    (push-stack-size (t-text textb)))))
      (fix-relocations! new-text-size (t-reloc textb))
      (fix-backward-refs! new-text-size (t-res textb))
      (make-forward-ref-patchers! new-text-size (t-unres textb))
      (make-block-f-ref-patchers! new-text-size (t-unres textb) (t-env textb))
      (fix-body-labels! new-text-size (t-label textb))
      (fix-block-labels! new-text-size (t-label textb) (t-env textb))
      (push-stack-append! (sassy-text-stack outp) (t-text textb))
      (push-stack-size (t-text textb))))))))

; Naive HTML parser
;
; HTML-PARSER input-port  =>  tree
;
;   Reads something that resembles HTML and returns a list of trees:
;     tree ::=  string
;            |  (tagname (attr...) tree ...)
;   where string is literal string data.  Terminates parsing at EOF;
;   there will be one tree for each top-level tag.
;
; Features
;  - Will handle non-well-formed data moderately well
;  - Handles SCRIPT and STYLE in-line data properly
;  - Handles PRE
;  - Handles HTML entities in the 8-bit range
;  - User-installable handlers: property HTML-REDUCE-HANDLER on tagname
;    is called with the element when a reduction is performed to create
;    the element.
;
; Documentation
;  "sos" stands for "stack of stuff".
;
; To do
;  - Deal more generally with optional-start-tag (html head body tbody)
;    and insert these structures properly into the output.  The current
;    in-head? structure can be generalized to some sort of state where
;    state is (perhaps) before-html, after-html, in-head, in-body, in-table
;    and the appropriate token is inserted when not found where it is needed.
;  - TERMINATES? can be more sophisticated: There's some sort of total 
;    order on tags that we can probably exploit more generally than the
;    ad-hoc code used now.
;  - Better handling of unterminated A: if A is encountered with 
;    another A on the stack, the A on the stack should be reduced 
;    alone, leaving content above it alone.  Handles <a name=foo>
;    as <a name=foo />
;  - Comments not handled correctly
;  - Much more testing, see testsuite
;  - Performance investigation

(define (html-parser in)

  (define out (open-output-string))	; string buffer

  (define in-head? #t)			; is set to #f when entering BODY

  (define (content-less? tag)
    (memq tag '(area base basefont br col frame hr img input  
		isindex link meta param)))

  (define (uninterpreted? tag)
    (memq tag '(script style)))

  (define (inline-element? tag)
    (memq tag '(tt i b u s strike big small em strong dfn code samp
	        kbd var cite abbr acronym a applet object font 
		map q sub sup span bdo iframe nobr
		select textarea label button script img basefont input)))

  (define (optional-end-tag? tag)	; restricted set
    (memq tag '(colgroup dd dt li option p tbody tfoot thead td th tr)))

  (define (header-content? tag)
    (memq tag '(title isindex base script style meta link object)))

  (define (reduce tag orig-sos)
    (dispatch tag (let loop ((sos orig-sos) (stuff '()))
		    (cond ((null? sos)
			   orig-sos)
			  ((eq? tag (car sos))
			   (cons (cons tag stuff) (cdr sos)))
			  ((symbol? (car sos))
			   (loop (cons (cons (car sos) stuff) 
				       (cdr sos)) 
				 '()))
			  (else
			   (loop (cdr sos) (cons (car sos) stuff)))))))

  (define (dispatch tag sos)
    (let ((probe (getprop tag 'html-reduce-handler)))
      (if probe
	  (probe (car sos)))
      sos))
	    
  (define (push-output sos)
    (let ((s (get-output-string out)))
      (if (= 0 (string-length s))
	  sos
	  (cons s sos))))

  (define (maybe-reduce-head sos)
    (if (and in-head?
	     (or (and (symbol? (car sos))
		      (not (header-content? (car sos))))
		 (and (pair? (car sos))
		      (not (header-content? (caar sos))))
		 (string? (car sos)))
	     (eq? (current-tag (cdr sos)) 'head))
	(begin
	  (set! in-head? #f)
	  (cons (car sos) (reduce 'head (cdr sos))))
	sos))
	  
  (define (terminates? t1 t2)		; opening t2 terminates pending t1?
    (and (not (eq? t1 '**fnord**))	; hack
	 (or (and (eq? t1 'p) (not (inline-element? t2)))
	     (and (eq? t1 'a) (eq? t2 'a))
	     (and (inline-element? t2) (not (inline-element? t1)))
	     (and (optional-end-tag? t1) (optional-end-tag? t2))
	     (and (eq? t2 'body) (not (eq? t1 'html)))
	     (and (eq? t2 'frameset) (not (eq? t1 'html))))))

  (define (current-tag sos)
    (cond ((null? sos) #f)
	  ((symbol? (car sos)) (car sos))
	  (else (current-tag (cdr sos)))))

  (define (scan sos)
    (scan-aux sos (if (memq 'pre sos) #t #f)))

  (define (scan-aux sos pre?)
    (reset-output-string out)
    (let loop ((sos sos))
      (let ((c (read-char in)))
	(cond ((eof-object? c)
	       (maybe-reduce-head (push-output sos)))
	      ((and (char-whitespace? c) (not pre?))
	       (let ((sos (maybe-reduce-head (push-output sos))))
		 (reset-output-string out)
		 (loop sos)))
	      ((char=? c #\<)
	       (scan-tag-or-directive (maybe-reduce-head (push-output sos))))
	      ((char=? c #\&)
	       (loop (scan-entity sos '())))
	      (else
	       (write-char c out)
	       (loop sos))))))

  (define (scan-entity sos cs)
    (let ((d (peek-char in)))
      (cond ((eof-object? d)
	     (maybe-reduce-head (push-output sos)))
	    ((char=? d #\;)
	     (read-char in)
	     (display (or (html-entity-equivalent (list->string (reverse cs))) 
			  #\?)
		      out)
	     sos)
	    ((or (char=? d #\<)
		 (char=? d #\&)
		 (char-whitespace? d)
		 (= (length cs) 10))
	     (display #\& out)
	     (display (list->string (reverse cs)) out)
	     sos)
	    (else
	     (scan-entity sos (cons (read-char in) cs))))))

  (define (scan-tag-or-directive sos)
    (let ((d (peek-char in)))
      (cond ((char=? d #\!)
	     (read-char in)
	     (scan-to-end-of-tag sos))	; FIXME, wrong
	    ((char=? d #\?)
	     (read-char in)
	     (scan-to-end-of-tag sos))
	    (else
	     (let ((end-tag? (eqv? d #\/)))
	       (if end-tag? 
		   (read-char in))
	       (let ((tagname (scan-tagname)))
		 (cond (end-tag?
			(scan-to-end-of-tag (reduce tagname sos)))
		       ((and (current-tag sos)
			     (not (terminates? (current-tag sos) tagname)))
			(scan-attributes (maybe-reduce-head (cons tagname sos))
					 '()))
		       (else
			(let loop ((sos sos))
			  (if (and (current-tag sos)
				   (terminates? (current-tag sos) tagname))
			      (loop (reduce (current-tag sos) sos))
			      (scan-attributes (maybe-reduce-head 
						(cons tagname sos))
					       '())))))))))))

  (define (scan-tagname)
    (reset-output-string out)
    (let loop ()
      (let ((c (peek-char in)))
	(if (and (char? c)
		 (not (char=? c #\/))
		 (not (char=? c #\>))
		 (not (char=? c #\=))
		 (not (char-whitespace? c)))
	    (begin
	      (read-char in)
	      (display (char-downcase c) out)
	      (loop))
	    (string->symbol (get-output-string out))))))

  (define (scan-to-end-of-tag sos)
    (let ((c (read-char in)))
      (cond ((eof-object? c)
	     sos)
	    ((char=? c #\>)
	     (scan sos))
	    (else
	     (scan-to-end-of-tag sos)))))

  (define (scan-attributes sos attrs)
    (let ((c (peek-char in)))
      (cond ((eof-object? c)
	     (cdr sos))
	    ((char=? c #\>)
	     (read-char in)
	     (cond ((content-less? (car sos))
		    (scan (dispatch (car sos)
				    (cons (list (car sos) attrs) (cdr sos)))))
		   ((uninterpreted? (car sos))
		    (scan-uninterpreted (cons attrs sos) (car sos)))
		   (else
		    (scan (cons attrs sos)))))
	    ((char=? c #\/)
	     (read-char in)
	     (scan-to-end-of-tag (reduce (car sos) (cons attrs sos))))
	    ((char-whitespace? c)
	     (read-char in)
	     (scan-attributes sos attrs))
	    (else
	     (scan-attribute-value sos attrs (scan-tagname))))))

  (define (scan-attribute-value sos attrs tagname)
    (let ((c (peek-char in)))
      (cond ((eof-object? c)
	     (cdr sos))
	    ((char=? c #\=)
	     (read-char in)
	     (scan-attributes sos (cons (cons tagname (scan-attrdata)) attrs)))
	    (else
	     (scan-attributes sos (cons tagname attrs))))))
  
  (define (scan-attrdata)
    (reset-output-string out)
    (let loop ((quoted? #f))
      (let ((c (peek-char in)))
	(cond ((eof-object? c)
	       (get-output-string out))
	      ((eqv? c quoted?)
	       (read-char in)
	       (get-output-string out))
	      ((and (or (char=? c #\") (char=? c #\'))
		    (not quoted?))
	       (read-char in)
	       (loop c))
	      ((and (not quoted?)
		    (or (char-whitespace? c)
			(char=? c #\/)
			(char=? c #\>)))
	       (get-output-string out))
	      (else
	       (read-char in)
	       (display c out)
	       (loop quoted?))))))

  (define (scan-uninterpreted sos tag)
    (reset-output-string out)
    (let loop ((in-comment? #f))
      (let ((c (read-char in)))
	(cond ((eof-object? c)
	       (cddr sos))
	      ((and (not in-comment?) (char=? c #\<))
	       (let ((d (read-char in)))
		 (if (eof-object? d)
		     (begin (write-char #\< out)
			    (loop #f))
		     (cond ((char=? d #\/)
			    (let* ((ts (symbol->string tag))
				   (tl (string-length ts))
				   (ss (make-string tl)))
			      (let mloop ((i 0))
				(let ((c (peek-char in)))
				  (cond ((and (< i tl)
					      (char? c)
					      (char-ci=? c (string-ref ts i)))
					 (string-set! ss i (read-char in))
					 (mloop (+ i 1)))
					((and (= i tl) (eqv? c #\>))
					 (read-char in)
					 (scan (reduce tag (push-output sos))))
					(else
					 (write-char #\< out)
					 (write-char #\/ out)
					 (display (substring ss 0 i) out)
					 (loop #f)))))))
			   ((char=? d #\!)
			    (write-char #\< out)
			    (write-char d out)
			    (let ((e (read-char in)))
			      (if (eof-object? e)
				  (loop #f)
				  (begin
				    (write-char e out)
				    (if (char=? e #\-)
					(let ((f (read-char in)))
					  (if (eof-object? f)
					      (loop #f)
					      (begin
						(write-char f out)
						(loop (char=? f #\-)))))
					(loop #f))))))
			   (else
			    (write-char #\< out)
			    (write-char d out)
			    (loop #f))))))
	      ((and in-comment? (char=? c #\-))
	       (write-char c out)
	       (let ((d (read-char in)))
		 (if (eof-object? d)
		     (loop #t)
		     (begin
		       (write-char d out)
		       (if (char=? d #\-)
			   (let ((e (read-char in)))
			     (if (eof-object? e)
				 (loop #t)
				 (begin
				   (write-char e out)
				   (loop (not (char=? e #\>))))))
			   (loop #t))))))
	      (else 
	       (write-char c out)
	       (loop in-comment?))))))

  (cdar (reduce '**fnord** (scan '(**fnord**)))))


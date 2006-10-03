; BUG: do not use fluid-let, it is not thread safe.

(require 'fluid)

(define content-tags
  '(p div span html body a h1 h2 h3 h4 h5 table tr th td
    strong b i em u big blockquote center code dl dd dt ul li
    del font form ins nobr noframes noscript nobr ol pre q s
    samp small strike sub sup tbody tfoot thead tt var))
	 
(define (html-format tree)
  
  ; Invariant: following a linebreak, pos is 0

  (define width    70)
  (define pos       0)
  (define indent    0)
  (define spacing? #t)
  (define counter   0)
  (define ol?      #f)
  (define purespace? #f)

  (define (linebreak)
    (newline)
    (set! pos 0))

  (define (display-word tree transform)
    (let* ((s (transform tree))
	   (l (string-length s))
	   (beginning? (or (= pos 0) (= pos indent)))
	   (nospace? (or beginning? (not spacing?)))
	   (ps? purespace?)
	   (x (if nospace? 0 1)))
      (set! purespace? #f)
      (if (and (> (+ pos l x) width) (not beginning?))
	  (begin (linebreak)
		 (begin 
		   (display (make-string indent #\space))
		   (set! pos indent))
		 (display s)
		 (set! pos (+ pos l)))
	  (begin (if (= pos 0)
		     (begin
		       (display (make-string indent #\space))
		       (set! pos indent))
		     (if (not nospace?)
			 (if ps?
			     (display " ")
			     (display (transform " ")))))
		 (display s)
		 (set! pos (+ pos l x))))))

  (define (link-transform t)
    (lambda (s)
      (list->string (map (lambda (x)
			   (if (char=? x #\space) #\_ x))
			 (string->list (t s))))))

  ; Simple functionality missing: center, dl/dt/dd
  ; Difficult functionality missing: tables
  ; Even harder: CSS :-)

  (define (html-fmt tree transform)
    (if (string? tree)
	(display-word tree transform)
	(case (car tree)
	  ((p div)
	   (if (not (= pos indent)) (linebreak))
	   (linebreak)
	   (html-fmt2 tree transform)
	   (linebreak))
	  ((br)
	   (linebreak))
	  ((strong b i em u big code)
	   (html-fmt2 tree (lambda (x)
			     (transform (string-upcase x)))))
	  ((blockquote)
	   (fluid-let ((indent (+ indent 3))
		       (width  (- width 3)))
	     (linebreak)
	     (html-fmt2 tree transform)
	     (linebreak)))
	  ((a)
	   (set! purespace? #t)
	   (html-fmt2 tree (link-transform transform)))
	  ((pre)
	   (let ((old-spacing? spacing?))
	     (set! spacing? #f)
	     (html-fmt2 tree (lambda (x) x))
	     (set! spacing? old-spacing?)))
	  ((h1 h2 h3 h4 h5 h6)
	   (linebreak)
	   (html-fmt2 tree (lambda (x)
			     (transform (string-upcase x))))
	   (linebreak))
	  ((ul)
	   (fluid-let ((indent (+ indent 3))
		       (ol? #f))
	     (html-fmt2 tree transform)))
	  ((ol)
	   (fluid-let ((indent (+ indent 3))
		       (counter 1)
		       (ol? #t))
	     (html-fmt2 tree transform)))
	  ((li)
	   (linebreak)
	   (let ((mark (if ol?
			   (let ((x (number->string counter)))
			     (set! counter (+ counter 1))
			     x)
			   "*")))
	     (if (> indent 0)
		 (begin (fluid-let ((indent (- indent 2)))
			  (display-word (string-append mark " ") transform))
			(html-fmt2 tree transform))
		 (begin (display-word (string-append " " mark " ") transform)
			(fluid-let ((indent (+ indent 3)))
			  (html-fmt2 tree transform))))))
	  ((img)
	   (display-word "<IMG>" transform))
	  ((hr)
	   (linebreak)
	   (display-word (make-string (- width indent) #\-))
	   (linebreak))
	  (else
	   (if (memq (car tree) content-tags)
	       (html-fmt2 tree transform))))))

  (define (html-fmt2 tree transform)
    (for-each (lambda (x) (html-fmt x transform)) (cddr tree)))

  (html-fmt tree (lambda (x) x)))


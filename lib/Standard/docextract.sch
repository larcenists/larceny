; Copyright 2003 Lars T Hansen
;
; Permission to use this code for any purpose whatsoever is hereby
; granted, provided that the above copyright notice and this legend
; are preserved in any work using this code.
;
; 2003-01-03 / lth

;@doc DOCEXTRACT file
;@syn Program to extract documentation blocks from Scheme code.
;
; What is it?
;
; DOCEXTRACT extracts documentation blocks from source files, converts
; them to HTML, and writes the HTML to an output file.
;
; Input file format.
;
; A source file consists of code interspersed with documentation blocks.
; A documentation block is some sequence of lines that all start with a
; semicolon (with no leading whitespace), delimited in the following
; ways:
;   * The block starts on a line starting with a string of semicolons 
;     followed by "@doc"
;   * The block ends on a line that either starts with a string of
;     semicolons followed by "@cod", or on a line starting a new
;     documentation block, or on a line not starting with a semicolon,
;     or at the end of the file.  The terminating line is not part of
;     the comment block.
;
; The line containing @doc must contain two more tokens.  The first
; token is the name of the entity being defined and the second is its
; category, which can be one of "file", "section", "syntax",
; "procedure", or "reader-syntax".
;
; Inside a @doc block, several qualifiers can be used to signal special
; parts of the documentation block:
;   * @syn is used to mark the synopsis of the entity being defined;
;     the synopsis is taken as the contents of the rest of the line
;   * @res is used to mark the description of the result of the entity's 
;     execution
; @syn can be used for all entities; @res cannot be used without @syn, 
; and only make sense for syntax, procedure, and reader-syntax.
;
; Text that is not marked by @syn or @res is interpreted in the
; following way.  A blank line means a paragraph break.  Lines that
; have zero or one leading spaces are run together into one paragraph.
; More-indented lines are processed as unordered lists (if bulleted by
; "-" or "*"), as ordered lists (if bulleted by "#"), or emitted
; literally otherwise.  Bulleted points can span multiple lines and
; lists can be nested, as signalled by indentation level.
;
; For an example, see the documentation of this file, which uses most
; of the facilities.
;@cod
;
; To do (for sure)
;
;  * Right now FILE and SECTION are not shown in the output, because
;    they are not very interesting for the kind of docs we are
;    testing.  But they should be shown.  What format, where, ... ?
;    One possibility: A by-file section, alphabetized by filename.
;    The file and section blocks are listed under each file, followed
;    by reader syntax, syntax, and procedures defined in that file,
;    sorted together alphabetically (just summaries, like in the
;    index).
;
; To do (maybe)
;
;  * Some documentation may exist in files that have a FILE
;    block, but where the tag in the FILE block isn't to be used
;    for getting the proc (eg it is in Lib/Common, so it is always
;    available).  Handle this with an annotation on the @doc
;    tag or with special extractor code?
;  * Sometimes several documentation sets can be extracted from
;    the same set of sources (eg in Lib/Common, we could extract
;    both implementation documentation and library documentation).
;    Can we handle this in a nice and general way, eg by an
;    optional annotation ("purpose") on @doc?
;  * Markup parameter names and return values separately (cf R5RS)
;  * Support for @also to create common doc for several procedures
;    (see example in list.sch)
;  * Support for @rat (Rationale subsection)
;  * Support for @eg (Example subsection)
;  * Support some inline markup, eg @parm to mark up next string of
;    nonblanks as parameter, @fun ditto for function, and perhaps
;    @code .. @edoc to mark up a section as code.  Don't yet know
;    what is most useful.  Keep it simple...

;@doc USAGE section
;@syn (extract-documentation title infilenames outfilename)
;
; Processes all files in infilenames and writes the documentation to
; outfilename, using title for the TITLE and H1 tags.

(require 'io)
(require 'regexp)
(require 'string)
(require 'list)
(require 'format)

;@doc IMPLEMENTATION section
;
; The implementation is split in two: one phase extracts the
; documentation blocks from the input files and recognizes keywords
; like @syn; the second phase writes the output while recognizing
; indented lines as list structures.
;
; It is possible that the list processing should be done during the
; first phase, since it would make it easier to write output routines
; for other formats than HTML.
;
; Performance: The extractor, running in the Petit Larceny interpreter
; on a 400MHz Macintosh G3 PowerBook, is really quite slow, taking
; 3.5s to process this file.  This is probably because all the
; auxiliary libraries -- even things like read-line -- are
; interpreted.  The interpreter conses 12MW during the run (but spends
; almost no time in GC).

;@doc EXTRACT-DOCUMENTATION procedure
;@syn (extract-documentation title (infilename ...) outfilename)
;@res unspecified
;
; Read the documentation blocks of all the infilenames, merge them, convert the
; result to HTML and write a complete HTML document to outfilename.
;
; The file contains four sections:
;
;  # A by-name section for reader syntax
;  # A by-name section for syntax
;  # A by-name section for procedures
;  # An index keyed on name across reader-syntax, syntax, and procedures
;
; At the top of the file there are quick links to each of the sections, and the
; beginning of each section 

(define (extract-documentation title fns . rest)

  (define (extract-it out)
    (let ((fns  (if (pair? fns) fns (list fns)))
	  (docs '()))

      (define (show-items key)
	(for-each (lambda (d)
		    (doc-as-html d out))
		  (sort (filter (lambda (x) (eq? (cadr x) key)) docs)
			(lambda (x y)
			  (string-ci<? (car x) (car y))))))

      (define (make-index)
	(for-each (lambda (d)
		    (let-values (((key type syn res doc fileref filename) (apply values d)))
		      (format out "<a href=\"#~a\"><span class=\"~a\">~a</span></a> <em>~a</em> (<span class=\"fileref-file\">~a</span>)<br>" 
			      (string-append (symbol->string type) "-" key)
			      (string-append (symbol->string type) "-name")
			      key 
			      type 
			      fileref)))
		  (sort (filter (lambda (x) (memq (cadr x) '(syntax reader-syntax procedure))) docs)
			(lambda (x y)
			  (string-ci<? (car x) (car y))))))
	
      (for-each (lambda (fn)
		  (set! docs (append docs 
				     (call-with-input-file fn 
				       (lambda (in) 
					 (doc-extract in (list fn)))))))
		fns)

      (docextract-display-header title out)
      (format out "<H1>~a</H1>~%" title)

      (format out "<p>~%")
      (format out "<A href=\"#reader-syntax\">Reader syntax</a><br>~%")
      (format out "<A href=\"#syntax\">Syntax</a><br>~%")
      (format out "<A href=\"#procedures\">Procedures</a><br>~%")
      (format out "<A href=\"#index\">Index</a><br>~%")
      (format out "</p>~%")

      (format out "<A name=\"reader-syntax\" /><H2>Reader syntax</H2>~%")
      (show-items 'reader-syntax)

      (format out "<A name=\"syntax\" /><H2>Syntax</H2>~%")
      (show-items 'syntax)

      (format out "<A name=\"procedures\" /><H2>Procedures</H2>~%")
      (show-items 'procedure)

      (format out "<A name=\"index\" /><H2>Index</H2>~%")
      (make-index)

      (docextract-display-footer out)))

  (if (null? rest)
      (extract-it (current-output-port))
      (begin
	(delete-file (car rest))
	(call-with-output-file (car rest) extract-it))))

;@doc DOC-EXTRACT procedure
;@syn (doc-extract input-port info)
;@res ((key type syn res doc fileref . info) ...)
;
; Read the contents of input-port to EOF and return list of all documentation
; blocks defined in the stream.  The blocks are returned in the order they occur in
; the file.
;
; Info is usually a list of one element: the file name or other general identifier
; for the origin of the input.

(define (doc-extract input info)
  (let* ((docstart (regexp-compile "^;+@doc"))
	 (docend   (regexp-compile "^;+@cod"))
	 (synopsis (regexp-compile "^;+@syn"))
	 (result   (regexp-compile "^;+@res"))
	 (fileref  #f)
	 (results  '()))

    ; l is the line with the @doc
    ; returns (key type syn res doc fileref . info) where 
    ;    key is string
    ;    type is symbol
    ;    syn, res, and fileref are string or #f,
    ;    doc is list of string
    ;    info is anything

    (define (extract-documentation l)
      (let ((key #f)
	    (type #f)
	    (syn #f)
	    (res #f)
	    (doc '()))      ; lines stripped of semicolon in reverse order

	(define (skip-keyword l)
	  (let loop ((i 0))
	    (cond ((= i (string-length l)) "")
		  ((char-whitespace? (string-ref l i))
		   (let loop ((i (+ i 1)))
		     (cond ((= i (string-length l)) "")
			   ((char-whitespace? (string-ref l i))
			    (loop (+ i 1)))
			   (else
			    (substring l i (string-length l))))))
		  (else
		   (loop (+ i 1))))))

	(define (skip-comment l)
	  (let loop ((i 0))
	    (cond ((= i (string-length l)) "")
		  ((char=? (string-ref l i) #\;)
		   (loop (+ i 1)))
		  (else
		   (substring l i (string-length l))))))

	(define (extract-body)
	  (let loop ((l (read-line input)))
	    (cond ((eof-object? l) l)
		  ((regexp-match docend l) l)
		  ((regexp-match docstart l) l)
		  ((regexp-match synopsis l)
		   (set! syn (skip-keyword l))
		   (loop (read-line input)))
		  ((regexp-match result l)
		   (set! res (skip-keyword l))
		   (loop (read-line input)))
		  ((and (> (string-length l) 0)
			(char=? (string-ref l 0) #\;))
		   (set! doc (cons (skip-comment l) doc))
		   (loop (read-line input)))
		  (else
		   l))))

	(define (astype t)
	  (cond ((string-ci=? t "procedure") 'procedure)
		((string-ci=? t "syntax") 'syntax)
		((string-ci=? t "reader-syntax") 'reader-syntax)
		((string-ci=? t "file") 'file)
		((string-ci=? t "section") 'section)
		(else 'unknown)))

	(let ((xs (string-split l (lambda (x) (not (char-whitespace? x))))))
	  (case (length xs)
	    ((1)  #t)
	    ((2)  (set! key (cadr xs)))
	    (else (set! key (cadr xs))
		  (set! type (astype (caddr xs)))))
	  (if (eq? type 'file)
	      (set! fileref key))
	  (let ((next-line (extract-body)))
	    (values (cons* key type syn res (reverse doc) fileref info)
		    next-line)))))
	
    (let loop ((l (read-line input)))
      (if (not (eof-object? l))
	  (begin
	    (if (and (> (string-length l) 0)
		     (regexp-match docstart l))
		(call-with-values
		    (lambda ()
		      (extract-documentation l))
		  (lambda (d l)
		    (set! results (cons d results))
		    (loop l)))
		(loop (read-line input))))
	  (reverse results)))))

;@doc DOC-AS-HTML procedure
;@syn (doc-as-html documentation-block output-port)
;@res unspecified
;
; Format the documentation-block as HTML on the output-port.  The documentation
; block has six fields: key type res syn doc file.  The output is entirely in terms
; of DIV and SPAN elements with different classes:
;   * The entire block is enclosed in a DIV with class documentationblock
;   * The heading is a DIV with classname the same as the type name of the block
;   * The synopsis, if present, is a DIV with classname synopsis
;   * The body of documentation is enclosed in a DIV with classname documentation
;   * Each paragraph in the body is enclosed in a classless DIV, but lines that
;     stand for themselves (more than one leading space) are just copied literally 
;     to the output
;   * Elements that can be marked up specially are wrapped in SPANs with these names:
;     * synopsiscode -- literal code in a synopsis
;     The spans are good.

(define (doc-as-html docblock output)

  (define (lines-of-text doc)

    (define (showpara p)
      (if (not (null? p))
	  (begin
	    (format output "<div class=\"documentationpara\">~%")
	    (for-each (lambda (l)
			(display l output)
			(newline output))
		      (reverse p))
	    (format output "</div>~%"))))

    (define (leading-spaces l)
      (let loop ((i 0))
	(if (and (< i (string-length l))
		 (char-whitespace? (string-ref l i)))
	    (loop (+ i 1))
	    i)))

    (define (at-toplevel doc para)
      (if (null? doc)
	  (showpara para)
	  (let* ((l (car doc))
		 (k (leading-spaces l)))
	    (cond ((and (> k 1) (< k (string-length l)))
		   (showpara para)
		   (at-toplevel (indentation k doc) '()))
		  ((= k (string-length l))
		   (showpara para)
		   (at-toplevel (cdr doc) '()))
		  (else
		   (at-toplevel (cdr doc) (cons l para)))))))

    (define (indentation k doc)
      (if (null? doc)
	  doc
	  (let* ((l (car doc))
		 (m (leading-spaces l)))
	    (cond ((= m (string-length l))
		   (indentation k (cdr doc)))
		  ((< m k)
		   doc)
		  ((< k (string-length l))
		   (case (string-ref l k)
		     ((#\* #\-) 
		      (format output "<ul>~%")
		      (let ((doc (indented-list k doc)))
			(format output "</ul>~%")
			(indentation k doc)))
		     ((#\#) 
		      (format output "<ol>~%")
		      (let ((doc (indented-list k doc)))
			(format output "</ol>~%")
			(indentation k doc)))
		     (else
		      (display l output)
		      (newline output)
		      (cdr doc))))
		  (else
		   doc)))))

    (define (indented-list k doc)
      (if (null? doc)
	  doc
	  (let* ((l (car doc))
		 (m (leading-spaces l)))
	    (if (= m (string-length l))
		(begin (format output "<br>~%")
		       (indented-list k (cdr doc)))
		(cond ((< m k)
		       doc)
		      ((= m k) 
		       (case (string-ref l m)
			 ((#\* #\# #\-)
			  (format output "<li>~a~%" (substring l (+ m 1) (string-length l)))
			  (indented-list k (cdr doc)))
			 (else
			  doc)))
		  ((and (> m k) 
			(memv (string-ref l m) '(#\* #\# #\-)))
		   (indented-list k (indentation m doc)))
		  (else
		   (format output "~a~%" l)
		   (indented-list k (cdr doc))))))))

    (format output "<div class=\"documentation\">~%")
    (at-toplevel doc '())
    (format output "</div>~%"))

  (define (code-body syn res doc fileref)
    (cond ((and syn res)
	   (format output
		   "<div class=\"synopsis\">Synopsis: <span class=\"synopsiscode\">~a => ~a</span></div>~%" 
		   syn res))
	  (syn
	   (format output 
		   "<div class=\"synopsis\">Synopsis: <span class=\"synopsiscode\">~a</span></div>~%" 
		   syn)))
    (if fileref
	(format output
		"<div class=\"fileref\">Require: <span class=\"fileref-file\">~a</span></div>~%"
		fileref))
    (lines-of-text doc))

  (let-values (((key type syn res doc fileref filename) (apply values docblock)))
     (format output "<a name=\"~a\" />~%" (string-append (symbol->string type) "-" key))
     (format output "<div class=\"documentationblock\">~%")
     (case type
       ((procedure) 
	(format output 
		"<div class=\"procedure\">Procedure <span class=\"procedure-name\">~a</span></div>~%" 
		key)
	(code-body syn res doc fileref))
       ((syntax)
	(format output 
		"<div class=\"syntax\">Syntax <span class=\"syntax-name\">~a</span></div>~%" 
		key)
	(code-body syn res doc fileref))
       ((reader-syntax)
	(format output 
		"<div class=\"reader-syntax\">Reader-syntax <span class=\"reader-syntax-name\">~a</span></div>~%" 
		key)
	(code-body syn res doc fileref))
       ((file) 
	(format output "<h3>File <tt>~a</tt> (from ~a)</h3>~%" fileref filename)
	(if syn
	    (format output
		    "<p><b>~a</b></p>~%" syn))
	(lines-of-text doc))
       ((section) 
	(format output "<h4>Section <tt>~a</tt></h4>~%" key)
	(if syn
	    (format output
		    "<p><b>~a</b></p>~%" syn))
	(lines-of-text doc))))
  (format output "</div>~%")
  (unspecified))

(define (docextract-display-header title out)
  (format out
"<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
<html>
<head>
<title>~a</title>
<style type=\"text/css\">
.documentationblock { margin-top: 2em }
.documentationpara { margin-top: 1em }
.procedure { margin-top: 1em }
.synopsis { font-style: italic }
.documentation { margin-top: 1em; margin-left: 2em }
.procedure { font-weight: bold }
.procedure-name { font-family: monospace; font-style: nromal }
.syntax { font-weight: bold }
.syntax-name { font-family: monospace; font-style: normal }
.reader-syntax { font-weight: bold }
.reader-syntax-name { font-family: monospace; font-style: normal }
.synopsiscode { font-family: monospace; font-style: normal }
.fileref { font-style: italic }
.fileref-file { font-family: monospace; font-style: normal }
</style>
</head>
<body>" title))

(define (docextract-display-footer out)
  (format out "</body>~%</html>~%"))

; eof

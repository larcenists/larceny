;;; Copyright (C) William D Clinger (2017). All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;; FIXME: the improvements flagged by FIXME comments below should
;;; be made after SRFI 152 becomes final.

(define (check:pred-string-start-end! name pred s rest)
  (if (not (procedure? pred))
      (error name (errmsg 'msg:notproc) pred))
  (if (not (string? s))
      (error name (errmsg 'msg:notstring) s))
  (if (not (null? rest))
      (let ((start (car rest))
            (end (if (null? (cdr rest)) (string-length s) (cadr rest))))
        (if (not (fixnum? start))
            (error name (errmsg 'msg:notindex) start))
        (if (not (fixnum? end))
            (error name (errmsg 'msg:notindex) end))
        (if (not (<= 0 start end (string-length s)))
            (error name (errmsg 'msg:illegalargs) s start end)))))

(define (default:start s rest)
  (if (null? rest)
      0
      (car rest)))

(define (default:end s rest)
  (if (or (null? rest)
          (null? (cdr rest)))
      (string-length s)
      (cadr rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string->utf16be s . rest)
  (check:pred-string-start-end! 'string->utf16be values s rest)
  (apply textual->utf16be s rest))

(define (string->utf16le s . rest)
  (check:pred-string-start-end! 'string->utf16le values s rest)
  (apply textual->utf16le s rest))

;;; FIXME: these would be more efficient if they didn't create an
;;; intermediate text.

(define (utf16be->string bv . rest)
  (textual->string
   (apply utf16be->text bv rest)))

(define (utf16le->string bv . rest)
  (textual->string
   (apply utf16le->string bv rest)))

;;; FIXME: these should ensure their first two arguments are strings.

(define string-contains       textual-contains)
(define string-contains-right textual-contains-right)

;;; FIXME: these would be more efficient if they didn't create an
;;; intermediate text.

(define (string-remove pred s . rest)
  (check:pred-string-start-end! 'string-remove pred s rest)
  (textual->string
   (apply textual-remove pred s rest)))

(define (string-replicate s from to . rest)
  (textual->string
   (apply textual-replicate s from to rest)))

(define (string-split s delimiter . rest)
  (map textual->string
       (apply textual-split s delimiter rest)))

;;; No close counterpart in previous libraries.

(define (string-take-while s pred . rest)
  (check:pred-string-start-end! 'string-take-while pred s rest)
  (let ((i (apply textual-skip s pred rest)))
    (substring s
               (default:start s rest)
               (or i
                   (default:end s rest)))))

(define (string-take-while-right s pred . rest)
  (check:pred-string-start-end! 'string-take-while-right pred s rest)
  (let ((i (apply textual-skip-right s pred rest)))
    (substring s
               (if i
                   (+ i 1)
                   (default:start s rest))
               (default:end s rest))))

(define (string-drop-while s pred . rest)
  (check:pred-string-start-end! 'string-drop-while pred s rest)
  (let ((i (apply textual-skip s pred rest)))
    (if i
        (substring s i (default:end s rest))
        "")))

(define (string-drop-while-right s pred . rest)
  (check:pred-string-start-end! 'string-drop-while pred s rest)
  (let ((i (apply textual-skip-right s pred rest)))
    (if i
        (substring s (default:start s rest) (+ i 1))
        "")))

(define (string-span s pred . rest)
  (check:pred-string-start-end! 'string-span pred s rest)
  (let ((i (apply textual-skip s pred rest)))
    (if i
        (values (substring s (default:start s rest) i)
                (substring s i (default:end s rest)))
        (values (substring s (default:start s rest) (default:end s rest))
                ""))))

(define (string-break s pred . rest)
  (check:pred-string-start-end! 'string-break pred s rest)
  (let ((i (apply textual-index s pred rest)))
    (if i
        (values (substring s (default:start s rest) i)
                (substring s i (default:end s rest)))
        (values (substring s (default:start s rest) (default:end s rest))
                ""))))

(define (string-segment s k)
  (if (and (string? s)
           (fixnum? k)
           (<= 0 k))
      (do ((n (string-length s))
           (i 0 (+ i k))
           (ss '()
               (cons (substring s i (min (+ i k) n))
                     ss)))
          ((>= i n)
           (reverse ss)))
      (error 'string-segment
             (errmsg 'msg:illegalargs)
             s k)))

;;; FIXME: string-map of (scheme base) incompatible with (srfi 152).
;;;
;;; The R7RS says "it is an error" if the first argument to
;;; string-map returns a string instead of a single character.
;;; SRFI 152 says the first argument to string-map is allowed
;;; to return strings as well as characters.
;;; WG2 appears to be leaning toward the status quo, meaning
;;; (scheme base) exports a string-map for which it is an error,
;;; (srfi 152) exports a different string-map as in SRFI 152, and
;;; we don't yet know which version (scheme string) will export.
;;;
;;; For the time being, in Larceny, the string-map exported by
;;; (scheme base) will be incompatible with the string-map
;;; exported by (srfi 152).
;;;
;;; WG2 has not as yet decided what to do about (scheme string),
;;; or perhaps even whether there will be a standard library
;;; of that name, so Larceny doesn't have to worry about that.
;;; For now.

(define string-map
  (let ()

    (define (string-map1 f x)
      (let ((k (string-length x)))
	(let loop ((i 0) ; index into the strings
		   (m 0) ; length of chars
		   (chars '()))
	  (if (< i k)
	      (let ((result (f (string-ref x i))))
		(cond ((char? result)
   		       (loop (+ i 1)
			     (+ m 1)
			     (cons result chars)))
		      ((string? result)
		       (loop (+ i 1)
			     (+ m (string-length result))
			     (append (reverse (string->list result)) chars)))
		      (else
		       (complain:result result))))
	      (finish m chars)))))

    (define (string-map2 f x y)
      (let ((k (min (string-length x) (string-length y))))
	(let loop ((i 0) ; index into the strings
		   (m 0) ; length of chars
		   (chars '()))
	  (if (< i k)
	      (let ((result (f (string-ref x i)
			       (string-ref y i))))
		(cond ((char? result)
   		       (loop (+ i 1)
			     (+ m 1)
			     (cons result chars)))
		      ((string? result)
		       (loop (+ i 1)
			     (+ m (string-length result))
			     (append (reverse (string->list result)) chars)))
		      (else
		       (complain:result result))))
	      (finish m chars)))))

    (define (string-map3 f x y z)
      (let ((k (min (string-length x) (string-length y) (string-length z))))
	(let loop ((i 0) ; index into the strings
		   (m 0) ; length of chars
		   (chars '()))
	  (if (< i k)
	      (let ((result (f (string-ref x i)
			       (string-ref y i)
			       (string-ref z i))))
		(cond ((char? result)
   		       (loop (+ i 1)
			     (+ m 1)
			     (cons result chars)))
		      ((string? result)
		       (loop (+ i 1)
			     (+ m (string-length result))
			     (append (reverse (string->list result)) chars)))
		      (else
		       (complain:result result))))
	      (finish m chars)))))

    (define (string-mapN k g)
      (let loop ((i 0) ; index into the strings
		 (m 0) ; length of chars
		 (chars '()))
	(if (< i k)
	    (let ((result (g i)))
	      (cond ((char? result)
   		     (loop (+ i 1)
			   (+ m 1)
			   (cons result chars)))
		    ((string? result)
		     (loop (+ i 1)
			   (+ m (string-length result))
			   (append (reverse (string->list result)) chars)))
		    (else
		     (complain:result result))))
	    (finish m chars))))

    (define (finish m chars)
      ;;(list->string (reverse chars)))    ; equivalent, but slower
      (let ((s (make-string m)))
	(do ((i (- m 1) (- i 1))
	     (chars chars (cdr chars)))
	    ((< i 0) s)
	  (string-set! s i (car chars)))))

    (define (complain:result result)
      (error 'string-map "neither character nor string: " result))
		  
    (case-lambda
     ((f x)     (string-map1 f x))
     ((f x y)   (string-map2 f x y))
     ((f x y z) (string-map3 f x y z))
     ((f . strings)
      (let ((k (apply min (map string-length strings))))
	(string-mapN k
		     (lambda (i)
		       (apply f
			      (map (lambda (s) (string-ref s i))
				   strings)))))))))

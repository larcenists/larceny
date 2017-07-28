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

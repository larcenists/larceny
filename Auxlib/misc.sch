; Larceny auxiliary library -- miscellaneous
; $Id: misc.sch,v 1.1 1997/02/27 16:48:16 lth Exp $

(define (some? p l)
  (and (not (null? l))
       (or (p (car l))
	   (some? p (cdr l)))))

(define (every? p l)
  (or (null? l)
      (and (p (car l))
	   (every? p (cdr l)))))

; Names used by older code.

(define some some?)
(define every every?)

; Names used in Chez Scheme

(define andmap every?)
(define ormap some?)


; Reductions.
; The procedures optionally take an identity element.  If the
; identity is present, lists of length 0 are allowed.  If not,
; the list must have at least one element.
;
; FIXME: These need to be compatible with MIT Scheme; check the docs.

(define (reduce-left proc l . rest)

  (define (loop val l)
    (if (null? l)
        val
        (loop (proc val (car l)) (cdr l))))

  (if (and (null? rest) (null? l))
      (error "reduce-left: not enough arguments."))
  (if (null? rest)
      (loop (car l) (cdr l))
      (loop (car rest) l)))

(define (reduce-right proc l . rest)

  (define (loop val l)
    (if (null? l)
	(if (null? rest)
	    val
	    (proc val (car rest)))
	(proc val (loop (car l) (cdr l)))))
	
  (if (and (null? rest) (null? l))
      (error "reduce-right: not enough arguments."))
  (if (null? l)
      (car rest)
      (loop (car l) (cdr l))))

; Names used by some (notably SML).

(define foldl reduce-left)
(define foldr reduce-right)

; eof

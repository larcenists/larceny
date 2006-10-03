; Directory listings (unix)
; 2004-01-11 / lth
;
; The offsets in the dirent accessors are probably x86-Linux-specific!!

(require 'ffi)

(define unix/opendir  (foreign-procedure "opendir" '(string) 'uint))
(define unix/readdir  (foreign-procedure "readdir" '(uint) 'uint))
(define unix/closedir (foreign-procedure "closedir" '(uint) 'int))

(define (dirent-ino dirent)    (%peek32 dirent))
(define (dirent-off dirent)    (%peek32u (+ dirent 4)))
(define (dirent-reclen dirent) (%peek16u (+ dirent 8)))
(define (dirent-type dirent)   (%peek8u (+ dirent 10)))
(define (dirent-name dirent)   (%peek-string (+ dirent 11)))

(define (directory-contents dirname)
  (let ((dir (unix/opendir dirname)))
    (if (zero? dir)
	(error "No directory " dirname)
	(let loop ((files '()))
	  (let ((dirent (unix/readdir dir)))
	    (if (zero? dirent)
		(begin (unix/closedir dir)
		       (reverse files))
		(loop (cons (dirent-name dirent) files))))))))

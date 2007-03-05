; Useful file system procedures.
; 2001-11-16 / lth
;
; $Id$

; Wrapper around the standard function that signals an error if the file
; does not exist.

(require 'foreign-ctools)
(require 'std-ffi)

(define file-modification-time
  (let ((file-modification-time file-modification-time))
    (lambda (fn)
      (or (file-modification-time fn)
	  (error "file-modification-time: \"" fn "\" does not exist.")))))

(define (file-newer? f1 f2)
  (let ((t1 (file-modification-time f1))
	(t2 (file-modification-time f2)))
    (let loop ((i 0))
      (cond ((= i (vector-length t1)) #f)
	    ((= (vector-ref t1 i) (vector-ref t2 i))
	     (loop (+ i 1)))
	    (else
	     (> (vector-ref t1 i) (vector-ref t2 i)))))))


(define list-directory
   (let ()
     (define-cstruct-offsets ("<dirent.h>") (*d_name_offset* "struct dirent" "d_name"))
     (define readdir  (foreign-procedure "readdir"  '(unsigned)  
'unsigned))
     (define opendir  (foreign-procedure "opendir"  '(string)    
'unsigned))
     (define closedir (foreign-procedure "closedir" '(unsigned) 'void))

     (define (dirent->name ent)
       (let loop ((l '())
                  (i *d_name_offset*))
         (let ((b (%peek8 (+ ent i))))
           (if (zero? b)
               (list->string (reverse l))
               (loop (cons (integer->char b) l)
                     (+ i 1))))))

     ;; list-directory : String -> [Listof String]
     (define (list-directory path)
       (let* ((dir (opendir path))
              (files (let loop ((ent (readdir dir)))
                       (if (zero? ent)
                           '()
                           (cons (dirent->name ent)
                                 (loop (readdir dir)))))))

         (closedir dir)
         files))

     list-directory))


; eof


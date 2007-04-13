; Useful file system procedures.
; 2001-11-16 / lth
;
; $Id$

; Wrapper around the standard function that signals an error if the file
; does not exist.

(require 'foreign-ctools)
(require 'std-ffi)
(require 'srfi-0) ; for conditional compilation below

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

(define stat-alist
  (let ()
    (define-c-info 
      (include<> "sys/stat.h") 
      (sizeof struct-stat-sz "struct stat")
      (sizeof dev-t-sz   "dev_t")        (sizeof ino-t-sz   "ino_t")  
      (sizeof mode-t-sz  "mode_t")       (sizeof nlink-t-sz "nlink_t")
      (sizeof uid-t-sz   "uid_t")        (sizeof gid-t-sz   "gid_t")
      (sizeof off-t-sz   "off_t")        (sizeof time-t-sz  "time_t")
      (sizeof blksize-t-sz "blksize_t")  (sizeof blkcnt-t-sz "blkcnt_t")
      (struct "stat" 
              (*st_dev_offs*   "st_dev")   (*st_ino_offs*     "st_ino")
              (*st_mode_offs*  "st_mode")  (*st_nlink_offs*   "st_nlink")
              (*st_uid_offs*   "st_uid")   (*st_gid_offs*     "st_gid")
              (*st_rdev_offs*  "st_rdev")  (*st_size_offs*    "st_size")
              (*st_atime_offs* "st_atime") (*st_mtime_offs*   "st_mtime")
              (*st_ctime_offs* "st_ctime") (*st_blksize_offs* "st_blksize")
              (*st_blocks*     "st_blocks")))
    (define stat ;; XXX consider grabbing the impl from unix.sch
      (cond-expand 
       (linux 
        (let ((xstat (foreign-procedure "__xstat" '(int string boxed) 'int)))
          (lambda (name buf) (xstat 0 name buf))))
       (else 
        (foreign-procedure "stat" '(string boxed) 'int))))
    (let* ((names 
            '(dev ino mode nlink uid gid rdev size atime mtime ctime blksize blocks))
           (offsets
            (list *st_dev_offs*    *st_ino_offs*    *st_mode_offs*   *st_nlink_offs*  
                  *st_uid_offs*    *st_gid_offs*    *st_rdev_offs*   *st_size_offs*   
                  *st_atime_offs*  *st_mtime_offs*  *st_ctime_offs*  *st_blksize_offs*
                  *st_blocks*))
           (sizes 
            (list dev-t-sz         ino-t-sz         mode-t-sz        nlink-t-sz
                  uid-t-sz         gid-t-sz         dev-t-sz         off-t-sz
                  time-t-sz        time-t-sz        time-t-sz        blksize-t-sz
                  blkcnt-t-sz))
           (getters (map (lambda (n) 
                           (case n 
                             ((1) %get8) ((2) %get16) ((4) %get32) ((8) %get64)
                             (else (error 'stat-alist-definition "Unhandled size for getters"))))
                         sizes)))
      (lambda (filename)
        (let* ((stat-results (make-bytevector struct-stat-sz))
               (errcode (stat filename stat-results)))
          (cond ((zero? errcode)
                 (map (lambda (name get offset) (list name (get stat-results offset)))
                      names getters offsets))
                (else
                 (error 'stat-alist ": something went wrong: "  errcode))))))))

(define (file-length filename)
  (cadr (assq 'size (stat-alist filename))))

;;; XXX not thread-safe!  Consider using the _r variants where appropriate.
;;; (but at the moment define-cstruct-offset is even less robust than this is...)
(define list-directory
   (let ()
     (define-cstruct-offsets () ("<dirent.h>") (*d_name_offset* "struct dirent" "d_name"))
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
              (ignore
               (if (zero? dir)
                   (error 'list-directory 
                          ": error opening directory " path)))
              (files (let loop ((ent (readdir dir)))
                       (if (zero? ent)
                           '()
                           (cons (dirent->name ent)
                                 (loop (readdir dir)))))))

         (closedir dir)
         files))

     list-directory))


; eof


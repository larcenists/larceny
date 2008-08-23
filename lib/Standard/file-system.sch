; Useful file system procedures.
; 2001-11-16 / lth
;
; $Id$

; Wrapper around the standard function that signals an error if the file
; does not exist.

(require 'foreign-ctools)
(require 'std-ffi)
(require 'foreign-stdlib)
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

(cond-expand
  (win32
    (define-values (list-directory file-directory? file-attributes)
      (let ()
        (define-c-info
          (include<> "windows.h")
          (sizeof find-data-size "WIN32_FIND_DATA")
          (const  invalid-handle ulong "INVALID_HANDLE_VALUE")
	  (const  file-attribute-directory ulong "FILE_ATTRIBUTE_DIRECTORY")
          (fields "WIN32_FIND_DATA"
                  (find-data-name "cFileName")))

        (define malloc
          (foreign-procedure "malloc" '(unsigned) 'unsigned))
        (define free
          (foreign-procedure "free" '(unsigned) 'void))

        (define find-first-file
          (foreign-procedure "FindFirstFileA"
                             '(string unsigned)
                             'unsigned
                             'stdcall))

        (define find-next-file
          (foreign-procedure "FindNextFileA"
                             '(unsigned unsigned)
                             'bool
                             'stdcall))

        (define find-close
          (foreign-procedure "FindClose"
                             '(unsigned)
                             'int
                             'stdcall))

	(define get-file-attributes 
	  (foreign-procedure "GetFileAttributesA"
			     '(string)
			     'long
			     'stdcall))
	  

        (define (find-data->filename find-data)
          (%peek-string (+ find-data-name find-data)))

	(define (list-directory path)
          (let* ((mask      (string-append path "/*"))
                 (find-data (malloc find-data-size))
                 (handle    (find-first-file mask find-data)))
            (dynamic-wind
              (lambda () #f)
              (lambda ()
                (cond
                  ((= handle invalid-handle)
                   (error 'list-directory
                          ": error opening directory " path))
                  (else
                    (do ((result (list (find-data->filename find-data))
                                 (cons (find-data->filename find-data)
                                       result)))
                      ((not (find-next-file handle find-data))
                       (reverse result))))))
              (lambda ()
                (find-close handle)
                (free find-data)))))
	
	(define (file-directory? filename)
	  (let ((attrs (get-file-attributes filename)))
	    (cond ((< attrs 0)
		   (error 'file-directory? ": " filename " does not exist.")))
	    (not (zero? (fxlogand file-attribute-directory attrs)))))
	
	(values list-directory file-directory? get-file-attributes))))

  (else
    ;;; XXX not thread-safe!  Consider using the _r variants where
    ;;; appropriate.  (but at the moment define-cstruct-offset is even less
    ;;; robust than this is...)
    (define-values (list-directory)
      (let ()
        (define-c-info (include<> "dirent.h")
                       (struct "dirent" (*d_name_offset* "d_name")))
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

    (define-values (stat-alist fstat-alist lstat-alist 
                               mode-type
                               file-directory? file-length)
      (let ()
        (define-c-info 
          (include<> "sys/stat.h") 
          (sizeof struct-stat-sz "struct stat")
          (sizeof dev-t-sz   "dev_t")        (sizeof ino-t-sz   "ino_t")  
          (sizeof mode-t-sz  "mode_t")       (sizeof nlink-t-sz "nlink_t") ;XXX
          (sizeof uid-t-sz   "uid_t")        (sizeof gid-t-sz   "gid_t")   ;XXX
          (sizeof off-t-sz   "off_t")        (sizeof time-t-sz  "time_t")
          (sizeof blksize-t-sz "blksize_t")  (sizeof blkcnt-t-sz "blkcnt_t") ;XXX
          (struct "stat" 
                  (*st_dev_offs*   "st_dev")   (*st_ino_offs*     "st_ino")
                  (*st_mode_offs*  "st_mode")  (*st_nlink_offs*   "st_nlink")
                  (*st_uid_offs*   "st_uid")   (*st_gid_offs*     "st_gid")
                  (*st_rdev_offs*  "st_rdev")  (*st_size_offs*    "st_size")
                  (*st_atime_offs* "st_atime") (*st_mtime_offs*   "st_mtime")
                  (*st_ctime_offs* "st_ctime") 
                  (*st_blksize_offs* "st_blksize") ;XXX
                  (*st_blocks*     "st_blocks") ;XXX
	)
          (const s_ifmt  uint "S_IFMT")
          (const s_ifblk uint "S_IFBLK")
          (const s_ifchr uint "S_IFCHR")
          (const s_ififo uint "S_IFIFO")
          (const s_ifreg uint "S_IFREG")
          (const s_ifdir uint "S_IFDIR")
          (const s_iflnk uint "S_IFLNK")
          (const s_isuid uint "S_ISUID")
          (const s_isgid uint "S_ISGID")
          (const s_irusr uint "S_IRUSR")
          (const s_iwusr uint "S_IWUSR")
          (const s_ixusr uint "S_IXUSR")
          (const s_irgrp uint "S_IRGRP")
          (const s_iwgrp uint "S_IWGRP")
          (const s_ixgrp uint "S_IXGRP")
          (const s_iroth uint "S_IROTH")
          (const s_iwoth uint "S_IWOTH")
          (const s_ixoth uint "S_IXOTH")
          )
        (define stat ;; XXX consider grabbing the impl from unix.sch
          (cond-expand 
           (linux 
            (let ((xstat (foreign-procedure "__xstat" '(int string boxed) 'int)))
              (define-c-info (include<> "sys/stat.h") 
                (const stat_ver int "_STAT_VER"))
              (lambda (name buf) (xstat stat_ver name buf))))
           (else 
            (foreign-procedure "stat" '(string boxed) 'int))))
        (define fstat ;; XXX consider grabbing the impl from unix.sch
          (cond-expand 
           (linux 
            (let ((xstat (foreign-procedure "__fxstat" '(int string boxed) 'int)))
              (define-c-info (include<> "sys/stat.h") 
                (const stat_ver int "_STAT_VER"))
              (lambda (name buf) (xstat stat_ver name buf))))
           (else 
            (foreign-procedure "fstat" '(string boxed) 'int))))
        (define lstat ;; XXX consider grabbing the impl from unix.sch
          (cond-expand 
           (linux 
            (let ((xstat (foreign-procedure "__lxstat" '(int string boxed) 'int)))
              (define-c-info (include<> "sys/stat.h") 
                (const stat_ver int "_STAT_VER"))
              (lambda (name buf) (xstat stat_ver name buf))))
           (else 
            (foreign-procedure "lstat" '(string boxed) 'int))))

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
               (size->getter (lambda (n) 
                               (case n 
                                 ((1) %get8) ((2) %get16) ((4) %get32) ((8) %get64)
                                 (else (error 'stat-alist-definition 
                                              "Unhandled size for getters")))))
               (getters (map size->getter sizes)))
          (define (stat-bytes stat filename)
            (let* ((stat-results (make-bytevector struct-stat-sz))
                   (errcode (stat filename stat-results)))
              (cond ((zero? errcode)
                     stat-results)
                    (else
                     (error 'stat ": something went wrong: " errcode " " filename)))))
          (define (make-stat-alist stat)
            (lambda (filename)
              (let ((stat-results (stat-bytes stat filename)))
                (map (lambda (name get offset) (list name (get stat-results offset)))
                     names getters offsets))))
          (define (mode-matches? mode s_xxx)
            (= (fxlogand mode s_ifmt) s_xxx))
          (define (mode-type mode)
            (cond 
             ((mode-matches? mode s_ifreg) 'regular)
             ((mode-matches? mode s_ifdir) 'directory)
             ((mode-matches? mode s_ifblk) 'block-special)
             ((mode-matches? mode s_ifchr) 'character-special)
             ((mode-matches? mode s_ififo) 'fifo)
             ((mode-matches? mode s_iflnk) 'symbolic-link)
             (else 'unknown)))
          (define (file-directory? filename)
            (eq? (mode-type (cadr (assq 'mode (stat-alist filename)))) 'directory))
          (define (file-length filename)
            (cadr (assq 'size (stat-alist filename))))
          (values (make-stat-alist stat) (make-stat-alist fstat) (make-stat-alist lstat)
                  mode-type file-directory? file-length
                  ))))))

; eof

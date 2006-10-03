; Standard autoloads, for Petit Larceny.
;
; FIXME: needs to be better integrated with the new library, and less with the
;        old auxlib.

(define osdep-file)

(let ((os (cdr (assq 'os-name (system-features)))))
  (cond ((string-ci=? os "win32")
	 (display "WIN32!")
	 (autoload:define-logical-directory 'lib (library-vicinity))
	 (autoload:define-logical-directory 'auxlib "/Source/Larceny/src/Auxlib/")
	 (set! osdep-file "~auxlib/osdep-unix.sch"))  ; FIXME
	((string-ci=? os "macos")
	 (autoload:define-logical-directory 'auxlib "Ertevann:Larceny:Larceny-0.42:src:Auxlib:")
	 (set! osdep-file "~auxlib/osdep-macos.sch"))
	(else
	 #t)))

(autoload (read-line read-substring write-substring file-modification-time file-newer)
	  "~auxlib/io.sch")

(autoload (aremq! aremv! aremove! filter find remove-duplicates least 
           greatest mappend make-list reduce reduce-right fold-left
           fold-right iota list-head )
      "~auxlib/list.sch")

(autoload (absolute-pathname? relative-pathname? make-pathname)
      osdep-file)

(autoload (pretty-print pretty-line-length)
      "~auxlib/pp.sch")

(autoload (substring-match substring-match-ci string-prefix=? ->string string-insert! string-split)
      "~auxlib/string.sch")

(define osdep-file)

; eof

;; FIXME:  the ilasm procedure (at the bottom) probably
;;         needs some attention
(define (reloadil)
  (load (make-filename *larceny-root* "Asm" "IL" "il.imp.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "il.imp2.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "config.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "util.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "il-gen.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "il-rtif.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "pass5p1.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "pass5p2.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "pass5p2-instructions.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "pass5p2-ops.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "peepopt.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "il-src2string.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "il-sourcefile.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "dumpheap-il.sch"))
  (load (make-filename *larceny-root* "Asm" "IL" "dumpheap-extra.sch")))

(reloadil)
(peephole-optimization #f)

(define (scheme->app file)
  (let [(base (rewrite-file-type file '(".sch" ".scm" ".mal") ""))]
    (scheme->il file)
    (create-application base (list (string-append base ".manifest")))
    (twobit-format (current-output-port)
                   "  application IL file -> ~s~%"
                   (string-append base ".il"))))

(define (scheme->il filename)
  (twobit-format (current-output-port) "Source file: ~s~%" filename)
  (if (file-type=? filename ".mal")
      (mal->il filename)
      (sch->il filename)))

(define (sch->il filename)
  (let ((lap-name (rewrite-file-type filename '(".scm" ".sch") ".lap")))
    (compile313 filename)
    (twobit-format (current-output-port) "  compiled -> ~s~%" lap-name)
    (mal->il lap-name)))

(define (mal->il filename)
  (let* ((base (rewrite-file-type filename '(".lap" ".mal") ""))
	 (listing-name (rewrite-file-type base "" ".list"))
	 (lop-name (rewrite-file-type base "" ".lop"))
	 (il-name (rewrite-file-type base "" ".code-il")))
    (if (codegen-option 'listify-write-list-file)
	(begin (listify-reset)
	       (set! listify-filename listing-name)
	       (set! listify-oport (open-output-file listing-name))))
    (assemble313 filename)
    (if (codegen-option 'listify-write-list-file)
	(begin ;; (flush-output-port listify-oport) ;; FIXME: WHY WAS THIS HERE?
	       (close-output-port listify-oport)
	       (listify-reset)
	       (twobit-format (current-output-port)
			      "  listing -> ~s~%" listing-name)))
    (twobit-format (current-output-port) "  assembled -> ~s~%" lop-name)
    
    (create-loadable-file lop-name)
    (twobit-format (current-output-port) "  IL dumped -> ~s~%" il-name)
    ))

(define (ilasm ilfile exefile)
  (define ilasm 
    (build-path "c:" "windows" "microsoft.net" 
                "framework" "v1.0.3705" "ilasm.exe"))
  (define /dev/null-out 
    (make-custom-output-port 
     #f 
     (lambda (s start end buffer-ok?) (- end start))
     void void))
  (define /dev/null-in (make-custom-input-port (lambda (s) eof) #f void))
  (define r 
    (parameterize [(current-output-port /dev/null-out)
                   (current-error-port /dev/null-out)
                   (current-input-port /dev/null-in)]
      (system* ilasm "/nologo" "/quiet" 
               (twobit-format #f "/output=~a" exefile) file)))
  (if r
      (twobit-format (current-output-port)
                     "  EXE assembled -> ~s~%" exefile)
      (mz:error 'il->exe "failed on file ~s" file)))

(define temporary-name-random-number
  (let ((generator (make-pseudo-random-generator))
        (digit-break (expt 36 4)))
    (lambda ()
      (parameterize ((current-pseudo-random-generator generator))
        (begin0 (+ (random digit-break)
                   (* (remainder (current-seconds) digit-break) digit-break)))))))

(define (number->digits number base)
  (define (number->digits-helper number)
    (if (zero? number)
        '()
        (call-with-values (lambda () (quotient/remainder number base))
          (lambda (q r)
            (cons r (number->digits-helper q))))))
  (reverse (number->digits-helper number)))

(define (temporary-name)
  (let ((chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (list->string
     (map (lambda (index)
            (string-ref chars index))
          (number->digits (temporary-name-random-number) 36)))))

(define (call-with-temporary-directory-name thunk)
  (let ((path (build-path (find-system-path 'temp-dir) (temporary-name))))
    (if (directory-exists? path)
        (call-with-temporary-directory-name thunk)
        (thunk path))))

(define *preserve-temporary-directories*
  (make-parameter #t))

(define (call-with-temporary-directory thunk)
  (call-with-temporary-directory-name
    (lambda (name)
      (dynamic-wind (lambda () (make-directory name))
                    (lambda () (thunk name))
                    (lambda () (if (not (*preserve-temporary-directories*))
                                   (delete-directory name)))))))

(define (create-larceny-directory-hierarchy target-directory)
  (make-directory target-directory)
  (for-each (lambda (relative-directory)
              (make-directory (build-path target-directory relative-directory)))
            (larceny-distribution-directories)))

(define (copy-larceny-files from-directory to-directory)
  (for-each (lambda (file)
              (copy-file (build-path from-directory file)
                         (build-path to-directory file)))
            (larceny-distribution-files))
  (cond 
   (#f
    (copy-file (build-path from-directory "bin\\Debug\\CommonLarceny.exe")
	       (build-path to-directory "bin\\Debug\\CommonLarceny.exe"))
    (copy-file (build-path from-directory "bin\\Debug\\CommonLarceny.pdb")
	       (build-path to-directory "bin\\Debug\\CommonLarceny.pdb"))
    (copy-file (build-path from-directory "bin\\Release\\CommonLarceny.exe")
	       (build-path to-directory "bin\\Release\\CommonLarceny.exe")))
   (#t
    (copy-file (build-path from-directory "dotnet.heap.exe")
	       (build-path to-directory "CommonLarceny.exe")))
   ))

(define system
  (let ((op (current-output-port)))
    (lambda args 	
      (write `(system ,@args) op) (newline op)
      (apply 
       (dynamic-require '(lib "process.ss") 'system)
       args))))

(define system*
  (let ((op (current-output-port)))
    (lambda args 	
      (write `(system* ,@args) op) (newline op)
      (apply 
       (dynamic-require '(lib "process.ss") 'system*)
       args))))

(define (guess-7z-path guess-path-suffix)
  (let* ((cmd (string-append 
	       "..\\..\\..\\..\\CL\\Scripts\\which.bat " guess-path-suffix))
	 (cmd-output
	  (let ((os (open-output-string)))
	    (parameterize ((current-output-port os))
	      (system cmd))
	    (get-output-string os)))
	 (cmd-output (substring cmd-output
				0
				(- (string-length cmd-output) 2)))
	 ;(cmd-output (regexp-replace " " cmd-output "\\\\ "))
	 )
    (cond ((regexp-match "not found" cmd-output)
	   #f)
	  (else cmd-output))))

(define (find-7z-path)
  (cond ((guess-7z-path "7z.exe"))
	((guess-7z-path "7-Zip\\7z.exe"))
	(else (error 'find-7z-path 
		     "Need more paths to guess to find 7z."))))

(define (find-7z-executable)
  (string-append (find-7z-path) "7z.exe"))

(define path->string
  (cond ((equal? (version) "209") (lambda (x) x))
	(else path->string)))


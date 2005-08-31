(load "..\\..\\..\\files.ss")

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
  (copy-file (build-path from-directory "bin\\Debug\\CommonLarceny.exe")
             (build-path to-directory "bin\\Debug\\CommonLarceny.exe"))
  (copy-file (build-path from-directory "bin\\Debug\\CommonLarceny.pdb")
             (build-path to-directory "bin\\Debug\\CommonLarceny.pdb"))
  (copy-file (build-path from-directory "bin\\Release\\CommonLarceny.exe")
             (build-path to-directory "bin\\Release\\CommonLarceny.exe")))

(define (setup-distribution-directory distribution-directory)
  (if (not (directory-exists? distribution-directory))
      (make-directory distribution-directory))
  (if (file-exists? (build-path distribution-directory "LarcenySDK.tar"))
      (delete-file (build-path distribution-directory "LarcenySDK.tar"))))

(define (distribute-larceny source-directory)
  (let ((distribution-directory (build-path source-directory "Distribution")))
    (setup-distribution-directory distribution-directory)
    (call-with-temporary-directory
     (lambda (tempdir)
       (let ((target-directory (build-path tempdir "CommonLarceny")))
         (create-larceny-directory-hierarchy target-directory)
         (display "Copying files...") (newline)
         (copy-larceny-files source-directory target-directory)
         (parameterize ((current-directory tempdir))
           ((dynamic-require '(lib "process.ss") 'system)
            (string-append "C:\\Progra~1\\7-Zip\\7z.exe a -ttar -r \""
                                  (path->string (build-path distribution-directory "LarcenySDK.tar"))
                                  "\" CommonLarceny\\*")))
         (parameterize ((current-directory distribution-directory))
           ((dynamic-require '(lib "process.ss") 'system)
            (string-append "C:\\Progra~1\\7-Zip\\7z.exe a -tgzip \""
                                  (path->string (build-path distribution-directory "LarcenySDK.tgz"))
                                  "\" LarcenySDK.tar"))))))))

(distribute-larceny (build-path (current-directory) "..\\..\\..\\..\\"))

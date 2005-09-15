(require (lib "errortrace.ss" "errortrace"))
(load "..\\..\\..\\files.ss") 
(load "bundle-common.ss")

(define (setup-distribution-directory distribution-directory)
  (if (not (directory-exists? distribution-directory))
      (make-directory distribution-directory))
  (if (file-exists? (build-path distribution-directory "LarcenySDK.plt"))
      (delete-file (build-path distribution-directory "LarcenySDK.plt"))))

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
           ((dynamic-require '(lib "pack.ss" "setup") 'pack)
            (build-path distribution-directory "LarcenySDK.plt")
            ""
            (list "CommonLarceny")
            '()
            (lambda (file) #t)
            #t
            'file
            #f
            #f))
         )))))

(distribute-larceny (build-path (current-directory) "..\\..\\..\\..\\"))

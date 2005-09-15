(load "..\\..\\..\\files.ss")
(load "bundle-common.ss")

(define (setup-distribution-directory distribution-directory)
  (if (not (directory-exists? distribution-directory))
      (make-directory distribution-directory))
  (if (file-exists? (build-path distribution-directory "LarcenySDK.tar"))
      (delete-file (build-path distribution-directory "LarcenySDK.tar"))))

(define (distribute-larceny source-directory)
  (let ((7z-exec (find-7z-executable))
	(distribution-directory (build-path source-directory "Distribution")))
    (setup-distribution-directory distribution-directory)
    (call-with-temporary-directory
     (lambda (tempdir)
       (let ((target-directory (build-path tempdir "CommonLarceny")))
         (create-larceny-directory-hierarchy target-directory)
         (display "Copying files...") (newline)
         (copy-larceny-files source-directory target-directory)
         (parameterize ((current-directory tempdir))
           (system*
	     7z-exec "a" "-ttar" "-r"
	     (path->string (build-path distribution-directory "LarcenySDK.tar"))
	     "CommonLarceny\\*"))
         (parameterize ((current-directory distribution-directory))
           (system*
	    7z-exec "a" "-tgzip"
	    (path->string (build-path distribution-directory "LarcenySDK.tgz"))
	    "LarcenySDK.tar")))))))

(distribute-larceny (build-path (current-directory) "..\\..\\..\\..\\"))

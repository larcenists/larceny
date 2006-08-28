(require (lib "errortrace.ss" "errortrace"))
(load "..\\..\\..\\files.ss")
(load "bundle-common.ss")

(define (setup-distribution-directory distribution-directory)
  (if (not (directory-exists? distribution-directory))
      (make-directory distribution-directory))
  (if (file-exists? (build-path distribution-directory "LarcenySDK.zip"))
      (delete-file (build-path distribution-directory "LarcenySDK.zip"))))

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
	    7z-exec "a" "-tzip" "-r" 
                                  (path->string (build-path distribution-directory "LarcenySDK.zip"))
				  "CommonLarceny\\*")))))))

(distribute-larceny (build-path (current-directory) "..\\..\\..\\..\\"))

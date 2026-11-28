;;; SRFI 176

(define (version-alist)
  (let* ((info (features))
         (syms (filter symbol? info))
         (srfis (filter (lambda (x)
                          (and (list? x)
                               (not (null? x))
                               (eq? (car x) 'srfi)
                               (not (null? (cdr x)))))
                        info))
         (srfi-numbers (map cadr srfis))
         (sysfeatures (system-features)))

    (define (get thing label)
      (let ((x (assq thing sysfeatures)))
        (if x
            (list label (cdr x))
            #f)))

    (let* ((major    (get 'larceny-major-version "MAJOR"))
           (minor    (get 'larceny-minor-version "MINOR"))
           (os       (get 'os-name "OS"))
           (os-major (get 'os-major-version "OSMAJOR"))
           (os-minor (get 'os-minor-version "OSMINRO"))
           (os-arch  (get 'arch-name "OSARCH"))
           (encoding (get 'string-representation "ENCODING"))
           (gc       (get 'gc-technology 'larceny.gc))
           (wordsize (get 'arch-word-size 'larceny.wordsize))
           (version  (list 'version
                           (string->number
                            (string-append (number->string (cadr major))
                                           "."
                                           (number->string (cadr minor))))))
           (uname    (if (and os os-major os-minor os-arch)
                         (list 'os.uname
                               (cadr os)
                               (string-append
                                (number->string (cadr os-major))
                                "."
                                (number->string (cadr os-minor)))
                               (cadr os-arch))
                         #f))
           (encoding (cond ((not encoding) #f)
                           ((eq? (cadr encoding) 'flat1)
                            '(encodings latin-1 iso-8859-1))
                           ((eq? (cadr encoding) 'flat4)
                            '(encodings utf-8 utf-16 utf-32))
                           (else #f))))
                         
      (list-deep-copy
       (filter (lambda (x) x)
               (list '(release.name "Larceny")
                     version
                     '(scheme.id larceny)
                     '(website "https://larcenists.org")
                     '(languages scheme r5rs r6rs r7rs)
                     encoding
                     wordsize
                     gc
                     uname
                     (cons 'scheme.features syms)
                     (cons 'scheme.srfi srfi-numbers)))))))

(define (list-deep-copy x)
  (cond ((pair? x)
         (cons (list-deep-copy (car x))
               (list-deep-copy (cdr x))))
        ((string? x)
         (string-copy x))
        (else x)))

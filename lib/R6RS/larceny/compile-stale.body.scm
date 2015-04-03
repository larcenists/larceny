;;; FIXME: unfinished; computes library dependencies, but doesn't compile

(define (compile-stale . files)
  (%compile-stale-libraries files))

;;; A .slfasl file is stale if and only if its source file has
;;; been modified since the .slfasl file was last modified.
;;;
;;; A .slfasl file whose source file cannot be located is not
;;; considered stale.  (Rationale:  This allows libraries to
;;; be supplied in compiled form without their source code.
;;; That's risky, however, because there is no way to recompile
;;; the missing source code if any of the files they depend upon
;;; are recompiled.)

;;; Given a list of file names containing R7RS/R6RS libraries or
;;; programs, attempts to compile those files.
;;;
;;; If any of the named files depends upon a library that cannot
;;; be located within the current require path after extending
;;; that path by adding the current directory, an error message
;;; will be printed and no files will be compiled.
;;;
;;; If any of the named files depends upon a stale file that lies
;;; outside of the current directory, an error message will be
;;; printed and no files will be compiled.
;;;
;;; In the process of compiling the named files, compile-stale
;;; will also attempt to compile all library source files X
;;; within the current directory such that
;;;
;;;     one of the named files depends upon X, and X has not
;;;     been compiled
;;;
;;;     one of the named files depends upon X, and the compiled
;;;     form of X is stale
;;;
;;;     X depends upon any of files that will be compiled as
;;;     a consequence of these rules
;;;
;;; If the list of named files is empty, no files will be compiled.

;;; Algorithm:
;;;     Locate all files upon which the named files depend.
;;;     If any cannot be located, then stop (with error message).
;;;     Locate all stale compiled files upon which the named files depend.
;;;     If any of those stale files lie outside the current directory,
;;;         then stop (with error message).
;;;     Locate all available libraries that depend upon one of the
;;;         named or stale files.
;;;     If any of those libraries lie outside the current directory,
;;;         then stop (with error message).
;;;     The files to be compiled consist of
;;;         the named files
;;;         the stale files upon which a named file depends
;;;         the files within the current directory that depend upon
;;;             one of the named or stale files
;;;     Sort the files to be compiled so every file will be compiled
;;;         before all files that depend upon it.
;;;     Compile the files in that order.
;;;     If any compilation fails, restore all of the compiled files
;;;         to their previous state.

(define (%compile-stale-libraries filenames)
  (%compile-stale-libraries1 filenames))

(define (%compile-stale-libraries-FIXME filenames)
  (let* ((dir (current-directory))
         (dirs (current-require-path))
         (dirs (if (member dir dirs) dirs (cons dir dirs))))
    (parameterize ((current-require-path dirs))
     (%compile-stale-libraries1 filenames))))

(define (complain-about-file filename)
  (error 'compile-stale
         "file contains a malformed library or top-level program"
         filename))

;;; larceny:available-source-libraries returns an association list
;;; in which each entry is of the form
;;;
;;;     (<library> <exports> <imports> <filename> <multiple>)
;;;
;;; where
;;;
;;;     <library> is the name of a library
;;;     <exports> is its export form
;;;     <imports> is its import form
;;;     <filename> is the source file that defines the library
;;;     <multiple> is true if two distinct files define the library
;;;
;;; The following procedure computes similar entries from the named
;;; files to be compiled.  For top-level programs, the <library>
;;; will be an empty list.

(define (%compute-entries-for-files filenames)

  (define (okay? keyword form)
    (eq? keyword (car form)))

  (define (%compute-entries-for-port p filename entries)
    (let loop ((library (read p))
               (entries entries))
      (cond ((eof-object? library)
             entries)
            ((not (list? library))
             (complain-about-file filename))
            ((and (<= 4 (length library))
                  (memq (car library) *library-keywords*)
                  (let ((name (cadr library))
                        (exports (caddr library))
                        (imports (cadddr library)))
                    (and (pair? name)
                         (list? name)
                         (okay? (car name) name)
                         (okay? 'export exports)
                         (okay? 'import imports)
                         (let* ((filename (larceny:absolute-path filename)))
                           (loop (read p)
                                 (cons (list name exports imports filename #f)
                                       entries)))))))
            ((and (<= 1 (length library))
                  (okay? 'import library))
             (let* ((imports library) ; it's really just an import form
                    (filename (larceny:absolute-path filename)))
               (cons (list '() '(export) imports filename #f)
                     entries)))
            (else
             (complain-about-file filename)))))

  (if (null? filenames)
      '()
      (let ((filename (car filenames))
            (filenames (cdr filenames)))
        (append (call-with-input-file
                 filename
                 (lambda (p)
                   (%compute-entries-for-port p filename '())))
                (%compute-entries-for-files filenames)))))

(define (%compile-stale-libraries1 filenames)
  (let* ((libs (larceny:available-source-libraries))
         (to-compile (%compute-entries-for-files filenames))
         (libs (map (lambda (lib)
                      (cons (larceny:libname-without-version (car lib))
                            (cdr lib)))
                    libs))
         (to-compile (map (lambda (lib)
                            (cons (larceny:libname-without-version (car lib))
                                  (cdr lib)))
                          to-compile))
         (lib-table (make-hashtable equal-hash equal?))
         (comp-table (make-hashtable equal-hash equal?))
         (dependency-table (make-hashtable equal-hash equal?))
         (counter 0))
    (for-each (lambda (lib)
                (let* ((name (car lib))
                       (probe (hashtable-ref lib-table name #f)))
                  (hashtable-set! lib-table
                                  name
                                  (if probe
                                      (list name
                                            (cadr lib)   ; exports
                                            (caddr lib)  ; imports
                                            (cadddr lib) ; filename
                                            #t)
                                      lib))))
              libs)
    (for-each (lambda (lib/pgm)
                (let* ((name (car lib/pgm))
                       (name (if (null? name)
                                 (begin (set! counter (+ 1 counter))
                                        (list '#(program) counter))
                                 name))
                       (lib/pgm (cons name (cdr lib/pgm))))
                  (hashtable-set! comp-table (car lib/pgm) lib/pgm)))
              to-compile)

    ;; These two libraries are defined within r6rs-standard-libraries.sch

    (hashtable-set! dependency-table '(rnrs base) '())
    (hashtable-set! dependency-table '(rnrs io simple) '())

    (%compile-stale-libraries2 lib-table
                               comp-table
                               dependency-table)))

(define (%compile-stale-libraries2 lib-table comp-table dependency-table)

  (define (dependencies name)
    (let ((probe (hashtable-ref dependency-table name #f)))
      (cond ((eq? probe #t)
             (error 'compile-stale
                    "import graph contains cycles"
                    name)
             '())
            (probe probe)
            (else
             (hashtable-set! dependency-table name #t)
             (let* ((lib (hashtable-ref lib-table name #f))
                    (lib (or lib (hashtable-ref comp-table name #f))))
               (if (not lib)
                   (begin (display "library not found: ")
                          (write name)
                          (newline)
                          (hashtable-set! dependency-table name '())
                          '())
                   (let ((directly-imported
                          (make-lset
                           (map import-spec->libname
                                (cdr (caddr lib))))))
                     (let loop ((directly-imported directly-imported)
                                (depends-upon      directly-imported))
                       (if (null? directly-imported)
                           (begin (hashtable-set! dependency-table
                                                  name
                                                  depends-upon)
                                  depends-upon)
                           (let* ((import1 (import-spec->libname
                                            (car directly-imported)))
                                  (depends (dependencies import1)))
                             (loop (cdr directly-imported)
                                   (lset-union equal?
                                               depends
                                               depends-upon))))))))))))

  (define (make-lset bag)
    (if (null? bag)
        bag
        (let ((set (make-lset (cdr bag))))
          (if (member (car bag) set)
              set
              (cons (car bag) set)))))

  ;; FIXME: hard-wired special treatment for core and primitives isn't right

  (define (import-spec->libname spec)
    (cond ((or (not (list? spec))
               (< (length spec) 2))
           spec)
          ((memq (car spec)
                 '(only except prefix rename             ; R7RS/R6RS
                   for library))                         ; R6RS only
           (import-spec->libname (cadr spec)))
          ((memq (car spec)
                 '(core primitives))                     ; Larceny-specific
           '(rnrs base))
          (else
           (larceny:libname-without-version spec))))

  (vector-for-each dependencies (hashtable-keys comp-table))

  (vector-for-each (lambda (name)
                     (write name)
                     (newline)
                     (for-each (lambda (libname)
                                 (display "    ")
                                 (write libname)
                                 (newline))
                               (hashtable-ref dependency-table name '())))
                   (hashtable-keys dependency-table)))

;;; Given no arguments, re-compiles all libraries and programs
;;; found within the current directory, provided their names end
;;; with .sld, .sls, or .sps
;;;
;;; Given strings naming files within the current directory that
;;; contain libraries or programs to be compiled, compiles those
;;; files if it is safe to do or generates a warning message that
;;; explains the compilation might be unsafe.
;;; Returns true iff all of the given files are compiled.
;;;
;;; It is safe to compile the given files iff, for them and for
;;; each of the libraries on which they depend, the library
;;;
;;;     has already been compiled and is not stale
;;;         and does not depend on any stale or as-yet-uncompiled file
;;;  or resides within the current directory
;;;         and no library outside of the current directory (but
;;;         still within the library path) depends on it
;;;
;;; Dependency is transitive, so the second of those conditions
;;; implies that no library outside of the current directory depends
;;; on any files within the current directory that will be compiled
;;; by compile-stale.
;;;
;;; FIXME: the dependency graph is not perfectly reliable because
;;;     the graph is calculated using cond-expand features recognized
;;;         when compile-stale is called, which may be different from
;;;         cond-expand features recognized at run time
;;;     libraries that are intended for use only by another library
;;;         or program found within the same file often have names
;;;         that prevent them from being found by Larceny's mapping
;;;         from library names to files
;;;
;;; FIXME: several calls to member should be replaced by hashtables.
;;;
;;; FIXME: named files are not ordered properly, so a named top-level
;;; program can be compiled before one of the named libraries it
;;; imports.

(define (compile-stale . files)
  (if (null? files)
      (compile-stale-libraries)
      (%compile-stale-libraries files finish-cautiously)))

;;; Like compile-stale, but doesn't re-compile everything in the
;;; current directory when no files are given.

(define (compile-stale-cautiously . files)
  (%compile-stale-libraries files finish-cautiously))

;;; Like compile-stale, but compiles the given files even if there
;;; are libraries outside of the current directory that depend on
;;; one of the files that will be compiled.

(define (compile-stale-regardless . files)
  (%compile-stale-libraries files finish-regardless))

;;; Like compile-stale-regardless, but compiles even if some of the
;;; files to be compiled lie outside the current directory.

(define (compile-stale-recklessly . files)
  (%compile-stale-libraries files finish-recklessly))

;;; Given a list of file names containing R7RS/R6RS libraries or
;;; programs, attempts to compile those files.
;;;
;;; If any of the named files depends upon a library that cannot
;;; be located within the current require path, an error message
;;; will be printed and no files will be compiled.
;;;
;;; If any of the named files depends upon a stale file that lies
;;; outside of the current directory, an error message will be
;;; printed and no files will be compiled.
;;;
;;; Will also attempt to compile all library source files X
;;; within the current directory such that
;;;
;;;     one of the named files depends upon X, and X has not
;;;     been compiled
;;;
;;;     one of the named files depends upon X, and the compiled
;;;     form of X is stale
;;;
;;;     X depends upon any files that will be compiled as
;;;     a consequence of these rules
;;;
;;; If the list of named files is empty, no files will be compiled.

;;; Algorithm:
;;;     Locate all files upon which the named files depend.
;;;     If any cannot be located, then stop (with error message).
;;;     Locate all stale or as-yet-uncompiled files upon which the
;;;         named files depend.
;;;     If any of those stale or as-yet-uncompiled files lie outside
;;;         the current directory, then stop (with error message).
;;;     Locate all available libraries that depend upon one of the
;;;         named or stale files.
;;;     If any of those libraries lie outside the current directory,
;;;         and we're being cautious, then stop (with error message).
;;;         (This is checked by the finish argument.)
;;;     The files to be compiled consist of
;;;         the named files
;;;         the stale files or as-yet-uncompiled files upon which a
;;;             named file depends
;;;         the files upon which a named file depends that depend
;;;             upon a named, stale, or as-yet-uncompiled file upon
;;;             upon which a named file depends
;;;         the files within the current directory that depend upon
;;;             a named, stale, or as-yet-uncompiled files upon which
;;;             a named file depends
;;;     Sort the files to be compiled so every file will be compiled
;;;         before all files that depend upon it.
;;;     Compile the files in that order.
;;;
;;; FIXME: should restore all compiled files to their previous state
;;;     if any compilation fails.

(define (%compile-stale-libraries filenames finish)
  (%compile-stale-libraries1 filenames finish))

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
;;; These entries are compatible with the entries computed by
;;; larceny:available-source-libraries, so the accessors defined in
;;; lib/R7RS/r7rs-cond-expander.sch can be used here:
;;;
;;;     larceny:library-entry-name
;;;     larceny:library-entry-exports
;;;     larceny:library-entry-imports
;;;     larceny:library-entry-filename
;;;     larceny:library-entry-multiple?
;;;
;;; The following procedure computes similar entries from the named
;;; files to be compiled.  For top-level programs, the <library>
;;; will be an empty list.

(define (%compute-entries-for-files filenames)

  (define (okay? keyword form)
    (eq? keyword (car form)))

  (define (%compute-entries-for-port p filename entries)
    (let ((path (larceny:absolute-path filename)))
      (let loop ((library (read p))
                 (entries entries))
        (cond ((eof-object? library)
               entries)
              ((not (list? library))
               (complain-about-file filename))
              ((larceny:library->entry library path)
               =>
               (lambda (entry)
                 (loop (read p)
                       (cons entry entries))))
              ((and (<= 1 (length library))
                    (okay? 'import library))
               (let* ((imports library)) ; it's really just an import form
                 (cons (larceny:make-library-entry '()
                                                   '(export)
                                                   imports
                                                   path
                                                   #f)
                       entries)))
              (else
               (complain-about-file filename))))))

  (if (null? filenames)
      '()
      (let ((filename (car filenames))
            (filenames (cdr filenames)))
        (append (call-with-input-file
                 filename
                 (lambda (p)
                   (%compute-entries-for-port p filename '())))
                (%compute-entries-for-files filenames)))))

(define (complain-about-file filename)
  (error 'compile-stale
         "file contains a malformed library or top-level program"
         filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Phase 1: collects dependency information for the files to compile
;;; and for all available libraries, and then proceeds to phase 2.

(define (%compile-stale-libraries1 filenames finish)
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

    ;; Compute comp-table.

    (for-each (lambda (lib/pgm)
                (let* ((name (car lib/pgm))
                       (name (if (null? name)
                                 (begin (set! counter (+ 1 counter))
                                        (list '#(program) counter))
                                 name))
                       (lib/pgm (cons name (cdr lib/pgm))))
                  (hashtable-set! comp-table
                                  (larceny:library-entry-name lib/pgm)
                                  lib/pgm)))
              to-compile)

    ;; Compute lib-table.

    (for-each (lambda (lib)
                (let* ((name (car lib))
                       (probe (hashtable-ref lib-table name #f)))
                  (hashtable-set! lib-table
                                  name
                                  (if (not probe)
                                      lib
                                      (larceny:make-library-entry
                                       name
                                       (larceny:library-entry-exports lib)
                                       (larceny:library-entry-imports lib)
                                       (larceny:library-entry-filename lib)
                                       #t)))))
              (union (vector->list (hashtable-values comp-table))
                     libs))

    ;; FIXME: Is this necessary?

    (for-each (lambda (lib)
                (hashtable-set! dependency-table lib '()))
              (base-libraries))

    (%compile-stale-libraries2 lib-table
                               comp-table
                               dependency-table
                               finish)))

;;; Phase 2: calculates dependency graph and then proceeds to phase 3.
;;;
;;; The lib-table maps names of available libraries to library entries.
;;; The comp-table maps names of files to be compiled to library entries.
;;; The dependency table is filled in by phase 2: it maps names of
;;; libraries and programs to lists of the libraries upon which they depend.
;;; For each of those lists, every library depends only upon libraries
;;; that follow it in the list.

(define (%compile-stale-libraries2 lib-table
                                   comp-table
                                   dependency-table
                                   finish)

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
                          (make-set
                           (map import-spec->libname
                                (cdr (larceny:library-entry-imports lib))))))
                     (let loop ((directly-imported directly-imported)
                                (depends-upon      '()))
                       (if (null? directly-imported)
                           (begin (hashtable-set! dependency-table
                                                  name
                                                  depends-upon)
                                  depends-upon)
                           (let* ((import1 (import-spec->libname
                                            (car directly-imported)))
                                  (depends (dependencies import1)))
                             (loop (cdr directly-imported)
                                   (union (cons import1 depends)
                                          depends-upon))))))))))))

  (vector-for-each dependencies (hashtable-keys lib-table))

  (if (debugging?)
      (vector-for-each (lambda (name)
                         (write name)
                         (newline)
                         (let* ((entry (hashtable-ref lib-table name #f))
                                (entry (or entry
                                           (hashtable-ref comp-table name #f)))
                                (filename
                                 (if entry
                                     (larceny:library-entry-filename entry)
                                     "")))
                           (if (obviously-stale? filename)
                               (display " S  ")
                               (display "    "))
                           (write filename)
                           (newline))
                         (for-each (lambda (libname)
                                     (display "    ")
                                     (write libname)
                                     (newline))
                                   (hashtable-ref dependency-table name '())))
                       (hashtable-keys dependency-table)))

  (%compile-stale-libraries3 lib-table comp-table dependency-table finish))

;;; Phase 3: identifies the files that need to be compiled, and proceeds
;;; to phase 4.  The files that need to be compiled are
;;;
;;;         the named files
;;;         the stale files upon which a named file depends
;;;         the files imported (directly or indirectly) by a named file
;;;             that import (directly or indirectly) a library defined
;;;             in a stale file or in one of the named files
;;;
;;; Phase 4 will then check whether any available libraries that are not
;;; in the list of files to be compiled depend upon a file to be compiled.
;;;
;;; The lib-table maps names of available libraries to library entries.
;;; The comp-table maps names of files to be compiled to library entries.
;;; The dependency table maps names of libraries and programs to lists
;;; of the libraries upon which they depend.  For each of those lists,
;;; every library depends only upon libraries that follow it in the list.


(define (%compile-stale-libraries3 lib-table
                                   comp-table
                                   dependency-table
                                   finish)

  ;; Returns the set of libraries that need to be compiled.

  (define (libraries-to-compile)

    ;; Memoization for the stale? procedure.

    (define stale-cache (make-hashtable equal-hash equal?))

    ;; Given a list of entries for files to be compiled,
    ;; returns a list of the stale or as-yet-uncompiled libraries
    ;; on which they depend.  That list includes the libraries
    ;; defined in the files to compiled.

    (define (stale-libraries entries)
      (if (null? entries)
          '()
          (union (let* ((entry (car entries))
                        (lib (larceny:library-entry-name entry)))
                   (cons lib
                         (filter
                          (lambda (lib)
                            (let* ((entry (hashtable-ref lib-table lib #f))
                                   (file
                                    (and
                                     entry
                                     (larceny:library-entry-filename entry))))
                              (and file
                                   (stale? file
                                           lib-table
                                           dependency-table
                                           stale-cache))))
                          (hashtable-ref dependency-table lib))))
                 (stale-libraries (cdr entries)))))

    ;; Given a list of libraries to be compiled,
    ;; Returns a list of all available libraries that depend upon
    ;; one of the libraries to be compiled.

    (define (libs-to-compile libs0)
      (let loop ((libs (vector->list (hashtable-keys lib-table)))
                 (to-compile libs0))
        (cond ((null? libs)
               to-compile)
              ((let* ((lib (car libs))
                      (prior-libs (hashtable-ref dependency-table lib)))
                 (any (lambda (lib) (member lib libs0))
                      prior-libs))
               (loop (cdr libs)
                     (union (list (car libs)) to-compile)))
              (else
               (loop (cdr libs) to-compile)))))

    (libs-to-compile
     (stale-libraries
      (vector->list
       (hashtable-values comp-table)))))

  ;; Given two sets of libraries to compile and a list of libraries
  ;; that can be compiled in reverse order of the list, returns a
  ;; list containing all libraries in those three arguments, in an
  ;; order that compiles each library only after all libraries on
  ;; which it depends have been compiled.
  ;;
  ;; Note: circular dependencies would have been detected by now,
  ;; so compilation-order will terminate.

  (define (compilation-order to-compile not-ready ready)
    (cond ((and (null? to-compile)
                (null? not-ready))
           (reverse ready))
          ((null? to-compile)
           (compilation-order not-ready '() ready))
          ((every (lambda (lib)
                    (and (not (member lib to-compile))
                         (not (member lib not-ready))))
                  (hashtable-ref dependency-table (car to-compile)))
           (compilation-order (cdr to-compile)
                              not-ready
                              (cons (car to-compile) ready)))
          (else
           (compilation-order (cdr to-compile)
                              (cons (car to-compile) not-ready)
                              ready))))

  (let* ((libs (libraries-to-compile))
         (libs (compilation-order libs '() '()))
         (minimal-dependencies
          (let ((ht (make-hashtable equal-hash equal?)))
            (vector-for-each (lambda (lib-or-program)
                               (let ((dependencies
                                      (hashtable-ref dependency-table
                                                     lib-or-program)))
                                 (for-each (lambda (lib)
                                             (hashtable-set! ht lib #t))
                                           dependencies)))
                             (hashtable-keys comp-table))
            ht))
         (minimal-libs
          (filter (lambda (lib)
                    (hashtable-ref minimal-dependencies lib #f))
                  libs))
         (libs->files
          (lambda (libs)
            (filter (lambda (filename)
                      (> (string-length filename) 0))
                    (make-set-taking-first
                     (append (map (lambda (lib)
                                    (larceny:library-entry-filename
                                     (hashtable-ref lib-table lib)))
                                  libs)
                             (map larceny:library-entry-filename
                                  (vector->list
                                   (hashtable-values comp-table))))))))
         (files         (libs->files libs))
         (minimal-files (libs->files minimal-libs))
         (other-files (filter (lambda (filename)
                                (not (member filename minimal-files)))
                              files)))

    (finish libs minimal-files other-files)))

(define (write-summary-message libs minimal-files other-files)
  (if (not (null? libs))
      (begin
       (display "\n\nLibraries and programs to be compiled:\n\n")
       (for-each (lambda (lib)
                   (display "    ")
                   (write lib)
                   (newline))
                 libs)))
  (if (not (null? minimal-files))
      (begin
       (display "\n\nMinimal files to be compiled:\n\n")
       (for-each (lambda (file)
                   (display "    ")
                   (write file)
                   (newline))
                 minimal-files)))
  (if (not (null? other-files))
      (begin
       (display "\n\nOther files that should also be compiled:\n\n")
       (for-each (lambda (file)
                   (display "    ")
                   (write file)
                   (newline))
                 other-files)))
  (newline))

;;; Finishers.

(define (finish-cautiously libs minimal-files other-files)
  (if (null? other-files)
      (finish-regardless libs minimal-files other-files)
      (begin
       (write-summary-message libs minimal-files other-files)
       (display no-files-were-compiled)
       (display because-other-files-message)
       (write-how-to-compile-message minimal-files other-files)
       (display no-files-were-compiled)
       #f)))

(define (finish-regardless libs minimal-files other-files)
  (write-summary-message libs minimal-files other-files)
  (let ((dir (current-directory)))
    (if (every (lambda (file)
                 (textual-prefix? dir file))
               (append minimal-files other-files))
        (begin (for-each compile-file minimal-files)
               #t)
        (begin
         (display no-files-were-compiled)
         (display because-outside-current-directory-message)
         (write-how-to-compile-message minimal-files other-files)
         (display no-files-were-compiled)
         #f))))

(define (finish-recklessly libs minimal-files other-files)
  (write-summary-message libs minimal-files other-files)
  (for-each compile-file minimal-files)
  #t)

(define no-files-were-compiled
  "\n\nNO FILES WERE COMPILED\n\n")

(define because-other-files-message
  "    because compiling would render previously compiled files stale\n\n")

(define because-outside-current-directory-message
  "    because some files to be compiled are outside current directory\n\n")

(define (write-how-to-compile-message minimal-files other-files)
  (display "To force compilation, import (larceny compiler) and evaluate\n\n")
  (pretty-print
   `(for-each compile-file
              '(,@(append minimal-files other-files)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Given lists x and y with no repetitions (in the sense of equal?),
;;; returns a list that's equal to (make-set (append x y)).
;;;
;;; FIXME: average time is O(n) but could be made faster

(define (union x y)
  (make-set (append x y)))

;;; Given a list of objects,
;;; returns that list from which repetitions (in the sense of equal?)
;;; have been eliminated by removing all but the first occurrence of
;;; each object in the list.

(define (make-set bag)
  (let ((ht (make-hashtable equal-hash equal?)))
    (let loop ((bag bag)
               (set '()))
      (cond ((null? bag)
             (reverse set))
            ((hashtable-ref ht (car bag) #f)
             (loop (cdr bag) set))
            (else
             (hashtable-set! ht (car bag) #t)
             (loop (cdr bag) (cons (car bag) set)))))))

;;; Given a list of strings, returns the set of strings obtained
;;; by keeping the first occurrence of each string and otherwise
;;; preserving the order of the list.

(define (make-set-taking-first bag)
  (let ((ht (make-hashtable string-hash string=?)))
    (let loop ((bag bag)
               (set '()))
      (cond ((null? bag)
             (reverse set))
            ((hashtable-ref ht (car bag) #f)
             (loop (cdr bag) set))
            (else
             (hashtable-set! ht (car bag) #t)
             (loop (cdr bag) (cons (car bag) set)))))))

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

;;; A source file is obviously stale if and only if its associated
;;; .slfasl file is stale.
;;;
;;; A source file that cannot be located is not considered stale.
;;; (Rationale:  This allows libraries to be supplied in compiled
;;; form without their source code, and allows for libraries that
;;; aren't written in Scheme.  Supplying libraries only in compiled
;;; form is risky, however, because there is no way to recompile
;;; the missing source code if any of the files they depend upon
;;; are recompiled.)

(define (obviously-stale? srcfile)
  (let ((faslfile (generate-fasl-name srcfile)))
    (and faslfile
         (file-exists? srcfile)
         (file-exists? faslfile)
         (file-newer? srcfile faslfile))))

(define (compiled? srcfile)
  (let ((faslfile (generate-fasl-name srcfile)))
    (and faslfile
         (file-exists? srcfile)
         (file-exists? faslfile))))

;; These two libraries are defined within r6rs-standard-libraries.sch
;; FIXME: shouldn't need so many special cases

(define (base-libraries)
  '((rnrs base)
    (rnrs io simple)))

;;; A source file is stale if and only if it is
;;;
;;;     obviously stale
;;;  or not compiled
;;;  or its .slfasl file is older than the .slfasl of a library it imports
;;;  or it depends upon a stale file
;;;
;;; The optional argument is a hashtable that maps library names
;;; to booleans indicating whether the library is defined by a
;;; stale file.

(define (stale? srcfile lib-table dependency-table . rest)
  (let ((cache (if (null? rest)
                   (make-hashtable equal-hash equal?)
                   (car rest))))
    (or (obviously-stale? srcfile)
        (not (compiled? srcfile))
        (any (lambda (lib)
               (cond ((hashtable-ref cache lib #f)
                      #t)
                     ((not (hashtable-ref cache lib #t))
                      #f)
                     (else
                      (let* ((entry (hashtable-ref lib-table lib #f))
                             (file
                              (and lib
                                   (larceny:library-entry-filename entry)))
                             (file (if (< 0 (string-length file))
                                       file
                                       #f))
                             (result
                              (and file
                                   (or (obviously-stale? file)
                                       (not (compiled? file))
                                       (file-newer?
                                        (generate-fasl-name file)
                                        (generate-fasl-name srcfile))))))
                        (hashtable-set! cache lib result)
                        result))))
             (hashtable-ref dependency-table srcfile '())))))

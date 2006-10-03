; Almost-portable autoloader for Scheme.
; 2001-11-16 / lth
;
; (AUTOLOAD <id> <filename>)                             Syntax
;
;   An AUTOLOAD expression expands to (DEFINE <id> <proc>) where <proc>
;   is a procedure that will load the file named by <filename>, unless
;   the file has already been loaded by AUTOLOAD.  An error is
;   signalled if loading the file does not result in a new value being
;   given to <id>.  After the file has been loaded, the new value of <id>
;   is called with the arguments that were passed to <proc>.
;
;   The file is loaded in the interaction-environment that was in
;   effect when the AUTOLOAD expression was evaluated.
;
;   AUTOLOAD maintains procedure identity and copes with files that
;   define multiple autoloaded procedures.
;
;   A filename can have two forms, indirect and direct.  
;
;   An indirect filename has the form
;       ~logical-directory/relative-path
;   where logical-directory is a string not containing / and 
;   relative-path is a relative path and filename in the operating 
;   system's native filename syntax.  The directory denoted by
;   logical-directory and the relative-path are concatenated to form
;   the name of the file to load.  AUTOLOAD maintains a database of
;   mappings from logical directories to actual filesystem paths.
;   Logical directories are case-insensitive.
;
;   A direct filename is any filename that is not indirect and directly
;   denotes the file to load.
;
;
; (AUTOLOAD (<id> ...) <filename>)                       Syntax
;
;   Shorthand for the obvious sequence of AUTOLOAD definitions.
;
;
; (AUTOLOAD:DEFINE-LOGICAL-DIRECTORY <logical-directory> <directory-spec>)
;                                                        Procedure
;
;   Add a mapping from <logical-directory> (a symbol) to <directory-spec>
;   (a string) in AUTOLOAD's logical directory database.
;
; Bugs/FIXME:
;   * Can't autoload syntax.
;   * Can't autoload variables.
;   * We could adopt a URL syntax for the file names:
;       file:~logical-directory/path     (OK because ~ can't occur in URLs?)
;       file:path
;       http://www.ai.mit.edu/home/shivers/srfi/srfi1.sch
;   * Any way to integrate with REQUIRE?

(define-syntax autoload
  (syntax-rules ()
    ((autoload (?id0 ...) ?filename)
     (begin (autoload ?id0 ?filename) ...))
    ((autoload ?id ?filename)
     (define ?id
       (letrec ((env  (interaction-environment))
                (fn   ?filename)
                (autoloaded 
                 (lambda args
                   (if (eq? ?id autoloaded)
                       (call-without-interrupts
                        (lambda ()
                          (if (eq? ?id autoloaded)
                              (autoload:autoload-file fn env))
                          (if (eq? ?id autoloaded)
                              (error "Autoload of " 
                                     '?id 
                                     " failed: not assigned during loading of "
                                     fn)))))
                   (apply ?id args))))
         autoloaded)))))

(define autoload:autoload-file)
(define autoload:define-logical-directory)

(let ((load load))  ; For sanity's sake.
  
  (define autoload-logical-directories '())
  (define autoloaded-files '())

  (define (autoload-file filename env)
    (let ((filename (expand-filename filename)))
      (if (member filename autoloaded-files)
          (error "AUTOLOAD error: " 
                 filename 
                 " autoloaded multiple times."))
      (set! autoloaded-files (cons filename autoloaded-files))
      (load filename env)))

  (define (define-logical-directory ld path)
    (set! autoload-logical-directories
          (cons (cons (symbol->string ld) path) autoload-logical-directories)))
  
  (define (expand-filename filename)
    (if (not (char=? #\~ (string-ref filename 0)))
        filename
        (let ((n (string-search-char #\/ filename)))
          (if (not n)
              (error "Invalid autoload path spec " filename))
          (let ((ld (substring filename 1 n)))
            (let loop ((dirs autoload-logical-directories))
              (cond ((null? dirs)
                     (error "No autoload directory for the logical directory "
                            ld))
                    ((string-ci=? ld (caar dirs))
                     (string-append (cdar dirs)
                                    (substring filename 
                                               (+ n 1)
                                               (string-length filename))))
                    (else
                     (loop (cdr dirs)))))))))
  
  (define (string-search-char c s)
    (let loop ((i 0))
      (cond ((= i (string-length s)) #f)
            ((char=? c (string-ref s i)) i)
            (else (loop (+ i 1))))))
  
  (set! autoload:autoload-file autoload-file)
  (set! autoload:define-logical-directory define-logical-directory)
  'autoload)

; eof

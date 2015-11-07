;;; This file is the entry point for building a copy of Larceny from
;;; scratch.  It should be R5RS compatible Scheme; part of its job is
;;; to load the implementation-specific extensions to Scheme based on
;;; the host system (as indicated by the user in the 'scheme: keyword
;;; argument to the setup procedure).

(load "src/Build/petit-unix-defns.sch")

;; A KeySpec is one of 
;; (list Symbol 'required)
;; (list Symbol 'with-default Any)
;; (list Symbol 'flag #f)

;; parse-keys : [Listof Any] [Listof KeySpec] (Symbol Any ... -> X) -> X
;; Parses ARGL according to KEYSPECS.  If the result satisf

(define (parse-keys argl keyspecs proc)

  ;; filter : (X -> Bool) [Listof X] -> [Listof X]
  ;; returns list containing only elements from LST satisfying PRED?

  (define (filter pred? lst)
    (let loop ((lst lst))
      (cond ((null? lst) lst) 
            ((pred? (car lst)) (cons (car lst) (loop (cdr lst))))
            (else (loop (cdr lst))))))

  (define spec-key car)
  (define spec-type cadr)
  (define spec-default caddr)
  (define (make-assoc-list)
    (let loop ((argl argl) 
               (required-keys (map spec-key
                                   (filter (lambda (spec) 
                                             (case (spec-type spec)
                                               ((with-default flag) #f)
                                               (else #t)))
                                           keyspecs))))
      (cond ((null? argl)
             (cond ((null? required-keys) '())
                   (else (error "Missing required keys: " required-keys))))
            ((assq (car argl) keyspecs)
             => (lambda (spec) 
                  (define (next rest) 
                    (loop rest
                          (filter (lambda (k) (not (eq? k (spec-key spec))))
                                  required-keys)))
                  (case (spec-type spec)
                    ((flag) 
                     (cons (list (car argl) #t)
                           (next (cdr argl))))
                    (else (if (null? (cdr argl))
                              (error "Key without value: " (car argl))
                              (cons (list (car argl) (cadr argl))
                                    (next (cddr argl))))))))
            (else (error "Unknown Key: " (car argl)
                         " Known Keys: " (map car keyspecs))))))
  (let ((alst (make-assoc-list)))
    (apply proc
           (map (lambda (spec)
                  (cond ((assq (spec-key spec) alst) => cadr)
                        ((case (spec-type spec) 
                           ((with-default flag) #t)
                           (else #f)) (spec-default spec))
                        (else (error "Missing Key: " (spec-key spec)))))
                keyspecs))))

;; Syntactic form for defining procedures that use keyword parameters
;; (as opposed to positional parameters)
;; If you don't pass a keyword+value, the parameter is given
;; the default value of (if #f #f)
;; 
;; Special:
;; FLAG keywords: just passed on their own, with no associated value.
;; WITH-DEFAULT keywords: have a default value defined by the procedure writer
;; REQUIRED keywords: have no default value; calls error if you don't pass them
;; 
;; Examples: 
;; ((lambda-keyed (a: b:) (- a: b:)) 'b: 4 'a: 6) ; ==> 2
;; ((lambda-keyed ((required a:) b:)  (+ a: b:)) 'a: 1)
;;                 ; ==> <<error in call to +>>
;; ((lambda-keyed ((required a:) b:)  (+ a: b:)) 'b: 1)
;;                 ; ==> <<error: Missing required keys: (a:)>>
;; ((lambda-keyed ((with-default 3 a:) b:)  (+ a: b:)) 'b: 1) ; ==> 4
;; ((lambda-keyed ((flag help) a b) (if help "Adds a and b" (+ a b)))
;;                 'a 1 'b 2) ; ==> 3
;; ((lambda-keyed ((flag help) a b) (if help "Adds a and b" (+ a b))) 'help)
;;                 ; ==> "Adds a and b"

(define-syntax lambda-keyed
  (syntax-rules (flag with-default required)
    ((lambda-keyed "help" () (KEYSPECS ...) (ARGS ...) (BODY ...))
     (lambda argl 
       (parse-keys argl (quasiquote (KEYSPECS ...))
                   (lambda (ARGS ...) BODY ...))))

    ((lambda-keyed "help"
       ((with-default DEFAULT KEY-ARG) KEY-ARGS ...)
       (KEYSPECS ...) (ARGS ...) (BODY ...))
     (lambda-keyed "help" (KEY-ARGS ...)
                   ((KEY-ARG with-default (unquote DEFAULT)) KEYSPECS ...)
                   (KEY-ARG ARGS ...) (BODY ...)))

    ((lambda-keyed "help" ((flag KEY-ARG) KEY-ARGS ...)
                   (KEYSPECS ...) (ARGS ...) (BODY ...))
     (lambda-keyed "help" (KEY-ARGS ...)
                   ((KEY-ARG flag #f) KEYSPECS ...)
                   (KEY-ARG ARGS ...) (BODY ...)))

    ((lambda-keyed "help" ((required KEY-ARG) KEY-ARGS ...)
                   (KEYSPECS ...) (ARGS ...) (BODY ...))
     (lambda-keyed "help" (KEY-ARGS ...)
                   ((KEY-ARG required) KEYSPECS ...)
                   (KEY-ARG ARGS ...) (BODY ...)))
    

    ((lambda-keyed "help" (KEY-ARG KEY-ARGS ...)
                   (KEYSPECS ...) (ARGS ...) (BODY ...))
     (lambda-keyed "help" (KEY-ARGS ...)
                   ((KEY-ARG with-default (unquote (if #f #f))) KEYSPECS ...)
                   (KEY-ARG ARGS ...) (BODY ...)))
    
    ((lambda-keyed (KEY-ARGS ...) BODY ...)
     (lambda-keyed "help" (KEY-ARGS ...) () () (BODY ...)))))

(define-syntax define-keyed
  (syntax-rules ()
    ((define-keyed (PROC ARGS ...) BODY ...)
     (define PROC (lambda-keyed (ARGS ...) BODY ...)))))


;; setup : oneof ['help]
;;             -> Void {'scheme: Symbol, 'host: Symbol, ['target: Symbol]}
;;                 -> Void
;; Main entry to set up the build for Petit compiler and runtime.

(define-keyed (setup (with-default 'larceny scheme:)
                     ;; inferred for scheme: 'larceny
                     (with-default #f host:)
                     ;; inherits host: if not given
                     (with-default #f target:)
                     ;; usually safe to leave alone
                     (with-default #f c-compiler:)
                     ;; current choices are: flat1, flat4
                     ;; future choices include: record1, record2
                     (with-default #f string-rep:)
                     (flag help)
                     ;; Options for generated runtime
                     (with-default #t rof-collector:)
                     (flag dof-collector)
                     (flag gclib-large-table)
                     ;; Build host options
                     (flag exit-on-error)
                     (flag quiet-load)
                     (flag verbose-load)
                     (flag always-source)
                     ;; Target variation options
                     (flag native)
                     (flag nasm)
                     (flag sassy)
                     (flag common)
                     ;; Deprecated options
                     (flag code-cov)
                     (flag rebuild-code-cov))
  (define (displn arg) (display arg) (newline))
  (define (help-text) 
    (displn "To setup Larceny, call ")
    (displn "(setup 'scheme: HOST-SCHEME 'host: PLATFORM ['target: PLATFORM])")
    (displn "e.g., (setup 'scheme: 'larceny 'host: 'macosx)")
    (displn "Example host schemes:   'larceny 'mzscheme")
    (displn "Example platforms: 'macosx 'solaris"))

  (set! *exit-on-error* exit-on-error)

  (cond (help (help-text)))

  ;; Fail fast in case user didn't know 'scheme: defaults to 'larceny
  ;; This expression should have no side-effects on Larceny, but
  ;; should fail spectacularly on almost any other imaginable Scheme.

  (cond ((eq? scheme: 'larceny)
         ((environment-get (interaction-environment) 'current-larceny-root))))

  (set! *always-source* always-source)
  (set! *runtime:additional-features* 
        (append 
         (if dof-collector     '("DOF_COLLECTOR") '())
         (if rof-collector:    '("ROF_COLLECTOR") '())
         (if gclib-large-table '("GCLIB_LARGE_TABLE") '())
         *runtime:additional-features*))

  (cond (quiet-load   (set! *verbose-load* #f))
        (verbose-load (set! *verbose-load* #t)))
                        
  ;; If on Larceny, allow more leeway for omitted options.  Need to
  ;; add arch: to keys above (w/ reasonable implication logic).

  (cond ((and (eq? scheme: 'larceny)
              (not host:))
         (let ((os-name (cdr (assq 'os-name (system-features))))
               (arch-name (cdr (assq 'arch-name (system-features))))
               (arch-endianness
                (cdr (assq 'arch-endianness (system-features)))))
           (cond 

            ((and (equal? os-name "Linux") (eq? arch-endianness 'little)) 
             (if (equal? arch-name "ARM")
                 (set! host: 'linux-arm-el)
                 (set! host: 'linux-el)))
            
            ((and (equal? os-name "Linux") (eq? arch-endianness 'big)) 
             (set! host: 'linux-be))
            
            ((and (equal? os-name "SunOS") (eq? arch-endianness 'big))
             (set! host: 'solaris))

            ((and (equal? os-name "Win32") (eq? arch-endianness 'little)) 
             (set! host: 'win32))

            ((and (equal? os-name "MacOS X") (eq? arch-endianness 'little)) 
             (set! host: 'macosx-el))

            ((and (equal? os-name "MacOS X") (eq? arch-endianness 'big)) 
             (set! host: 'macosx))
             
            ;; Fill in other host: clauses here
            
            ))))
  
  (cond ((and (not help) (or (not scheme:) (not host:)))
         (help-text))
        (else
         (let* ((host:   (case host: 
                           ;; Felix prefers "linux86" and "macosx86",
                           ;; so those aliases are supported here
                           ;;
                           ;; Lars notes that "linux-el" as a synonym for "linux-x86-el"
                           ;; is unreasonable given that Linux runs on so many platforms.
                           ((linux86) 'linux-el)
                           ((macosx86) 'macosx-el)
                           (else host:)))
                (target: (cond (target: target:)
                               (common 'clr-el)   ; FIXME
                               ((eq? host: 'linux-arm-el)
                                (error 'setup.sch "An explicit target: value is required for ARM Linux, either linux-arm-el-hardfp or linux-arm-el-softfp"))
                               (else host:))))
           (setup-real! scheme: host: target: c-compiler: string-rep:
            (or native sassy nasm) code-cov rebuild-code-cov
            sassy nasm)))))

;; Can't use parameters for *host-dir* and such, because we have not
;; loaded the compatibility files yet at the time we get here.
;; But this is just for setting things up anyway.
;; 
;; setup-real! : Symbol ... -> Void
;; Sets global variables (based on Scheme impl. running on and Target OS),
;; then calls UNIX-INITIALIZE

(define (setup-real! host-scheme host-arch target-arch 
                     c-compiler-choice string-rep-choice
                     native code-cov rebuild-code-cov 
                     sassy nasm)
  (define (platform->endianness sym)
    (case sym 
      ((macosx solaris clr-be)                  'big)
      ((macosx-el linux-el linux-arm-el linux-arm-el-hardfp cygwin win32 clr-el) 'little)
      (else (error 'platform->endianness "Unhandled case: ~a" sym))))
  (define (platform->os sym)
    (case sym 
      ((macosx macosx-el solaris linux-el cygwin linux-arm-el linux-arm-el-hardfp) 'unix)
      ((win32) 'win32)
      ((clr-be) 'unix)     ; FIXME
      ((clr-el) 'win32)    ; FIXME
      (else (error 'platform->os "Unhandled case: ~a" sym))))

  ;; Warn about "semi-working" cases

  (cond ((and (not native)
              (memq host-arch '(cygwin win32)))
         (display "Warning: Petit/Standard-C on Windows is incomplete.")
         (newline)
         (display "Use at own risk, or try Petit/NASM")
         (newline)
         (newline)

         ;; In particular, control transfer points are not guaranteed
         ;; to be 4-byte aligned, and therefore on win32 we use the
         ;; CODEPTR_SHIFT2 feature to ensure they have a fixnum tag.
         ;; This almost works, except that addresses with significant
         ;; bits that are corrupted by the shift-by-2; thus things
         ;; break when dynamically loading compiled code on win32
         ;; non-native.
         ))

  (case host-scheme
    ((plt-r5rs)
     (set! *host-dir*  "PLT-R5RS")
     (set! *host-name* "PLT-R5RS"))
    ((mzscheme) 
     (set! *host-dir*  "MzScheme") 
     (set! *host-name* "MzScheme"))
    ((larceny)
     (set! *host-dir*  "Larceny") 
     (set! *host-name* "Larceny"))
    ((chez petite)

     ;; Felix hasn't gotten his hands on Chez yet,
     ;; but will assume that Petite is a subset of Chez.

     (set! *host-dir*  "Petite")
     (set! *host-name* "Chez"))
    (else (error 'setup "Unknown host scheme: ~a" host-scheme)))
  
  (set! *host:os* host-arch)

  (set! *change-feature-set*
        (case target-arch
          ((macosx-el)    (cond (sassy 'features-x86-sassy-macosx)
                                (else 'features-petit-macosx-el)))
          ((macosx)       'features-petit-macosx)
          ((solaris)      (cond (native 'features-sparc-solaris)
                                (else 'features-petit-solaris)))
          ((linux-arm-el-hardfp)
                           (cond (native    'features-arm-el-hardfp-linux)
                                 (else      (error 'setup.sch "Only the native system is supported on ARM Linux"))))
          ((linux-el)     (cond (sassy     'features-x86-sassy-linux)
                                (nasm      'features-x86-nasm-linux)
                                (native    'features-x86-nasm-linux)
                                (else      'features-petit-linux)))
          ((cygwin)       'features-petit-cygwin)
          ((win32)        (cond (sassy  'features-x86-sassy-win32)
                                (nasm   'features-x86-nasm-win32)
                                (native 'features-x86-nasm-win32)
                                (else   'features-petit-win32)))

          ;; if client says we're using unix or clr,
          ;; then just use value set by features.sch

          ((unix clr-be clr-el)
                          *change-feature-set*)

          (else
           (error 'setup.sch "Must add support for target-arch"))
          ))

  (case target-arch
    ((cygwin win32)
       (set! *twobit-executable-name* "twobit.bin.exe")
       (set! *petit-executable-name*  "petit.bin.exe")))

  (set! *make:larceny-target*
        (case target-arch
          ((macosx
            macosx-el) "petitmacosx")
          ((solaris) "petitsparcsolaris")
          ((cygwin)  "petitcygwinmswindows")
          ((linux-el linux-arm-el-hardfp) "petitdebianlinux")
          ((clr-be clr-le) "common")))        ; FIXME

  (set! *host:endianness* (platform->endianness host-arch))
  (set! *target:endianness* (platform->endianness target-arch))
  (set! *target:os* (platform->os target-arch))
  
  (cond (sassy
         (case target-arch
           ((win32)
            (set! *target:machine* 'x86-sass)
            (set! *target:machine-source* "IAssassin")
            (set! *makefile-configuration* 'x86-win32-static-visualc)
            (set! *heap-type* 'sassy)
            (set! *runtime-type* 'sassy-native))
           ((macosx-el)
            (set! *target:machine* 'x86-sass)
            (set! *target:machine-source* "IAssassin")
            (set! *makefile-configuration* 'sassy-macosx-static-gcc-nasm)
            (set! *heap-type* 'sassy)
            (set! *runtime-type* 'sassy-native))
           ((linux-el)
            (set! *target:machine* 'x86-sass)
            (set! *target:machine-source* "IAssassin")
            (set! *makefile-configuration* 'sassy-unix-static-gcc-nasm)
            (set! *heap-type* 'sassy)
            (set! *runtime-type* 'sassy-native))))

        (native
         (case target-arch
           ((solaris)
            (set! *target:machine* 'sparc)
            (set! *target:machine-source* "Sparc")
            (set! *makefile-configuration* 'sparc-solaris-static-gcc)
            (set! *heap-type* 'sparc-native)
            (set! *runtime-type* 'sparc-native))

           ((linux-arm-el-hardfp)
            (set! *target:machine* 'arm)
            (set! *target:machine-source* "Fence")
            (set! *makefile-configuration* 'arm-hardfp-linux-gcc-v4-gas)
            (set! *heap-type* 'arm-native)
            (set! *runtime-type* 'arm-native))

           ;; Win32 native is just Petit with extasm of NASM rather than C

           ((win32)
            (set! *target:machine* 'x86-nasm)
            (set! *target:machine-source* "Standard-C")
            (set! *makefile-configuration* #f)
            (set! *heap-type* 'petit)
            (set! *runtime-type* 'petit))

           ;; Linux86 native is just Petit with extasm of NASM rather than C           
           ((linux-el)
            (set! *target:machine* 'x86-nasm)
            (set! *target:machine-source* "Standard-C")
            (set! *makefile-configuration* #f)
            (set! *heap-type* 'petit)
            (set! *runtime-type* 'petit))

           (else 
            (error "Unsupported architecture for native setup: "
                   target-arch))))

        ((memq target-arch '(clr-be clr-el))
         (set! *target:machine* 'clr)
         (set! *target:machine-source* "IL")
         (set! *makefile-configuration* #f)
         (set! *heap-type* 'clr)
         (set! *runtime-type* 'clr))

        (else
         (set! *target:machine* 'standard-c)
         (set! *target:machine-source* "Standard-C")
         (set! *makefile-configuration* #f)
         (set! *heap-type* 'petit)
         (set! *runtime-type* 'petit)
         ))

  (set! *code-coverage* (or code-cov rebuild-code-cov))
  (set! *rebuild-code-coverage* rebuild-code-cov)
        
  ;; [usually #f; user may override with e.g. 'mwcc aka CodeWarrior]

  (set! *host:c-compiler* (or c-compiler-choice
                              (and native (not sassy)
                                   (eq? target-arch 'win32)
                                   'nasm+msvc)
                              (and native (not sassy)
                                   (eq? target-arch 'linux-el)
                                   'nasm+gcc)
                              #f))

  (set! *target:string-rep*
        (case string-rep-choice
         ((flat4 #f)
          'flat4)
         ((flat1)
          (if (or (and native (eq? target-arch 'solaris))
                  sassy)
              'flat1
              (error "Unsuported architecture for flat1 string-rep: "
                     target-arch)))
         (else (error "Unsuported string representation: "
                      string-rep-choice))))
  
  (if (or (eq? *target:machine* 'x86-nasm)
          (eq? *target:machine* 'x86-sass))
      (set! *globals-table* "globals-nasm.cfg"))

  (if (eq? *target:machine* 'arm)
      (set! *globals-table* "globals-fence.cfg"))
 
  (case target-arch
   ((clr-be clr-el)
    (load "src/Build/dotnet.sch")
    (larceny-setup "Larceny" *target:os* *target:endianness*))   ; FIXME
   (else
    (unix-&-win32-initialize))))

(define (setup-load-build . args) 
  (apply setup args)
  (build-config-files)
  (load-compiler)
  (build-runtime)
  (build-heap) 
  (build-executable)
  (build-larceny-files))

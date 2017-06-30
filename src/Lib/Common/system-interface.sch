; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Non-system-dependent system interface.  See also sys-<SYSNAME>.sch.
; This is a safe interface to unsafe code.

($$trace "system-interface")

; FIXME: temporary infrastructure for Unicode conversion
; FIXME: defaults to (native-transcoder), which iosys.sch defines later

(define (sys$string->cstring s)
  (cond ((bytevector? s) s)
        ((string? s)
         (string->bytevector s (native-transcoder)))
        (else (error 'sys$string->cstring "bad string" s))))

(define (sys$cstring->string bv)
  (cond ((bytevector? bv)
         (bytevector->string bv (native-transcoder)))
        (else (error 'sys$cstring->string "bad cstring" bv))))

; FIXME: temporary instrumentation for C <--> Scheme string conversions.

(define **syscall-magic-cookie** (make-vector 50 #f))

(define (syscall id . args)
  (cond ((eq? id '**syscall-magic-cookie**)
         **syscall-magic-cookie**)
        ((and (fixnum? id) (<= 0 id))
         (let* ((converter
                 (lambda (x)
                   (if (string? x)
                       (begin
                        (vector-set! **syscall-magic-cookie** id id)
                        (sys$string->cstring x))
                       x)))
                (args (map converter args)))
           (apply %syscall id args)))
        (else
         (error "syscall: bad index " id))))

(define (sys$get-resource-usage) 
  (let try-again ()
    (let* ((ngens (sys$system-feature 'stats-generations))
           (nsets (sys$system-feature 'stats-remembered-sets))
           (statss (make-stats-structure ngens nsets))
           (gc-count (gc-counter))
           (ngens* (sys$system-feature 'stats-generations))
           (nsets* (sys$system-feature 'stats-remembered-sets)))
      (if (or (> ngens* ngens)
              (> nsets* nsets))
          (try-again)
          (if (> (gc-counter) gc-count)
              (try-again)
              (syscall syscall:get-resource-usage statss))))))

(define (make-stats-structure generations remsets)
  (let* ((outer       (make-vector $mstat.v-size 0))
         (gen         (make-vector generations 0))
         (rem         (make-vector remsets 0)))
    (vector-set! outer $mstat.generations gen)
    (vector-set! outer $mstat.remsets rem)
    (do ((i 0 (+ i 1)))
        ((= i generations))
      (vector-set! gen i (make-vector $mstat.g-size 0)))
    (do ((i 0 (+ i 1)))
        ((= i remsets))
      (vector-set! rem i (make-vector $mstat.r-size 0)))
    outer))

(define (sys$gc gen type)
  (if (not (fixnum? gen))
      (error "sys$gc: bad generation " gen))
  (if (not (fixnum? type))
      (error "sys$gc: bad type " type))
  (syscall syscall:gc gen type)
  (unspecified))

(define (sys$codevector-iflush bv)
  (if (not (bytevector-like? bv))
      (error "sys$codevector-iflush: not a bytevector-like: " bv))
  (syscall syscall:iflush bv))

(define (sys$gcctl heap rator rand)
  (if (not (and (fixnum? heap) (fixnum? rator) (fixnum? rand)))
      (error "sys$gcctl: bogus: " (list heap rator rand)))
  (syscall syscall:gcctl heap rator rand)
  (unspecified))

(define (stats-dump-on fn)
  (if (not (string? fn))
      (error "stats-dump-on: invalid filename " fn))
  (if (< (syscall syscall:stats-dump-on fn) 0)
      (error "stats-dump-on: I/O error."))
  (unspecified))

(define (stats-dump-off)
  (if (< (syscall syscall:stats-dump-off) 0)
      (error "stats-dump-off: I/O error."))
  (unspecified))

(define (stats-dump-stdout)
  (syscall syscall:stats-dump-stdout)
  (unspecified))

(define (sys$dump-heap fn proc)
  (if (not (string? fn))
      (error "sys$dump-heap: bad file name " fn))
  (if (not (procedure? proc))
      (error "sys$dump-heap: not a procedure " proc))
  (syscall syscall:dump-heap fn proc)
  (unspecified))

(define (sys$exit code)
  (if (not (fixnum? code))
      (error "sys$exit: bad code " code))
  (syscall syscall:exit code))

(define feature-vector-length     5)
(define feature$larceny-major     0)
(define feature$larceny-minor     1)
(define feature$os-major          2)
(define feature$os-minor          3)
(define feature$gc-info           4)
(define feature$gen-info          5)
(define feature$arch-name         6)
(define feature$os-name           7)
(define feature$endianness        8)
(define feature$stats-generations 9)
(define feature$stats-remsets     10)
(define feature$codevec           11)
(define feature$foldcase          12)
(define feature$execmode          13)
(define feature$pedantic          14)
(define feature$r6path            15)
(define feature$r6program         16)
(define feature$unsafe            17)
(define feature$transcoder        18)
(define feature$r6path2           19)
(define feature$r7features        20)

(define (sys$system-feature name)

  (define (sysfeature v)
    (syscall syscall:sys-feature v))

  (define v (make-vector feature-vector-length 0))

  (define (get-feature feature)
    (vector-set! v 0 feature)
    (sysfeature v)
    (vector-ref v 0))
    
  (case name
    ((larceny-major)
     (get-feature feature$larceny-major))
    ((larceny-minor) 
     (get-feature feature$larceny-minor))
    ((arch-name)
     (let ((arch (get-feature feature$arch-name)))
       (case arch
         ((0) "SPARC")
         ((1) "Standard-C")
         ((2) "X86-NASM")
         ((3) "IAssassin")
         ((4) "CLR")
	 ((5) "ARM")
         (else "Unknown"))))
    ((endian)
     (let ((end (get-feature feature$endianness)))
       (case end
         ((0) 'big)
         ((1) 'little)
         (else (error "Illegal endianness identifier " 
                      end
                      " gotten from RTS.")))))
    ((os-name)
     (let ((os (get-feature feature$os-name)))
       (case os
         ((0) "SunOS")
         ((1) "Linux")
         ((2) "MacOS")
         ((3) "Win32")
         ((4) "OSF")
         ((5) "Unix")
         ((6) "Generic")
         ((7) "BSD Unix")
         ((8) "MacOS X")
         (else "Unknown"))))
    ((os-major)
     (get-feature feature$os-major))
    ((os-minor)
     (get-feature feature$os-minor))
    ((codevec)
     (case (get-feature feature$codevec)
       ((0) 'bytevector)
       ((1) 'address)
       ((2) 'address-shifted-1)
       ((3) 'address-shifted-2)
       (else 'unknown)))
    ((gc-tech)
     (vector-set! v 0 feature$gc-info)
     (sysfeature v)
     (let* ((gc-type (vector-ref v 0))
            (gc-gens (vector-ref v 1))
            (g       (make-vector gc-gens #f)))
       (do ((i 0 (+ i 1)))
           ((= i gc-gens))
         (vector-set! v 0 feature$gen-info)
         (vector-set! v 1 (+ i 1))
         (sysfeature v) 
         (let* ((type (case (vector-ref v 0)
                        ((0) 'nursery)
                        ((1) 'two-space)
                        ((2) 'two-space-np-old)
                        ((3) 'two-space-np-young)
                        ((4) 'static)
                        ((5) 'conservative-mark-sweep)
                        ((6) 'one-space-dof)
                        (else 'unknown)))
                (size (vector-ref v 1))
                (params (case type
                          ((two-space-np-old two-space-np-young)
                           (list (cons 'k (vector-ref v 2))
                                 (cons 'j (vector-ref v 3))))
                          ((two-space nursery static one-space-dof)
                           (if (zero? (vector-ref v 2))
                               'fixed
                               'expandable))
                          (else
                           #f))))
           (vector-set! g i (vector i type size params))))
       (cons (case gc-type
               ((0) 'stop+copy)
               ((1) 'generational)
               ((2) 'conservative)
               (else 'unknown))
             g)))
    ((stats-generations)
     (get-feature feature$stats-generations))
    ((stats-remembered-sets)
     (get-feature feature$stats-remsets))
    ((foldcase)
     (case (get-feature feature$foldcase)
      ((0) #f)
      (else #t)))
    ((execmode)
     (case (get-feature feature$execmode)
      ((0)   'r5rs)
      ((3 5) 'r7rs)    ; 3 is scheme-script mode, which is now R7RS
      ((6)   'r7r6)
      ((1)   'err5rs)
      ((2)   'r6rs)
      ((4)   'spanky)
      (else  'unknown)))
    ((ignore1)
     (case (get-feature feature$execmode)
      ((3)   #t)
      (else  #f)))
    ((pedantic)
     (case (get-feature feature$pedantic)
      ((0) #f)
      ((1) #t)
      (else 'extremely)))
    ((r7features)
     (let ((bv (get-feature feature$r7features)))
       (if (bytevector? bv)
           (sys$cstring->string bv)
           "")))
    ((r6path)
     (let ((bv (get-feature feature$r6path)))
       (if (bytevector? bv)
           (sys$cstring->string bv)
           "")))
    ((r6path2)
     (let ((bv (get-feature feature$r6path2)))
       (if (bytevector? bv)
           (sys$cstring->string bv)
           "")))
    ((r6program)
     (let ((bv (get-feature feature$r6program)))
       (if (bytevector? bv)
           (sys$cstring->string bv)
           "")))
    ((safety)
     (case (get-feature feature$unsafe)
      ((0) 1)
      (else 0)))
    ((transcoder)
     (get-feature feature$transcoder))
    (else 
     (error "sys$system-feature: " name " is not a system feature name"))))

; For deciding whether to use R7RS, R6RS, or R5RS semantics.
; Must keep in sync with src/Rts/Sys/primitive.c (which explains encodings).
;
; FIXME: this should be faster

(define (larceny:execution-mode)
  (case (sys$system-feature 'execmode)
   ((r5rs)             'r5rs)
   ((r7rs r7r6 err5rs) 'r7rs)         ; Aeryn mode is now R7RS instead of R6RS.
   (else               'r6rs)))

; Get the value of an environment variable.

(define (sys$check-env-var fn name)
  (cond ((symbol? name)  (symbol->string name))
        ((string? name)  name)
        (else  (error fn ": not a valid name: " name)
               #t)))

(define (getenv name)
  (let ((result
         (syscall syscall:getenv (sys$check-env-var 'getenv name))))
    (if (bytevector-like? result)
        (sys$cstring->string result)
        result)))

(define (setenv name value)
  (if (string? value)
    (syscall syscall:setenv (sys$check-env-var 'setenv name) value)
    (error "setenv: not a string: " value))
  (unspecified))

(define (get-environment-variables)
  (define (name-part s)
    (let* ((n (string-length s))
           (chars (string->list s))
           (probe (memv #\= chars)))
      (if (not probe)
          s
          (substring s 0 (- n (length probe))))))
  (let ((generator (syscall syscall:listenv-init)))
    (if generator
        (let loop ((entries '()))
          (let ((result (syscall syscall:listenv generator)))
            (cond ((bytevector-like? result)
                   (loop (cons (sys$cstring->string result) entries)))
                  (else
                   (let* ((names (map name-part entries)))
                     (map (lambda (name) (cons name (getenv name)))
                          (reverse names)))))))
        '())))

(define (get-errno)
  (syscall syscall:errno))

(define (set-errno! n)
  (assert (fixnum? n))
  (syscall syscall:seterrno n))

(define (make-env-parameter name . rest)
  (let ((*name* (sys$check-env-var 'make-env-parameter name))
        (ok?    (if (null? rest)
                  (lambda (x) #t)
                  (car rest))))
    (lambda args
      (cond
        ((not (pair? args))
         (getenv *name*))
        ((not (null? (cdr args)))
         (error *name* ": too many arguments."))
        ((ok? (car args))
         (setenv *name* (car args)))
        (else
         (error *name* ": Invalid value " (car args)))))))

(define (sro ptr hdr limit)
  (if (not (and (fixnum? ptr)
                (fixnum? hdr)
                (fixnum? limit)))
      (error "Bogus parameters to sro " (list ptr hdr limit)))
  (syscall syscall:sro ptr hdr limit))

(define (system cmd)
  (if (not (string? cmd))
      (error "system: " cmd " is not a string."))
  (syscall syscall:system cmd))

(define (current-directory . rest)
  (if (null? rest)
      (let ((result (syscall syscall:cwd)))
        (if (bytevector-like? result)
            (sys$cstring->string result)
            result))
      (let ((path (car rest)))
        (if (not (string? path))
            (error "current-directory: " path " is not a string."))
        (syscall syscall:chdir (sys$string->cstring path)))))

(define (list-directory . rest)
  (define excluded-names '("." ".."))
  (if (null? rest)
      (list-directory (current-directory))
      (let ((path (car rest)))
        (cond ((string? path)
               (let ((generator
                      (syscall syscall:listdir-open
                               (sys$string->cstring path))))
                 (and generator
                      (let loop ((filenames '()))
                        (let ((result (syscall syscall:listdir generator)))
                          (cond ((bytevector-like? result)
                                 (let* ((name (sys$cstring->string result))
                                        (filenames
                                         (if (member name excluded-names)
                                             filenames
                                             (cons name filenames))))
                                   (loop filenames)))
                                (else
                                 (syscall syscall:listdir-close generator)
                                 (list-sort string<? filenames))))))))
              (else (error 'list-directory (errmsg 'msg:notstring) path))))))

(define (sys$c-ffi-apply trampoline arg-encoding ret-encoding actuals)
  (syscall syscall:c-ffi-apply trampoline arg-encoding ret-encoding actuals))

(define (sys$c-ffi-dlopen path)
  (cond ((not (bytevector? path))       ; 0-terminated bytevector
         (error "sys$c-ffi-dlopen: bad path.") #t)
        (else
         (syscall syscall:c-ffi-dlopen path))))

(define (sys$c-ffi-dlsym handle sym)
  (cond ((not (and (integer? handle) (exact? handle)))
         (error "sys$c-ffi-dlsym: bad handle " handle) #t)
        ((not (bytevector? sym))
         (error "sys$c-ffi-dlsym: bad symbol " sym) #t)
        (else
         (syscall syscall:c-ffi-dlsym handle sym))))

(define (peek-bytes addr bv count)
  (if (and (bytevector? bv)
           (fixnum? count)
           (<= count (bytevector-length bv))
           (or (fixnum? addr)
               (and (bignum? addr)
                    (<= 0 addr 4294967295))))
      (syscall syscall:peek-bytes addr bv count)
      (error "peek-bytes: invalid arguments " addr ", " bv ", " count)))

(define (poke-bytes addr bv count)
  (if (and (bytevector? bv)
           (fixnum? count)
           (<= count (bytevector-length bv))
           (or (fixnum? addr)
               (and (bignum? addr)
                    (<= 0 addr 4294967295))))
      (syscall syscall:poke-bytes addr bv count)
      (error "poke-bytes: invalid arguments " addr ", " bv ", " count)))

; Unicode character values, used in both reader.sch and print.sch.

(define **nul** 0)
(define **alarm** 7)
(define **backspace** 8)
(define **tab** 9)
(define **linefeed** 10)
(define **vtab** 11)
(define **page** 12)
(define **return** 13)
(define **esc** 27)
(define **space** 32)
(define **delete** 127)

; System-dependent character values.

(define **newline** osdep/newline)

; eof

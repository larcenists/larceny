; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Non-system-dependent system interface.  See also sys-<SYSNAME>.sch.
; This is a safe interface to unsafe code.

($$trace "system-interface")

(define (sys$get-resource-usage)
  (syscall syscall:get-resource-usage (make-stats-structure)))

(define (make-stats-structure)
  (let* ((f (cdr (assq 'heap-area-info (system-features))))
         (generations (vector-length f))
         (outer (make-vector $mstat.v-size 0))
         (gen   (make-vector generations 0))
         (rem   (make-vector generations 0)))
    (vector-set! outer $mstat.generations gen)
    (vector-set! outer $mstat.remsets rem)
    (do ((i 0 (+ i 1)))
        ((= i generations))
      (vector-set! gen i (make-vector $mstat.g-size 0))
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

(define feature-vector-length 5)
(define feature$larceny-major 0)
(define feature$larceny-minor 1)
(define feature$os-major      2)
(define feature$os-minor      3)
(define feature$gc-info       4)
(define feature$gen-info      5)
(define feature$arch-name     6)
(define feature$os-name       7)
(define feature$endianness    8)

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
         (else "Unknown"))))
    ((os-major)
     (get-feature feature$os-major))
    ((os-minor)
     (get-feature feature$os-minor))
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
                        (else 'unknown)))
                (size (vector-ref v 1))
                (params (case type
                          ((two-space-np-old two-space-np-young)
                           (list (cons 'k (vector-ref v 2))
                                 (cons 'j (vector-ref v 3))))
                          ((two-space nursery static)
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
    (else 
     (error "sys$system-feature: " name " is not a system feature name"))))

; Get the value of an environment variable.

(define (getenv name)
  (cond ((symbol? name)
         (syscall syscall:getenv (symbol->string name)))
        ((string? name)
         (syscall syscall:getenv name))
        (else
         (error "getenv: not a valid name: " name)
         #t)))

(define (sro ptr hdr limit)
  (if (not (and (fixnum? ptr)
                (fixnum? hdr)
                (fixnum? limit)))
      (error "Bogus parameters to sro " (list ptr hdr limit)))
  (syscall syscall:sro ptr hdr limit))

(define (system cmd)
  (osdep/system cmd))

(define (sys$C-ffi-apply trampoline arg-encoding ret-encoding actuals)
  (syscall syscall:C-ffi-apply trampoline arg-encoding ret-encoding actuals))

(define (sys$C-ffi-dlopen path)
  (cond ((not (bytevector? path))       ; 0-terminated bytevector
         (error "sys$C-ffi-dlopen: bad path.") #t)
        (else
         (syscall syscall:C-ffi-dlopen path))))

(define (sys$C-ffi-dlsym handle sym)
  (cond ((not (and (integer? handle) (exact? handle)))
         (error "sys$C-ffi-dlsym: bad handle " handle) #t)
        ((not (bytevector? sym))
         (error "sys$C-ffi-dlsym: bad symbol " sym) #t)
        (else
         (syscall syscall:C-ffi-dlsym handle sym))))

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

; System-dependent character values.

(define **newline** osdep/newline)
(define **space** 32)
(define **tab** 9)
(define **carriage-return** 13)
(define **linefeed** 10)
(define **form-feed** 12)
(define **backspace** 8)

; eof

; NOT YET FINISHED!

; Makefile to build the runtime-system for Sparc and Petit Larceny.
; Paths here are relative to the Rts subdirectory!

(define common-headers 
  '("Sys/larceny.h" "Sys/larceny-types.h" "Sys/macros.h" "Sys/assert.h"
    "Build/cdefs.h" "Sys/config.h"))

(define sparc-headers
  '("Build/asmdefs.h" "Sparc/asmmacro.h"))

(define petit-headers
  '("Standard-C/millicode.h" "Standard-C/petit-config.h" 
    "Standard-C/petit-hacks.h"))

; FIXME: remove .o here, parameterize this fn by extension, then cache the correct value, use fn below.

(define common-rts-object-dependencies
  (let ((cached #f)
        (deps
          `(  ("Sys/alloc.o" 
                  ,@common-headers "Sys/barrier.h" "Sys/gclib.h" 
                  "Sys/semispace_t.h")
              ("Sys/argv.o" 
                  ,@common-headers)
              ("Sys/barrier.o" 
                  ,@common-headers "Sys/memmgr.h" "Sys/barrier.h")
              ("Sys/bdw-collector.o" 
                  ,@common-headers "Sys/barrier.h" "Sys/gc.h" "Sys/gc_t.h" 
                  "Sys/gclib.h" "Sys/stats.h" "Sys/memmgr.h" "Sys/stack.h" 
                  "bdw-gc/include/gc.h")
              ("Sys/bdw-gc.o" 
                  "Sys/gc.c" ,@common-headers "Sys/gc.h" "Sys/gc_t.h" 
                  "Sys/heapio.h" "Sys/static_heap_t.h")
              ("Sys/bdw-larceny.o" 
                  "Sys/larceny.c" ,@common-headers "Sys/gc.h")
              ("Sys/bdw-stats.o" 
                  "Sys/stats.c" ,@common-headers "Sys/gc.h" "Sys/gc_t.h" 
                  "Sys/gclib.h" "Sys/stats.h" "Sys/memmgr.h")
              ("Sys/bdw-ffi.o" 
                  "Sys/ffi.c" ,@common-headers)
              ("Sys/callback.o" 
                  ,@common-headers)
              ("Sys/cheney.o" 
                  ,@common-headers "Sys/barrier.h" "Sys/gc_t.h" "Sys/gclib.h"
                  "Sys/los_t.h" "Sys/memmgr.h" "Sys/semispace_t.h" 
                  "Sys/static_heap_t.h")
              ("Sys/dof-heap.o"
                  ,@common-headers "Sys/gc.h" "Sys/gc_t.h" "Sys/gclib.h" 
                  "Sys/stats.h" "Sys/los_t.h" "Sys/memmgr.h" 
                  "Sys/old_heap_t.h" "Sys/remset_t.h" "Sys/semispace_t.h" 
                  "Sys/young_heap_t.h")
              ("Sys/ffi.o" 
                  ,@common-headers)
              ("Sys/gc.o"
                  ,@common-headers "Sys/gc.h" "Sys/gc_t.h" "Sys/heapio.h" 
                  "Sys/semispace_t.h" "Sys/static_heap_t.h")
              ("Sys/gc_t.o" 
                  ,@common-headers "Sys/gc_t.h")
              ("Sys/heapio.o" 
                  ,@common-headers "Sys/heapio.h" "Sys/semispace_t.h" 
                  "Sys/gclib.h")
              ("Sys/larceny.o" 
                  ,@common-headers "Sys/gc.h")
              ("Sys/ldebug.o"
                  ,@common-headers "Sys/macros.h")
              ("Sys/los.o"
                  ,@common-headers "Sys/gclib.h" "Sys/los_t.h")
              ("Sys/malloc.o" 
                  ,@common-headers)
              ("Sys/memmgr.o" 
                  ,@common-headers "Sys/barrier.h" "Sys/gc.h" "Sys/gc_t.h" 
                  "Sys/gclib.h" "Sys/stats.h" "Sys/heapio.h" "Sys/los_t.h"
                  "Sys/memmgr.h" "Sys/old_heap_t.h" "Sys/remset_t.h" 
                  "Sys/static_heap_t.h" "Sys/young_heap_t.h")
              ("Sys/np-sc-heap.o"
                  ,@common-headers "Sys/gc.h" "Sys/gc_t.h" "Sys/gclib.h"
                  "Sys/stats.h" "Sys/los_t.h" "Sys/memmgr.h" 
                  "Sys/old_heap_t.h" "Sys/remset_t.h" "Sys/semispace_t.h" 
                  "Sys/young_heap_t.h")
              ("Sys/nursery.o"
                  ,@common-headers "Sys/gc.h" "Sys/gc_t.h" "Sys/gclib.h"
                  "Sys/stats.h" "Sys/los_t.h" "Sys/memmgr.h" "Sys/stack.h"
                  "Sys/young_heap_t.h")
              ("Sys/old_heap_t.o"
                  ,@common-headers "Sys/old_heap_t.h")
              ("Sys/old-heap.o"
                  ,@common-headers "Sys/gc.h" "Sys/gc_t.h" "Sys/gclib.h"
                  "Sys/stats.h" "Sys/los_t.h" "Sys/memmgr.h" "Sys/old_heap_t.h"
                  "Sys/remset_t.h" "Sys/semispace_t.h" "Sys/static_heap_t.h" 
                  "Sys/young_heap_t.h")
              ("Sys/osdep.o"
                  ,@common-headers)
              ("Sys/remset.o" 
                  ,@common-headers "Sys/gclib.h" "Sys/memmgr.h" 
                  "Sys/remset_t.h")
              ("Sys/sc-heap.o"
                  ,@common-headers "Sys/gc.h" "Sys/gc_t.h" "Sys/gclib.h"
                  "Sys/stats.h" "Sys/los_t.h" "Sys/memmgr.h" 
                  "Sys/semispace_t.h" "Sys/stack.h" "Sys/static_heap_t.h" 
                  "Sys/young_heap_t.h")
              ("Sys/semispace.o" 
                  ,@common-headers "Sys/gclib.h" "Sys/semispace_t.h")
              ("Sys/signals.o"
                  ,@common-headers "Sys/signals.h")
              ("Sys/sro.o"
                  ,@common-headers "Sys/gc.h" "Sys/gc_t.h" "Sys/gclib.h"
                  "Sys/heapio.h" "Sys/memmgr.h")
              ("Sys/stack.o"
                  ,@common-headers "Sys/stack.h")
              ("Sys/static-heap.o"
                  ,@common-headers "Sys/gc.h" "Sys/gclib.h" "Sys/stats.h" 
                  "Sys/memmgr.h" "Sys/semispace_t.h" "Sys/static_heap_t.h")
              ("Sys/stats.o"
                  ,@common-headers "Sys/gc.h" "Sys/gc_t.h" "Sys/gclib.h"
                  "Sys/stats.h" "Sys/memmgr.h")
              ("Sys/syscall.o" 
                  ,@common-headers "Sys/signals.h")
              ("Sys/primitive.o"
                  ,@common-headers  "Sys/signals.h")
              ("Sys/osdep-unix.o"
                  ,@common-headers)
              ("Sys/osdep-win32.o"
                  ,@common-headers)
              ("Sys/osdep-generic.o"
                  ,@common-headers)
              ("Sys/util.o"
                  ,@common-headers "Sys/gc.h" "Sys/gc_t.h")
              ("Sys/version.o"
                  "Sys/config.h")
              ("Sys/young_heap_t.o"
                  ,@common-headers "Sys/young_heap_t.h")))
    (lambda ()
      (if cached
          cached

)

(define sparc-rts-object-dependencies
  `(  ("Sparc/barrier.o"
          ,@sparc-headers)
      ("Sparc/bdw-memory.o"
          "Sparc/memory.s" ,@sparc-headers)
      ("Sparc/cache.o"
          ,@common-headers)
      ("Sparc/cache0.o"
          ,@sparc-headers)
      ("Sparc/cglue.o"
          ,@common-headers "Sys/signals.h")
      ("Sparc/bdw-cglue.o"
          "Sparc/cglue.c" ,@common-headers "Sys/signals.h")
      ("Sparc/generic.o"
          ,@sparc-headers)
      ("Sparc/bdw-generic.o"
          "Sparc/generic.s" ,@sparc-headers)
      ("Sparc/glue.o"
          ,@sparc-headers)
      ("Sparc/mcode.o"
          ,@sparc-headers)
      ("Sparc/memory.o"
          ,@sparc-headers)
      ("Sparc/signals.o"
          ,@common-headers)
      ("Sparc/syscall2.o"
          ,@common-headers)))

(define petit-rts-object-dependencies
  `(  ("Standard-C/arithmetic.o" 
          ,@common-headers ,@petit-headers)
      ("Standard-C/millicode.o" 
          ,@common-headers ,@petit-headers "Sys/gc_t.h" "Sys/barrier.h" 
          "Sys/stack.h")
      ("Standard-C/multiply.o" 
          ,@common-headers ,@petit-headers)
      ("Standard-C/syscall2.o" 
          ,@common-headers ,@petit-headers)))

(define (common-rts-objects)
  (map caar common-rts-object-dependencies))

(define (petit-rts-objects)
  (map caar petit-rts-object-dependencies))

(define (rts-compile ...) ...)
(define (rts-assemble ...) ...)
(define (rts-link-static-library ...) ...)

(define petit-runtime-system-project
  (make:project "libpetit.a"
    `(rules
      (".o" ".c" ,rts-compile)
      (".o" ".s" ,rts-assemble))
    `(targets
      ("libpetit.a" ,rts-link-static-library))
    `(dependencies
      ("libpetit.a" ,@(common-rts-objects) ,@(petit-rts-objects))
      ,@common-rts-object-dependencies
      ,@petit-rts-object-dependencies)))


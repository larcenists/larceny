;; These will be prerequisites of this file at some point in future.
(define (prereq name thunk)
  (run-benchmark name thunk))
(define (prereq:load name file)
  (prereq name (lambda () (load file))))

(prereq "DOTNET"
	(lambda () 
	  (load "src/Build/dotnet.sch")
	  (larceny-setup "Larceny" 'win32 'little)))

(prereq "COMPILER"
	(lambda () 
	  (load-compiler)
	  ;; (peephole-optimization #f) ;; not there yet
	  ))
(prereq:load "LINK-LOP" "src/Asm/Shared/link-lop.sch")
(prereq "IL-LCG" 
	(lambda () 
	  (if #f 
	      (peephole-optimization #f) ;; not there yet
	      (load "src/Asm/IL-LCG/peepopt.sch"))
	  (load "src/Asm/IL-LCG/dotnet-ffi-lcg.sch")
	  (load "src/Asm/IL-LCG/pass5p2.sch")))

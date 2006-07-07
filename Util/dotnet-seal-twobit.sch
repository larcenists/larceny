(load "Twobit.fasl")
(load "Compiler/driver-larceny.sch")
(load "Util/seal-twobit.sch")
(load "Asm/IL/il-load-coremem.sch")

(compiler-switches 'standard) ;; This may stop the compiler from
			      ;; introducing .car:pair and such
                              ;; (the introduction of such identifiers
                              ;;  has been a problem e.g. when 
                              ;;  installing the debugger)

(seal-twobit 
 (append '(link-lop-segment/clr 
           eval/clr 
           load-lop/clr 
           with-saving-assembly-to-dll/full-control 
           with-saving-assembly-to-dll 
           compile-file/clr) 
         standard-proc-names))

(load "Twobit.fasl")
(load "Compiler/driver-larceny.sch")
(load "Util/seal-twobit.sch")
(load "Asm/IL/il-load-coremem.sch")

(seal-twobit 
 (append '(link-lop-segment/clr 
           eval/clr 
           load-lop/clr 
           with-saving-assembly-to-dll/full-control 
           with-saving-assembly-to-dll 
           compile-file/clr) 
         standard-proc-names))

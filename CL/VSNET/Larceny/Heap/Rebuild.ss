
(define (main args)
  (current-directory "..\\..\\..\\..")
  (load "util\\dotnet.sch")
  (larceny-setup "MzScheme" 'win32 'little)
  (load-compiler)
  (remove-dotnet-heap-objects)
  (set! integrate-procedures (make-parameter "Integrate Procedures" 'larceny))
  (set! call-with-error-control (lambda (t1 t2) (t1)))
  
  (cond ((member "debug" args)
	 (set-codegen-option! 'ilasm-debug)))
  (cond ((member "opt" args)
	 (set-codegen-option! 'ilasm-opt)))
  (cond ((member "clr-2.0" args)
	 (set-codegen-option! 'clr-2.0)))
  
  ; (make-dotnet-heap)
  ;; See def'n of make-dotnet-heap for the inspiration for below expr.
  (parameterize ((integrate-procedures 'larceny))
    (cond 
   	 ((member "debug" args)
	  (make:make dotnet-heap-project "bin\\Debug\\CommonLarceny"))
	 (else
	  (make:make dotnet-heap-project "bin\\Release\\CommonLarceny"))))
#|  
  (cond ((member "debug" args)
         (copy-file "dotnet.heap.exe" "bin\\Debug\\CommonLarceny.exe")
         (copy-file "dotnet.heap.pdb" "bin\\Debug\\CommonLarceny.pdb"))
        (else 
         (copy-file "dotnet.heap.exe" "bin\\Release\\CommonLarceny.exe")))
|#           
  (exit))



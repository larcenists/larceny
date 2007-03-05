;;; This file provides utilities for interoperating with libraries
;;; written in C.  
;;;
;;; For example, the actual offset into a structure for a particular
;;; field is target-dependent, so rather than hard code the offsets
;;; into our code, we calculate it on the fly (at compile time) by
;;; emitting C code to tell us the offsets and then running that
;;; through the system C compiler.

;;; Wish list: 
;;; * make code more robust by checking that input strings do not
;;;   inject malicious code

;; (define-cstruct-offsets (header-str ...) (offset-id struct field) ...)
(define-syntax define-cstruct-offsets
  (transformer 
   (let* ((c-compiler-cmd 
           (let ((os-name (assq 'os-name (system-features))))
             (cond 
              ((or (equal? os-name '(os-name . "Linux"))
                   (equal? os-name '(os-name . "MacOS X")))
               (lambda (c-src-path exe-path)
                 (string-append "gcc -o " exe-path " " c-src-path)))
              (else 
               (error 'c-compiler-cmd ": add case for " os-name)))))

          (temp-c-file "/tmp/cstructs.c")
          (temp-c-exec "/tmp/cstruct")
          (temp-c-outp "/tmp/cstruct-output")
          
          (make-c-contents
           (lambda (header-names offset-forms)
             (append (map (lambda (header-name) 
                            (string-append "#include " header-name))
                          header-names)
                     (list "#include <stdio.h>")
                     (list "int main(int argc, char **argv) {")
                     (list "   printf(\"\\n(\\n\");")
                     (map (lambda (struct fieldn)
                            (string-append 
                             "{ " struct " s; "
                             "  printf(\"%d \","
                             "   (char*)&(s." fieldn ") - (char*)&s );"
                             "}"))
                          (map cadr offset-forms)
                          (map caddr offset-forms))
                     (list "   printf(\"\\n)\\n\");")
                     (list "}"))))

          (generate-c-code (lambda (c-contents)
                             (call-with-output-file temp-c-file
                               (lambda (out)
                                 (for-each (lambda (str)
                                             (display str out)
                                             (newline out))
                                           c-contents)))))
          (compile-c-code (lambda () 
                            (system (c-compiler-cmd temp-c-file temp-c-exec))))
          (run-c-program  (lambda ()
                            (system (string-append temp-c-exec " > " temp-c-outp))))
          (read-output    (lambda () 
                            (call-with-input-file temp-c-outp read))))

     
     (lambda (exp ren cmp)
       (let* ((header-names (cadr exp))
              (offset-forms (cddr exp))
              (c-contents (make-c-contents header-names offset-forms)))

         (generate-c-code c-contents)
         (compile-c-code)
         (run-c-program)
         
         (let ((offsets (read-output))
               (offset-names (map car offset-forms)))
           ;; arguably I should invoke the rename procedure on 'begin
           ;; and define here.  But maybe we *want* this macro to
           ;; acquire any local redefinition of begin or define...
           `(begin
              ,@(map (lambda (name offset) `(define ,name ,offset))
                     offset-names offsets))))))))

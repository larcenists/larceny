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

;; (define-c-info
;;    <decl> ...
;;    <defn> ...)
;;
;; <decl>    ::= (compiler <cc-spec>)
;;             | (path <include-path>) ...
;;             | (include <header>) ...
;;             | (include<> <header>) ...
;;
;; <cc-spec> ::= cc | cl
;;
;; <defn>    ::= (const <scheme-name> <type> <c-expr>)
;;             | (sizeof <scheme-name> <c-type>)
;;             | (struct <c-name> <fldspec> ...)
;;             | (fields <c-name> <fldspec> ...)
;;             | (ifdefconst <scheme-name> <type> <c-name>)      ;; unspec o/w
;;
;; <fldspec> ::= (<offset> <c-field>)
;;             | (<offset> <c-field> <size>)
;; 
;; <type>    ::= int | uint | long | ulong
;;
;; <c-field> ::= <string-literal>
;; 
;; <offset>  ::= <scheme-name>
;; <size>    ::= <scheme-name>

(define-syntax define-c-info
  (transformer
    (lambda (exp ren cmp)
      (let ((cc-spec  '())
            (paths    '())
            (prologue '())
            (body     '()))

        (define (stringify thing)
          (cond
            ((string? thing) thing)
            ((symbol? thing) (symbol->string thing))
            ((number? thing) (number->string thing))
            ((pair? thing)   (apply
                               string-append
                               (foldr (lambda (each acc)
                                        (if acc
                                          (cons (stringify each)
                                                (cons " " acc))
                                          (list (stringify each))))
                                      #f
                                      thing)))
            (else            (error 'define-c-info ": Can't stringify"))))

        (define (types type-spec)
          (cond 
           ((cmp type-spec 'int)   (values "%d"  "int"))
           ((cmp type-spec 'uint)  (values "%u"  "unsigned int"))
           ((cmp type-spec 'long)  (values "%ld" "long"))
           ((cmp type-spec 'ulong) (values "%lu" "unsigned long"))
           (else     (values "%od" "long"))))

        (define (gen-const name type-spec expr . decls)
          (set! body
            (cons
              (call-with-values
                (lambda () (types type-spec))
                (lambda (fmt type)
                  `(,name
                     ,fmt
                     ,(string-append
                        "(" type ")"
                        "(" expr  ")")
                     ,@decls)))
              body)))

        (define (gen-ifdefconst name type-spec cname)
          (set! body
            (cons
              (call-with-values
                (lambda () (types type-spec))
                (lambda (fmt type)
                  `(,name
                     (ifdef ,cname
                            (,fmt
                             ,(string-append
                               "(" type ")"
                               "(" cname  ")"))
                            ("%s"
                             "\"#!unspecified\"")))))
              body)))

        (define (gen-fields name fields)
          (for-each
            (lambda (field-exp)
              (gen-const
                (car field-exp)
                'long
                (string-append
                  "(char *)&s."
                  (stringify (cadr field-exp))
                  " - (char *)&s")
                (string-append name " s;"))

              ;; Optional binding of field size 
              (cond
               ((not (null? (cddr field-exp)))
                (gen-const
                 (caddr field-exp)
                 'ulong
                 (string-append
                  "sizeof( s." (stringify (cadr field-exp)) ")")
                 (string-append name " s;")))))
            fields))

        (define (gen-include header)
          (set! prologue
            (cons (string-append "#include " header)
                  prologue)))

        (for-each
          (lambda (form)
            (cond
             ((cmp (car form) 'compiler)
              (set! cc-spec (list (cadr form))))

             ((cmp (car form) 'path)
              (set! paths (cons (cadr form) paths)))

             ((cmp (car form) 'include)
              (gen-include
               (string-append "\"" (eval (cadr form)) "\"")))

             ((cmp (car form) 'include<>)
              (gen-include
               (string-append "<" (eval (cadr form)) ">")))

             ((cmp (car form) 'const)
              (gen-const
               (cadr form)
               (caddr form)
               (stringify (cadddr form))))

             ((cmp (car form) 'ifdefconst)
              (gen-ifdefconst
               (cadr form)
               (caddr form)
               (stringify (cadddr form))))

             ((cmp (car form) 'sizeof)
              (gen-const
               (cadr form)
               'ulong
               (string-append
                "sizeof(" (stringify (caddr form)) ")")))

             ((cmp (car form) 'fields)
              (gen-fields (stringify (cadr form))
                          (cddr form)))

             ((cmp (car form) 'struct)
              (gen-fields (string-append
                           "struct " (stringify (cadr form)))
                          (cddr form)))

             ((cmp (car form) 'raw)
              (set! body (cons (cdr form) body)))

             (else
              (error 'define-c-info
                     (string-append
                      ": Unknown c-info spec: "
                      (stringify (car form)))))))
          (cdr exp))

        `(define-c-values
           ,@cc-spec
           ,(reverse paths)
           ,(reverse prologue)
           ,@(reverse body))))))

;; (define-c-values
;;    [ cc | cl ]
;;    (<include-path> ...)
;;    (<c-prologue> ...)
;;    <decl> ...)
;;
;; <desc> ::= (<printf-fmt> <c-expr> <c-decls> ...)
;; <decl> ::= (<scheme-name> . <desc>)
;;         |  (<scheme-name> (ifdef <id> <desc> <desc>))
;;
(define-syntax define-c-values
  (transformer
   (let* ((os-name (assq 'os-name (system-features)))
          (os-type
            (cond
              ((member os-name '((os-name . "Linux")
                                 (os-name . "MacOS X")
                                 (os-name . "SunOS")))
               'unix)
              ((member os-name '((os-name . "Win32")))
               'windows)
              (else
               (error 'c-compiler-cmd ": add case for " os-name))))
          (temp-c-file "larceny-c-info.c")
          (temp-c-exec (if (eq? os-type 'windows)
                         "larceny-c-info.exe"
                         "./larceny-c-info"))
          (temp-c-outp "larceny-c-info-output")

          (c-compile (let ()
                       (define (cc-compile exe-path c-src-path . includes)
                         (let ((cc (or (getenv "CC") "cc"))
                               (include-directives
                                (apply string-append
                                       (map (lambda (path)
                                              (string-append "-I" path " "))
                                            includes))))
                           (zero?
                            (system
                             (string-append
                              cc " "
                              include-directives
                              " -m32 "
                              " -D_XOPEN_SOURCE=500 "
                              " -o " exe-path
                              " "  c-src-path)))))
                       
                       (define (cl-compile exe-path c-src-path . includes)
                         (let ((include-directives
                                (apply string-append
                                       (map (lambda (path)
                                              (string-append "/I" path " "))
                                            includes)))
                               (obj-path (string-append c-src-path ".obj")))
                           (let ((result (zero?
                                          (system
                                           (string-append
                                            "cl /nologo "
                                            include-directives
                                            " /Fo" obj-path
                                            " /Fe" exe-path
                                            " "  c-src-path
                                            "> nul:")))))
                             (if result (delete-file obj-path))
                             result)))
                       
                       (case os-type
                         ((unix)    cc-compile)
                         ((windows) cl-compile))))

          (make-c-contents
           (lambda (c-prologue c-forms)
             `(,@c-prologue
               "#include <stdio.h>"
               "int main(int argc, char **argv) {"
               "   printf(\"\\n(\\n\");"
               ,@(let ((process-standard 
                        (lambda (printf-fmt c-expr c-decls)
                          (string-append
                           "{ "
                           (apply string-append c-decls)
                           "  printf(\"" printf-fmt " \",(" c-expr "));"
                           "}")))
                       (process-ifdef
                        (lambda (id then-fmt then-c-expr then-decls
                                    else-fmt else-c-expr else-decls)
                          (string-append
                           "{ "
                           "\n#ifdef " id "\n"
                           (apply string-append then-decls)
                           "  printf(\"" then-fmt " \",(" then-c-expr "));"
                           "\n#else \n"
                           (apply string-append else-decls)
                           "  printf(\"" else-fmt " \",(" else-c-expr "));"
                           "\n#endif \n"
                           "}"))))
                   (map (lambda (entry)
                          (cond ((string? (cadr entry))
                                 (process-standard (cadr entry)
                                                   (caddr entry)
                                                   (cdddr entry)))
                                ((eq? 'ifdef (car (cadr entry)))
                                 (let ((tst (cadr (cadr entry)))
                                       (thn (caddr (cadr entry)))
                                       (els (cadddr (cadr entry))))
                                   (process-ifdef tst
                                                  (car thn)
                                                  (cadr thn)
                                                  (cddr thn)
                                                  (car els)
                                                  (cadr els)
                                                  (cddr els))))
                                (else (error))))
                        c-forms))
               "  printf(\"\\n)\\n\");"
               "  return 0;"
               "}")))

          (generate-c-code (lambda (c-contents)
                             (call-with-output-file temp-c-file
                               (lambda (out)
                                 (for-each (lambda (str)
                                             (display str out)
                                             (newline out))
                                           c-contents)))))
          (compile-c-code (lambda (c-compile include-paths)
                            (apply c-compile
                                   temp-c-exec
                                   temp-c-file
                                   include-paths)))
          (run-c-program  (lambda ()
                            (system (string-append temp-c-exec
                                                   " > " temp-c-outp))))
          (read-output    (lambda ()
                            (call-with-input-file temp-c-outp read)))
          (delete-temps   (lambda ()
                            (delete-file temp-c-file)
                            (delete-file temp-c-exec)
                            (delete-file temp-c-outp))))

     (lambda (exp ren cmp)
       (let* ((compile/rest (case (cadr exp)
                               ((cc) (cons cc-compile (cddr exp)))
                               ((cl) (cons cl-compile (cddr exp)))
                               (else (cons c-compile (cdr exp)))))
              (c-compile    (car compile/rest))
              (rest         (cdr compile/rest))
              (include-paths (car rest))
              (c-prologue   (cadr rest))
              (c-forms      (cddr rest))
              (c-contents   (make-c-contents c-prologue c-forms)))

         (generate-c-code c-contents)
         (if (not (compile-c-code c-compile include-paths))
             (error 'define-c-value ": compiler error."))
         (run-c-program)

         (let ((c-values     (read-output))
               (scheme-names (map car c-forms)))
           (delete-temps)

           ;; arguably I should invoke the rename procedure on 'begin
           ;; and define here.  But maybe we *want* this macro to
           ;; acquire any local redefinition of begin or define...
           `(begin
              ,@(map (lambda (name offset) `(define ,name ,offset))
                     scheme-names c-values))))))))


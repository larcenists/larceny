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
;;             | (struct <c-name> (<scheme-name> <c-field>) ...)
;;             | (fields <c-name> (<scheme-name> <c-field>) ...)
;;
;; <type>    ::= int | uint | long | ulong
;;
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
          (case type-spec
            ((int)    (values "%d"  "int"))
            ((uint)   (values "%u"  "unsigned int"))
            ((long)   (values "%ld" "long"))
            ((ulong)  (values "%lu" "unsigned long"))
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
                (string-append name " s;")))
            fields))

        (define (gen-include header)
          (set! prologue
            (cons (string-append "#include " header)
                  prologue)))

        (for-each
          (lambda (form)
            (case (car form)
              ((compiler)
               (set! cc-spec (list (cadr form))))

              ((path)
               (set! paths (cons (cadr form) paths)))

              ((include)
               (gen-include
                 (string-append "\"" (stringify (cadr form)) "\"")))

              ((include<>)
               (gen-include
                 (string-append "<" (stringify (cadr form)) ">")))

              ((const)
               (gen-const
                 (cadr form)
                 (caddr form)
                 (stringify (cadddr form))))

              ((sizeof)
               (gen-const
                 (cadr form)
                 'ulong
                 (string-append
                   "sizeof(" (stringify (caddr form)) ")")))

              ((fields)
               (gen-fields (stringify (cadr form))
                           (cddr form)))

              ((struct)
               (gen-fields (string-append
                             "struct " (stringify (cadr form)))
                           (cddr form)))

              ((raw)
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


;; (define-cstruct-offsets [ c-compiler-spec ]
;;                         (include-path ...)
;;                         (header-name ...)
;;                         (offset-id struct field) ...)
;;
;; This is deprecated in favor of define-c-info.
;;
(define-syntax define-cstruct-offsets
  (transformer
     (lambda (exp ren cmp)
       (let* ((cc-spec/rest (if (symbol? (cadr exp))
                              (cons (list (cadr exp)) (cddr exp))
                              (cons '() (cdr exp))))
              (include-paths (cadr cc-spec/rest))
              (header-names (caddr cc-spec/rest))
              (offset-forms (cdddr cc-spec/rest)))
         `(define-c-values
            ,@(car cc-spec/rest)
            ,include-paths
            ,(map (lambda (name)
                    (string-append "#include " name))
                  header-names)
            ,@(map (lambda (offset)
                     `(,(car offset)
                       "%d"
                       ,(string-append "(char *)&s." (caddr offset)
                                       " - (char *)&s")
                       ,(string-append (cadr offset) " s;")))
                   offset-forms))))))

;; (define-c-values
;;    [ cc | cl ]
;;    (<include-path> ...)
;;    (<c-prologue> ...)
;;    <decl> ...)
;;
;; <decl> ::= (<scheme-name> <printf-fmt> <c-expr> <c-decls> ...)
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

          (c-compile (case os-type
                       ((unix)    cc-compile)
                       ((windows) cl-compile)))

          (make-c-contents
           (lambda (c-prologue c-forms)
             `(,@c-prologue
               "#include <stdio.h>"
               "int main(int argc, char **argv) {"
               "   printf(\"\\n(\\n\");"
               ,@(map (lambda (printf-fmt c-expr c-decls)
                        (string-append
                          "{ "
                          (apply string-append c-decls)
                          "  printf(\"" printf-fmt " \",(" c-expr "));"
                          "}"))
                      (map cadr   c-forms)
                      (map caddr  c-forms)
                      (map cdddr  c-forms))
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
         (compile-c-code c-compile include-paths)
         (run-c-program)

         (let ((c-values     (read-output))
               (scheme-names (map car c-forms)))
           ;(delete-temps)

           ;; arguably I should invoke the rename procedure on 'begin
           ;; and define here.  But maybe we *want* this macro to
           ;; acquire any local redefinition of begin or define...
           `(begin
              ,@(map (lambda (name offset) `(define ,name ,offset))
                     scheme-names c-values))))))))


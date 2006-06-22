(for-each 
 require
 '(srfi-0 srfi-1 srfi-9 
   srfi-23 
   srfi-56 ;; We may not need 56 (just provide a stub that errors?)
   srfi-60 srfi-69))

(parameterize ((current-require-path
                (cons "Asm/Intel/Sassy" 
                      (current-require-path))))
  (let ((r (lambda (x)
             (begin (display `(require ,x)) (newline))
             (require x))))
    (r 'extras)
    (r 'meta-lambda)
    (r 'push-stacks)
    (r 'api)
    (r 'intern)
    (r 'macros)
    (r 'numbers)
    (parameterize ((current-require-path
                    (cons "Asm/Intel/Sassy/other" 
                          (current-require-path))))
      (r 'srfi-56-pieces))
    (r 'operands)
    (r 'text-block)
    (r 'opcodes)
    (r 'text)
    (r 'parse)
    (r 'main)
    (r 'flat-bin)
    (r 'elf)))


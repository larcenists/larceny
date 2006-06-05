(for-each 
 require
 '(srfi-0 srfi-1 srfi-9 
   srfi-23 
   srfi-56 ;; We may not need 56 (just provide a stub that errors?)
   srfi-60 srfi-69))

(parameterize ((current-require-path
                (cons "Asm/Intel/Sassy" 
                      (current-require-path))))
  (require 'extras)
  (require 'meta-lambda)
  (require 'push-stacks)
  (require 'api)
  (require 'intern)
  (require 'macros)
  (require 'numbers)
  (parameterize ((current-require-path
                  (cons "Asm/Intel/Sassy/other" 
                        (current-require-path))))
    (require 'srfi-56-pieces))
  (require 'operands)
  (require 'text-block)
  (require 'opcodes)
  (require 'text)
  (require 'parse)
  (require 'main)
  (require 'flat-bin)
  (require 'elf))


(define (.common-patch-procedure file ns id number proc)
  (procedure-set! proc
                  0
                  (syscall syscall:segment-code-address file ns id number))
  proc)

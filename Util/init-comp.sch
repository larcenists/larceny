; $Id$
; Haaaahahahahah!!!!

(let ((v (system "case `uname -r` in
                    4.*) exit 1 ;;
                    5.*) exit 2 ;;
                 esac
                 exit 99")))
  (case (quotient v 256)
    ((1) (target-architecture 'sun4-sunos))
    ((2) (target-architecture 'sun4-solaris))
    (else (target-architecture 'unknown))))

; Reasonable defaults.

; Compiler

(issue-warnings #f)
(include-variable-names #f)
(include-source-code #f)

; eof

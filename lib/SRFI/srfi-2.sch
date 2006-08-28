; SRFI-2
; $Id$

; Copyright (c) 1993-2001 by Richard Kelsey and Jonathan Rees.
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions
;are met:
;1. Redistributions of source code must retain the above copyright
;   notice, this list of conditions and the following disclaimer.
;2. Redistributions in binary form must reproduce the above copyright
;   notice, this list of conditions and the following disclaimer in the
;   documentation and/or other materials provided with the distribution.
;3. The name of the authors may not be used to endorse or promote products
;   derived from this software without specific prior written permission.
;
;THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; The reference implementation is written in some weird Scheme variant.
; This is an attempt to produce the same result using SYNTAX-RULES.
;
; I found the both the specification and the implementation unhelpful.
; For example, one would think that (AND-LET* ()) -> #T by analogy with
; (AND) -> #T.  The specification doesn't say.
;
; The following behaves correctly on the test cases at the end of the
; reference implementation,  except that it doesn't catch the three syntax
; errors.  There is no way for SYNTAX-RULES to distinguish between a
; constant and a variable, and no easy way to check if a variable is
; being used twice in the same AND-LET* (and why is that an error? LET*
; allows it).

(define-syntax and-let*
  (syntax-rules ()

    ; No body - behave like AND.
    ((and-let* ())
     #t)
    ((and-let* ((var exp)))
     exp)
    ((and-let* ((exp)))
     exp)
    ((and-let* (var))
     var)

    ; Have body - behave like LET* but check for #F values.

    ; No clauses so just use the body.
    ((and-let* () . body)
     (begin . body))

    ; (VAR VAL) clause - bind the variable and check for #F.
    ((and-let* ((var val) more ...) . body)
     (let ((var val))
       (if var
	   (and-let* (more ...) . body)
	   #f)))

    ; Error check to catch illegal (A B ...) clauses.
    ((and-let* ((exp junk . more-junk) more ...) . body)
     (error "syntax error"
	    '(and-let* ((exp junk . more-junk) more ...) . body)))

    ; (EXP) and VAR - just check the value for #F.
    ; There is no way for us to check that VAR is an identifier and not a
    ; constant
    ((and-let* ((exp) more ...) . body)
     (if exp
	 (and-let* (more ...) . body)
	 #f))
    ((and-let* (var more ...) . body)
     (if var
	 (and-let* (more ...) . body)
	 #f))))

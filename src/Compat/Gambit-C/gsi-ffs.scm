; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Sundry foreign functions for Gambit-C.
;
; Compile with
;
;  gsc gsi-ffs
;  gcc -shared -fPIC -D___DYNAMIC gsi-ffs.c gsi-ffs_.c -lc -lgambc \
;    -o gsi-ffs.o1

(c-declare "
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
")

; Foreign-procedure for Gambit-C to extract bits from a double.

(define double-bits
  (c-lambda (int double) unsigned-int 
	    "if (___arg1 == 0)
               ___result = *( (unsigned*) &___arg2 );
             else
               ___result = *( (unsigned*) &___arg2 + 1 );"))

; Return the file modification time as a number, or 0 if the file does not
; exist or its data are not accessible.

(define file-modification-time
  (c-lambda (char-string) unsigned-int
	    "struct stat buf;
             if (stat( ___arg1, &buf ) == 0)
               ___result = (unsigned int)buf.st_mtime;
             else
               ___result = 0;"))

; eof

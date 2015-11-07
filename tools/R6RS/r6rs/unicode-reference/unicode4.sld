; Copyright (c) 2006 Michael Sperber
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. The name of the authors may not be used to endorse or promote products
;    derived from this software without specific prior written permission.
; 
; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; From: Michael Sperber <sperber@informatik.uni-tuebingen.de>
; To: "William D. Clinger" <will@ccs.neu.edu>
; Subject: Unicode normalization
; Date: Tue, 20 Jun 2006 19:59:15 +0200
; 
; 
; I've attached the code I wrote....
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Revised by Will Clinger, 20 June - 11 September 2006.
;
; Modifications include:
;     deletion of tables (that occupied at least 136208 bytes)
;     addition of tables (that occupy about 60 kilobytes)
;     deletion of several procedures:
;         sv-normalization-info
;         sv-has-canonical-decomposition?
;         sv-has-compatibility-decomposition?
;         sv-canonical-decomposition-encoding
;     complete rewrites of several procedures:
;         binary-search (since moved to unicode0.sch)
;         sv-combining-class
;         reverse-decomposition
;         compose-2
;     addition of several procedures:
;         sv-decomposition-index-of-index
;         sv-decomposition-sequence-range
;
; The above changes reduced the table sizes by a factor of 2,
; and sped up the tests in NormalizationTests.txt by a factor of 2.
;
; The tables in this file were generated from the
; Unicode Character Database, revision 7.0.0.
;
; This file does not rely on any lexical syntax for
; non-Ascii characters and strings.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-library (r6rs unicode-reference unicode4)

  (export

    string-normalize-nfd
    string-normalize-nfkd
    string-normalize-nfc
    string-normalize-nfkc)

  (import (scheme base)
         ;(rnrs base)
         ;(rnrs bytevectors)
         ;(rnrs mutable-strings)
          (r6rs unicode-reference unicode0)
          (r6rs unicode-reference unicode1)
          (r6rs unicode-reference unicode2)
          (r6rs unicode-reference unicode3))

  (include "unicode4.body.scm")

  )

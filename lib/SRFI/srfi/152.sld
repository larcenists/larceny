;;; SRFI 152: String Library (reduced)

(define-library (srfi 152)

  (export string? make-string string
          string->vector string->list list->string vector->string
          string-length string-ref substring string-copy
          string=? string<? string>? string<=? string>=?
          string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
          string-upcase string-downcase string-foldcase
          string-append string-map string-for-each
          read-string write-string
          string-set! string-fill! string-copy!

          string->utf8 string->utf16 string->utf16be string->utf16le
          utf8->string utf16->string utf16be->string utf16le->string

          string-normalize-nfc string-normalize-nfkc
          string-normalize-nfd string-normalize-nfkd

          string-null? string-every string-any
          string-tabulate string-unfold string-unfold-right
          reverse-list->string
          string-take string-drop string-take-right string-drop-right
          string-pad string-pad-right
          string-trim string-trim-right string-trim-both
          string-replace
          string-prefix-length string-suffix-length
          string-prefix? string-suffix?
          string-index string-index-right string-skip string-skip-right
          string-contains string-contains-right
          string-take-while string-take-while-right
          string-drop-while string-drop-while-right
          string-break string-span
          string-concatenate string-concatenate-reverse
          string-join
          string-fold string-fold-right string-count
          string-filter string-remove
          string-replicate string-segment string-split)

  (import (scheme base)
          (scheme cxr)
          (scheme char)
          (scheme case-lambda)
          (except (srfi 13)
                  string-downcase
                  string-upcase
                  string-titlecase
                  string-map
                  string-for-each)
          (scheme text)
          (only (rnrs bytevectors)
                string->utf8 string->utf16 utf8->string utf16->string)
          (only (rnrs unicode)
                string-normalize-nfc string-normalize-nfkc
                string-normalize-nfd string-normalize-nfkd)
          (rnrs arithmetic fixnums)

          (primitives errmsg))

  (include "152.body.scm"))

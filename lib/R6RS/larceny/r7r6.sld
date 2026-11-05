;;; Importing this composite library is equivalent to importing
;;; all of the standard R7RS/R6RS libraries plus some Larceny-specific
;;; procedures and syntaxes.

(define-library (larceny r7r6)

  (export

   &assertion
   &condition
   &error
   &i/o
   &i/o-decoding
   &i/o-encoding
   &i/o-file-already-exists
   &i/o-file-does-not-exist
   &i/o-file-is-read-only
   &i/o-file-protection
   &i/o-filename
   &i/o-invalid-position
   &i/o-port
   &i/o-read
   &i/o-write
   &implementation-restriction
   &irritants
   &lexical
   &message
   &no-infinities
   &no-nans
   &non-continuable
   &serious
   &syntax
   &undefined
   &violation
   &warning
   &who
   *
   +
   -
   ...
   /
   <
   <=
   =
   =>
   >
   >=
   _
   abs
   acos
   and
   angle
   append
   apply
   asin
   assert
   assertion-violation
   assertion-violation?
   assoc
   assp
   assq
   assv
   atan
   begin
   binary-port?
   bitwise-and
   bitwise-arithmetic-shift
   bitwise-arithmetic-shift-left
   bitwise-arithmetic-shift-right
   bitwise-bit-count
   bitwise-bit-field
   bitwise-bit-set?
   bitwise-copy-bit
   bitwise-copy-bit-field
   bitwise-first-bit-set
   bitwise-if
   bitwise-ior
   bitwise-length
   bitwise-not
   bitwise-reverse-bit-field
   bitwise-rotate-bit-field
   bitwise-xor
   boolean=?
   boolean?
   bound-identifier=?
   buffer-mode
   buffer-mode?
   bytevector
   bytevector->sint-list
   bytevector->string
   bytevector->u8-list
   bytevector->uint-list
   bytevector-append
;  bytevector-copy
;  bytevector-copy!
   bytevector-fill!
   bytevector-ieee-double-native-ref
   bytevector-ieee-double-native-set!
   bytevector-ieee-double-ref
   bytevector-ieee-double-set!
   bytevector-ieee-single-native-ref
   bytevector-ieee-single-native-set!
   bytevector-ieee-single-ref
   bytevector-ieee-single-set!
   bytevector-length
   bytevector-s16-native-ref
   bytevector-s16-native-set!
   bytevector-s16-ref
   bytevector-s16-set!
   bytevector-s32-native-ref
   bytevector-s32-native-set!
   bytevector-s32-ref
   bytevector-s32-set!
   bytevector-s64-native-ref
   bytevector-s64-native-set!
   bytevector-s64-ref
   bytevector-s64-set!
   bytevector-s8-ref
   bytevector-s8-set!
   bytevector-sint-ref
   bytevector-sint-set!
   bytevector-u16-native-ref
   bytevector-u16-native-set!
   bytevector-u16-ref
   bytevector-u16-set!
   bytevector-u32-native-ref
   bytevector-u32-native-set!
   bytevector-u32-ref
   bytevector-u32-set!
   bytevector-u64-native-ref
   bytevector-u64-native-set!
   bytevector-u64-ref
   bytevector-u64-set!
   bytevector-u8-ref
   bytevector-u8-set!
   bytevector-uint-ref
   bytevector-uint-set!
   bytevector=?
   bytevector?
   caaaar
   caaadr
   caaar
   caadar
   caaddr
   caadr
   caar
   cadaar
   cadadr
   cadar
   caddar
   cadddr
   caddr
   cadr
   call-with-bytevector-output-port
   call-with-current-continuation
   call-with-input-file
   call-with-output-file
   call-with-port
   call-with-string-output-port
   call-with-values
   call/cc
   car
   case
   case-lambda
   cdaaar
   cdaadr
   cdaar
   cdadar
   cdaddr
   cdadr
   cdar
   cddaar
   cddadr
   cddar
   cdddar
   cddddr
   cdddr
   cddr
   cdr
   ceiling
   char->integer
   char-alphabetic?
   char-ci<=?
   char-ci<?
   char-ci=?
   char-ci>=?
   char-ci>?
   char-downcase
   char-foldcase
   char-general-category
   char-lower-case?
   char-numeric?
   char-ready?
   char-title-case?
   char-titlecase
   char-upcase
   char-upper-case?
   char-whitespace?
   char<=?
   char<?
   char=?
   char>=?
   char>?
   char?
   close-input-port
   close-output-port
   close-port
   command-line
   complex?
   cond
   cond-expand
   condition
   condition-accessor
   condition-irritants
   condition-message
   condition-predicate
   condition-who
   condition?
   cons
   cons*
   cos
   current-error-port
   current-input-port
   current-jiffy
   current-output-port
   current-second
   datum->syntax
   define
   define-condition-type
   define-enumeration
   define-record-type
   define-syntax
   define-values
   delay
   delay-force
   delete-file
   denominator
   digit-value
   display
   div
   div-and-mod
   div0
   div0-and-mod0
   do
   dynamic-wind
   else
   emergency-exit
   endianness
   enum-set->list
   enum-set-complement
   enum-set-constructor
   enum-set-difference
   enum-set-indexer
   enum-set-intersection
   enum-set-member?
   enum-set-projection
   enum-set-subset?
   enum-set-union
   enum-set-universe
   enum-set=?
   environment
   eof-object
   eof-object?
   eol-style
   eq?
   equal-hash
   equal?
   eqv?
   error
   error-handling-mode
   error-object-irritants
   error-object-message
   error-object?
   error?
   eval
   even?
   exact
   exact->inexact
   exact-integer-sqrt
   exact-integer?
   exact?
   exists
   exit
   exp
   expt
   features
   file-error?
   file-exists?
   file-options
   filter
   find
   finite?
   fixnum->flonum
   fixnum-width
   fixnum?
   fl*
   fl+
   fl-
   fl/
   fl<=?
   fl<?
   fl=?
   fl>=?
   fl>?
   flabs
   flacos
   flasin
   flatan
   flceiling
   flcos
   fldenominator
   fldiv
   fldiv-and-mod
   fldiv0
   fldiv0-and-mod0
   fleven?
   flexp
   flexpt
   flfinite?
   flfloor
   flinfinite?
   flinteger?
   fllog
   flmax
   flmin
   flmod
   flmod0
   flnan?
   flnegative?
   flnumerator
   flodd?
   flonum?
   floor
   floor-quotient
   floor-remainder
   floor/
   flpositive?
   flround
   flsin
   flsqrt
   fltan
   fltruncate
   flush-output-port
   flzero?
   fold-left
   fold-right
   for-all
   for-each
   force
   free-identifier=?
   fx*
   fx*/carry
   fx+
   fx+/carry
   fx-
   fx-/carry
   fx<=?
   fx<?
   fx=?
   fx>=?
   fx>?
   fxand
   fxarithmetic-shift
   fxarithmetic-shift-left
   fxarithmetic-shift-right
   fxbit-count
   fxbit-field
;  fxbit-set?
;  fxcopy-bit
   fxcopy-bit-field
   fxdiv
   fxdiv-and-mod
   fxdiv0
   fxdiv0-and-mod0
   fxeven?
   fxfirst-bit-set
   fxif
   fxior
   fxlength
   fxmax
   fxmin
   fxmod
   fxmod0
   fxnegative?
   fxnot
   fxodd?
   fxpositive?
   fxreverse-bit-field
   fxrotate-bit-field
   fxxor
   fxzero?
   gcd
   generate-temporaries
   get-bytevector-all
   get-bytevector-n
   get-bytevector-n!
   get-bytevector-some
   get-char
   get-datum
   get-environment-variable
   get-environment-variables
   get-line
   get-output-bytevector
   get-output-string
   get-string-all
   get-string-n
   get-string-n!
   get-u8
   greatest-fixnum
   guard
   hashtable-clear!
   hashtable-contains?
   hashtable-copy
   hashtable-delete!
   hashtable-entries
   hashtable-equivalence-function
   hashtable-hash-function
   hashtable-keys
   hashtable-mutable?
   hashtable-ref
   hashtable-set!
   hashtable-size
   hashtable-update!
   hashtable?
   i/o-decoding-error?
   i/o-encoding-error-char
   i/o-encoding-error?
   i/o-error-filename
   i/o-error-port
   i/o-error-position
   i/o-error?
   i/o-file-already-exists-error?
   i/o-file-does-not-exist-error?
   i/o-file-is-read-only-error?
   i/o-file-protection-error?
   i/o-filename-error?
   i/o-invalid-position-error?
   i/o-port-error?
   i/o-read-error?
   i/o-write-error?
   identifier-syntax
   identifier?
   if
   imag-part
   implementation-restriction-violation?
   include
   include-ci
   inexact
   inexact->exact
   inexact?
   infinite?
   input-port-open?
   input-port?
   integer->char
   integer-valued?
   integer?
   interaction-environment
   irritants-condition?
   jiffies-per-second
   lambda
   latin-1-codec
   lcm
   least-fixnum
   length
   let
   let*
   let*-values
   let-syntax
   let-values
   letrec
   letrec*
   letrec-syntax
   lexical-violation?
   list
   list->string
   list->vector
   list-copy
   list-ref
   list-set!
   list-sort
   list-tail
   list?
   load
   log
   lookahead-char
   lookahead-u8
   magnitude
   make-assertion-violation
   make-bytevector
   make-custom-binary-input-port
   make-custom-binary-input/output-port
   make-custom-binary-output-port
   make-custom-textual-input-port
   make-custom-textual-input/output-port
   make-custom-textual-output-port
   make-enumeration
   make-eq-hashtable
   make-eqv-hashtable
   make-error
   make-hashtable
   make-i/o-decoding-error
   make-i/o-encoding-error
   make-i/o-error
   make-i/o-file-already-exists-error
   make-i/o-file-does-not-exist-error
   make-i/o-file-is-read-only-error
   make-i/o-file-protection-error
   make-i/o-filename-error
   make-i/o-invalid-position-error
   make-i/o-port-error
   make-i/o-read-error
   make-i/o-write-error
   make-implementation-restriction-violation
   make-irritants-condition
   make-lexical-violation
   make-list
   make-message-condition
   make-no-infinities-violation
   make-no-nans-violation
   make-non-continuable-violation
   make-parameter
   make-polar
   make-promise
   make-record-constructor-descriptor
   make-record-type-descriptor
   make-rectangular
   make-serious-condition
   make-string
   make-syntax-violation
   make-transcoder
   make-undefined-violation
   make-variable-transformer
   make-vector
   make-violation
   make-warning
   make-who-condition
   map
   max
   member
   memp
   memq
   memv
   message-condition?
   min
   mod
   mod0
   modulo
   nan?
   native-endianness
   native-eol-style
   native-transcoder
   negative?
   newline
   no-create
   no-fail
   no-infinities-violation?
   no-nans-violation?
   no-truncate
   non-continuable-violation?
   not
   null-environment
   null?
   number->string
   number?
   numerator
   odd?
   open-binary-input-file
   open-binary-output-file
   open-bytevector-input-port
   open-bytevector-output-port
   open-file-input-port
   open-file-input/output-port
   open-file-output-port
   open-input-bytevector
   open-input-file
   open-input-string
   open-output-bytevector
   open-output-file
   open-output-string
   open-string-input-port
   open-string-output-port
   or
   output-port-buffer-mode
   output-port-open?
   output-port?
   pair?
   parameterize
   partition
   peek-char
   peek-u8
   port-eof?
   port-has-port-position?
   port-has-set-port-position!?
   port-position
   port-transcoder
   port?
   positive?
   procedure?
   promise?
   put-bytevector
   put-char
   put-datum
   put-string
   put-u8
   quasiquote
   quasisyntax
   quote
   quotient
   raise
   raise-continuable
   rational-valued?
   rational?
   rationalize
   read
   read-bytevector
   read-bytevector!
   read-char
   read-error?
   read-line
   read-string
   read-u8
   real->flonum
   real-part
   real-valued?
   real?
   record-accessor
   record-constructor
   record-constructor-descriptor
   record-field-mutable?
   record-mutator
   record-predicate
   record-rtd
   record-type-descriptor
   record-type-descriptor?
   record-type-field-names
   record-type-generative?
   record-type-name
   record-type-opaque?
   record-type-parent
   record-type-sealed?
   record-type-uid
   record?
   remainder
;  remove
   remp
   remq
   remv
   reverse
   round
   scheme-report-environment
   serious-condition?
   set!
   set-car!
   set-cdr!
   set-port-position!
   simple-conditions
   sin
   sint-list->bytevector
   sqrt
   square
   standard-error-port
   standard-input-port
   standard-output-port
   string
   string->bytevector
   string->list
   string->number
   string->symbol
   string->utf16
   string->utf32
   string->utf8
   string->vector
   string-append
   string-ci-hash
   string-ci<=?
   string-ci<?
   string-ci=?
   string-ci>=?
   string-ci>?
   string-copy
   string-copy!
   string-downcase
   string-fill!
   string-foldcase
   string-for-each
   string-hash
   string-length
   string-map
   string-normalize-nfc
   string-normalize-nfd
   string-normalize-nfkc
   string-normalize-nfkd
   string-ref
   string-set!
   string-titlecase
   string-upcase
   string<=?
   string<?
   string=?
   string>=?
   string>?
   string?
   substring
   symbol->string
   symbol-hash
   symbol=?
   symbol?
   syntax
   syntax->datum
   syntax-case
   syntax-error
   syntax-rules
   syntax-violation
   syntax-violation-form
   syntax-violation-subform
   syntax-violation?
   tan
   textual-port?
   transcoded-port
   transcoder-codec
   transcoder-eol-style
   transcoder-error-handling-mode
   truncate
   truncate-quotient
   truncate-remainder
   truncate/
   u8-list->bytevector
   u8-ready?
   uint-list->bytevector
   undefined-violation?
   unless
   unquote
   unquote-splicing
   unsyntax
   unsyntax-splicing
   utf-16-codec
   utf-8-codec
   utf16->string
   utf32->string
   utf8->string
   values
   vector
   vector->list
   vector->string
   vector-append
   vector-copy
   vector-copy!
   vector-fill!
   vector-for-each
   vector-length
   vector-map
   vector-ref
   vector-set!
   vector-sort
   vector-sort!
   vector?
   violation?
   warning?
   when
   who-condition?
   with-exception-handler
   with-input-from-file
   with-output-to-file
   with-syntax
   write
   write-bytevector
   write-char
   write-shared
   write-simple
   write-string
   write-u8
   zero?

   r6rs:fxbit-set?
   r6rs:fxcopy-bit
   r6rs:bytevector-copy
   r6rs:bytevector-copy!
   r6rs:remove
   r7rs:let-syntax
   r7rs:letrec-syntax

   ;; R7RS Red Edition

   ;; (scheme list)

;  cons                ; part of R7RS small
;  list                ; part of R7RS small
   xcons
;  cons*               ; part of (rnrs lists)
;  make-list           ; part of R7RS small
   list-tabulate
;  list-copy           ; part of R7RS small
   circular-list
   iota
;  pair?               ; part of R7RS small
;  null?               ; part of R7RS small
   proper-list?
   circular-list?
   dotted-list? 
   not-pair?
   null-list?
   list=
;  car                 ; part of R7RS small
;  cdr                 ; part of R7RS small
;  caar                ; part of R7RS small
;  cadr                ; part of R7RS small
;  cdar                ; part of R7RS small
;  cddr                ; part of R7RS small
;  caaar               ; part of R7RS small
;  caadr               ; part of R7RS small
;  cadar               ; part of R7RS small
;  caddr               ; part of R7RS small
;  cdaar               ; part of R7RS small
;  cdadr               ; part of R7RS small
;  cddar               ; part of R7RS small
;  cdddr               ; part of R7RS small
;  caaaar              ; part of R7RS small
;  caaadr              ; part of R7RS small
;  caadar              ; part of R7RS small
;  caaddr              ; part of R7RS small
;  cadaar              ; part of R7RS small
;  cadadr              ; part of R7RS small
;  caddar              ; part of R7RS small
;  cadddr              ; part of R7RS small
;  cdaaar              ; part of R7RS small
;  cdaadr              ; part of R7RS small
;  cdadar              ; part of R7RS small
;  cdaddr              ; part of R7RS small
;  cddaar              ; part of R7RS small
;  cddadr              ; part of R7RS small
;  cdddar              ; part of R7RS small
;  cddddr              ; part of R7RS small
;  list-ref            ; part of R7RS small
   first
   second
   third
   fourth
   fifth
   sixth
   seventh
   eighth
   ninth
   tenth
   car+cdr
   take
   drop
   take-right
   drop-right
   take!
   drop-right! 
   split-at
   split-at! 
   last
   last-pair
;  length              ; part of R7RS small
   length+
;  append              ; part of R7RS small
   concatenate
;  reverse             ; part of R7RS small
   append!
   concatenate!
   reverse!
   append-reverse
   append-reverse!
   zip
   unzip1
   unzip2
   unzip3
   unzip4
   unzip5
   count
;  map                 ; part of R7RS small
;  for-each            ; part of R7RS small
   fold
   unfold
   pair-fold
   reduce 
;  fold-right          ; part of (rnrs lists)
   unfold-right
   pair-fold-right
   reduce-right 
   append-map
   append-map!
   map!
   pair-for-each
   filter-map
   map-in-order
;  filter              ; part of (rnrs lists)
;  partition           ; part of (rnrs lists)
   remove              ; conflicts with (rnrs lists)
   filter!
   partition!
   remove!
;  member              ; part of R7RS small
;  memq                ; part of R7RS small
;  memv                ; part of R7RS small
;  find                ; part of (rnrs lists)
   find-tail 
   any
   every
   list-index
   take-while
   drop-while
   take-while!
   span
   break
   span!
   break!
   delete
   delete-duplicates 
   delete!
   delete-duplicates!
;  assoc               ; part of R7RS small
;  assq                ; part of R7RS small
;  assv                ; part of R7RS small
   alist-cons
   alist-copy
   alist-delete
   alist-delete!
   lset<=
   lset=
   lset-adjoin
   lset-union
   lset-union!
   lset-intersection
   lset-intersection!
   lset-difference
   lset-difference!
   lset-xor
   lset-xor!
   lset-diff+intersection
   lset-diff+intersection!
;  set-car!            ; part of R7RS small
;  set-cdr!            ; part of R7RS small

   ;; (scheme vector)

   vector-unfold vector-unfold-right vector-reverse-copy 
   vector-concatenate vector-append-subvectors
   vector-empty? vector=
   vector-fold vector-fold-right vector-map!
   vector-count vector-cumulate
   vector-index vector-index-right vector-skip vector-skip-right 
   vector-binary-search vector-any vector-every vector-partition
   vector-swap! vector-reverse! 
   vector-reverse-copy! vector-unfold! vector-unfold-right!
   reverse-vector->list reverse-list->vector

   ;; (scheme sort)

   list-sorted?               vector-sorted?
;  list-sort
;  vector-sort
   list-stable-sort           vector-stable-sort
   list-sort!
;  vector-sort!
   list-stable-sort!          vector-stable-sort!
   list-merge                 vector-merge
   list-merge!                vector-merge!
   list-delete-neighbor-dups  vector-delete-neighbor-dups
   list-delete-neighbor-dups! vector-delete-neighbor-dups!
   vector-find-median         vector-find-median!
   vector-select!             vector-separate!

   ;; (scheme set)

   set set-unfold
   set? set-contains? set-empty? set-disjoint?
   set-member set-element-comparator
   set-adjoin set-adjoin! set-replace set-replace!
   set-delete set-delete! set-delete-all set-delete-all! set-search!
   set-size set-find set-count set-any? set-every?
   set-map set-for-each set-fold
   set-filter set-remove set-partition
   set-filter! set-remove! set-partition!
   set-copy set->list list->set list->set!
   set=? set<? set>? set<=? set>=?
   set-union set-intersection set-difference set-xor
   set-union! set-intersection! set-difference! set-xor!
   set-comparator
  
   bag bag-unfold
   bag? bag-contains? bag-empty? bag-disjoint?
   bag-member bag-element-comparator
   bag-adjoin bag-adjoin! bag-replace bag-replace!
   bag-delete bag-delete! bag-delete-all bag-delete-all! bag-search!
   bag-size bag-find bag-count bag-any? bag-every?
   bag-map bag-for-each bag-fold
   bag-filter bag-remove bag-partition
   bag-filter! bag-remove! bag-partition!
   bag-copy bag->list list->bag list->bag!
   bag=? bag<? bag>? bag<=? bag>=?
   bag-union bag-intersection bag-difference bag-xor
   bag-union! bag-intersection! bag-difference! bag-xor!
   bag-comparator
  
   bag-sum bag-sum! bag-product bag-product!
   bag-unique-size bag-element-count bag-for-each-unique bag-fold-unique
   bag-increment! bag-decrement! bag->set set->bag set->bag!
   bag->alist alist->bag

   ;; (scheme charset)

   char-set? char-set= char-set<=
   char-set-hash 
   char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
   char-set-fold char-set-unfold char-set-unfold!
   char-set-for-each char-set-map
   char-set-copy char-set

   list->char-set  string->char-set 
   list->char-set! string->char-set! 

   char-set-filter
   char-set-filter!
   ucs-range->char-set
   ucs-range->char-set!
   ->char-set

   char-set->list char-set->string

   char-set-size char-set-count char-set-contains?
   char-set-every char-set-any

   char-set-adjoin  char-set-delete 
   char-set-adjoin! char-set-delete!

   char-set-complement  char-set-union  char-set-intersection  
   char-set-complement! char-set-union! char-set-intersection! 

   char-set-difference  char-set-xor  char-set-diff+intersection
   char-set-difference! char-set-xor! char-set-diff+intersection!

   char-set:lower-case          char-set:upper-case     char-set:title-case
   char-set:letter              char-set:digit          char-set:letter+digit
   char-set:graphic             char-set:printing       char-set:whitespace
   char-set:iso-control         char-set:punctuation    char-set:symbol
   char-set:hex-digit           char-set:blank          char-set:ascii
   char-set:empty               char-set:full

   ;; (scheme hash-table)

   make-hash-table
   hash-table
   hash-table-unfold
   alist->hash-table 

   hash-table?
   hash-table-contains?
   hash-table-empty?
   hash-table=?
   hash-table-mutable? 

   hash-table-ref
   hash-table-ref/default 

   hash-table-set!
   hash-table-delete!
   hash-table-intern!
   hash-table-update!
   hash-table-update!/default
   hash-table-pop!
   hash-table-clear! 

   hash-table-size
   hash-table-keys
   hash-table-values
   hash-table-entries
   hash-table-find
   hash-table-count

   hash-table-map
   hash-table-for-each
   hash-table-map!
   hash-table-map->list
   hash-table-fold
   hash-table-prune!

   hash-table-copy
   hash-table-empty-copy
   hash-table->alist 

   hash-table-union!
   hash-table-intersection!
   hash-table-difference!
   hash-table-xor!

   ;; The following procedures are deprecated by SRFI 125:

   hash
;  string-hash
;  string-ci-hash
   hash-by-identity

   hash-table-equivalence-function
   hash-table-hash-function
   hash-table-exists?
   hash-table-walk
   hash-table-merge!

   ;; (scheme ilist)

   iq
   ipair ilist xipair ipair* make-ilist ilist-tabulate iiota
   ipair?
   proper-ilist? ilist? dotted-ilist? not-ipair? null-ilist? ilist=
   icar icdr ilist-ref
   ifirst isecond ithird ifourth ififth isixth iseventh ieighth ininth itenth
   icaar icadr icdar icddr
   icaaar icaadr icadar icaddr icdaar icdadr icddar icdddr
   icaaaar icaaadr icaadar icaaddr icadaar icadadr icaddar icadddr
   icdaaar icdaadr icdadar icdaddr icddaar icddadr icdddar icddddr
   icar+icdr itake idrop ilist-tail
   itake-right idrop-right isplit-at ilast last-ipair
   ilength iappend iconcatenate ireverse iappend-reverse
   izip iunzip1 iunzip2 iunzip3 iunzip4 iunzip5
   icount imap ifor-each ifold iunfold ipair-fold ireduce 
   ifold-right iunfold-right ipair-fold-right ireduce-right 
   iappend-map ipair-for-each ifilter-map imap-in-order
   ifilter ipartition iremove imember imemq imemv
   ifind ifind-tail iany ievery
   ilist-index itake-while idrop-while ispan ibreak
   idelete idelete-duplicates 
   iassoc iassq iassv ialist-cons ialist-delete
   replace-icar replace-icdr
   pair->ipair ipair->pair list->ilist ilist->list
   tree->itree itree->tree gtree->itree gtree->tree
   iapply

   ipair-comparator
   ilist-comparator
   make-ipair-comparator
   make-ilist-comparator
   make-improper-ilist-comparator
   make-icar-comparator
   make-icdr-comparator

   ;; (scheme rlist)

   rquote
   rpair?
   rcons
   rcar
   rcdr
   rcaar
   rcadr
   rcddr
   rcdar
   rcaaar
   rcaadr
   rcaddr
   rcadar
   rcdaar
   rcdadr
   rcdddr
   rcddar
   rcaaaar
   rcaaadr
   rcaaddr
   rcaadar
   rcadaar
   rcadadr
   rcadddr
   rcaddar
   rcdaaar
   rcdaadr
   rcdaddr
   rcdadar
   rcddaar
   rcddadr
   rcddddr
   rcdddar
   rnull?
   rlist?
   rlist
   make-rlist
   rlength
   rappend
   rreverse
   rlist-tail
   rlist-ref
   rlist-set
   rlist-ref/update
   rmap
   rfor-each
   rlist->list
   list->rlist

   ;; (scheme ideque)

   ideque ideque-tabulate ideque-unfold ideque-unfold-right 
   ideque? ideque-empty? ideque= ideque-any ideque-every

   ideque-front ideque-add-front ideque-remove-front
   ideque-back  ideque-add-back  ideque-remove-back

   ideque-ref
   ideque-take ideque-take-right ideque-drop ideque-drop-right
   ideque-split-at

   ideque-length ideque-append ideque-reverse
   ideque-count ideque-zip

   ideque-map ideque-filter-map
   ideque-for-each ideque-for-each-right
   ideque-fold ideque-fold-right
   ideque-append-map
          
   ideque-filter ideque-remove ideque-partition

   ideque-find ideque-find-right
   ideque-take-while ideque-take-while-right
   ideque-drop-while ideque-drop-while-right
   ideque-span ideque-break
          
   list->ideque ideque->list
   generator->ideque ideque->generator

   ;; (scheme text)

   text?                 textual?
   textual-null? 
   textual-every         textual-any
   make-text             text
   text-tabulate
   text-unfold           text-unfold-right
   textual->text
   textual->string       textual->vector      textual->list
   string->text          vector->text         list->text    reverse-list->text
   textual->utf8         textual->utf16be
   textual->utf16        textual->utf16le
   utf8->text            utf16be->text
   utf16->text           utf16le->text
   text-length           textual-length
   text-ref              textual-ref
   subtext               subtextual
   textual-copy
   textual-take          textual-take-right
   textual-drop          textual-drop-right
   textual-pad           textual-pad-right 
   textual-trim          textual-trim-right   textual-trim-both
   textual-replace
   textual=?             textual-ci=?
   textual<?             textual-ci<?
   textual>?             textual-ci>?
   textual<=?            textual-ci<=?
   textual>=?            textual-ci>=?
   textual-prefix-length textual-suffix-length
   textual-prefix?       textual-suffix?    
   textual-index         textual-index-right
   textual-skip          textual-skip-right
   textual-contains      textual-contains-right
   textual-upcase        textual-downcase
   textual-foldcase      textual-titlecase
   textual-append        textual-concatenate  textual-concatenate-reverse
   textual-join
   textual-fold          textual-fold-right
   textual-map           textual-for-each
   textual-map-index     textual-for-each-index
   textual-count
   textual-filter        textual-remove
   textual-replicate     textual-split

   ;; (scheme generator)
   ;;     superseded by a newer version of that library; see below

   #|
   generator make-iota-generator make-range-generator 
   make-coroutine-generator list->generator vector->generator
   reverse-vector->generator string->generator
   bytevector->generator
   make-for-each-generator make-unfold-generator
   gcons* gappend gcombine gfilter gremove 
   gtake gdrop gtake-while gdrop-while
   gdelete gdelete-neighbor-dups gindex gselect
   generator->list generator->reverse-list
   generator->vector generator->vector!  generator->string
   generator-fold generator-for-each generator-find
   generator-count generator-any generator-every generator-unfold
   |#

   ;; (scheme lseq)

   generator->lseq lseq? lseq=?
   lseq-car lseq-first lseq-cdr lseq-rest lseq-ref lseq-take lseq-drop
   lseq-realize lseq->generator lseq-length lseq-append lseq-zip
   lseq-map lseq-for-each lseq-filter lseq-remove
   lseq-find lseq-find-tail lseq-take-while lseq-drop-while
   lseq-any lseq-every lseq-index lseq-member lseq-memq lseq-memv

   ;; (scheme stream)

   stream-null stream-cons stream? stream-null? stream-pair? stream-car
   stream-cdr stream-lambda define-stream list->stream port->stream stream
   stream->list stream-append stream-concat stream-constant stream-drop
   stream-drop-while stream-filter stream-fold stream-for-each stream-from
   stream-iterate stream-length stream-let stream-map stream-match
   stream-of stream-range stream-ref stream-reverse stream-scan
   stream-take
   stream-take-while stream-unfold stream-unfolds stream-zip

   ;; (scheme box)

   box box? unbox set-box!

   ;; (scheme list-queue)

   make-list-queue list-queue list-queue-copy
   list-queue-unfold list-queue-unfold-right
   list-queue? list-queue-empty?
   list-queue-front list-queue-back list-queue-list list-queue-first-last
   list-queue-add-front! list-queue-add-back!
   list-queue-remove-front! list-queue-remove-back!
   list-queue-remove-all! list-queue-set-list!
   list-queue-append list-queue-append! list-queue-concatenate
   list-queue-map list-queue-map! list-queue-for-each

   ;; (scheme ephemeron)

   ephemeron?
   make-ephemeron
   ephemeron-broken?
   ephemeron-key
   ephemeron-datum
   reference-barrier

   ;; (scheme comparator)

   comparator? comparator-ordered? comparator-hashable?
   make-comparator
   make-pair-comparator make-list-comparator make-vector-comparator
   make-eq-comparator make-eqv-comparator make-equal-comparator
   boolean-hash char-hash char-ci-hash
;  string-hash string-ci-hash symbol-hash
   number-hash
   make-default-comparator default-hash comparator-register-default!
   comparator-type-test-predicate comparator-equality-predicate
   comparator-ordering-predicate comparator-hash-function
   comparator-test-type comparator-check-type comparator-hash
   hash-bound hash-salt
   =? <? >? <=? >=?
   comparator-if<=>

   ;; R7RS Tangerine Edition

   ;; (scheme bitwise)                       ; SRFI 151

;  bitwise-not                ; part of (rnrs arithmetic bitwise)
;  bitwise-and                ; part of (rnrs arithmetic bitwise)
;  bitwise-ior                ; part of (rnrs arithmetic bitwise)
;  bitwise-xor                ; part of (rnrs arithmetic bitwise)
   bitwise-eqv                ; not R6RS
   bitwise-nand               ; not R6RS
   bitwise-nor                ; not R6RS
   bitwise-andc1              ; not R6RS
   bitwise-andc2              ; not R6RS
   bitwise-orc1               ; not R6RS
   bitwise-orc2               ; not R6RS

   arithmetic-shift           ; renaming of R6RS procedure
   bit-count                  ; renaming of R6RS procedure
   integer-length             ; renaming of R6RS procedure
;  bitwise-if                 ; part of (rnrs arithmetic bitwise)

   bit-set?                   ; not R6RS
   copy-bit                   ; not R6RS
   bit-swap                   ; not R6RS
   any-bit-set?               ; not R6RS
   every-bit-set?             ; not R6RS
   first-set-bit              ; renaming of R6RS procedure

   bit-field                  ; renaming of R6RS procedure
   bit-field-any?             ; not R6RS
   bit-field-every?           ; not R6RS
   bit-field-clear            ; not R6RS
   bit-field-set              ; not R6RS
   bit-field-replace          ; not R6RS
   bit-field-replace-same     ; not R6RS
   bit-field-rotate           ; not R6RS
   bit-field-reverse          ; renaming of R6RS procedure

   bits->list                 ; not R6RS
   list->bits                 ; not R6RS
   bits->vector               ; not R6RS
   vector->bits               ; not R6RS
   bits                       ; not R6RS
   bitwise-fold               ; not R6RS
   bitwise-for-each           ; not R6RS
   bitwise-unfold             ; not R6RS
   make-bitwise-generator     ; not R6RS

   ;; (scheme bytevector)                    ; (rnrs bytevectors)

   ;; (scheme division)                      ; SRFI 141

   ceiling/ ceiling-quotient ceiling-remainder
   ;; floor/
   ;; floor-quotient
   ;; floor-remainder
   ;; truncate/
   ;; truncate-quotient
   ;; truncate-remainder
   round/ round-quotient round-remainder
   euclidean/ euclidean-quotient euclidean-remainder
   balanced/ balanced-quotient balanced-remainder

   ;; (scheme generator)                     ; SRFI 158

   generator circular-generator make-iota-generator make-range-generator
   make-coroutine-generator list->generator vector->generator
   reverse-vector->generator string->generator
   bytevector->generator
   make-for-each-generator make-unfold-generator
   gcons* gappend gcombine gfilter gremove
   gtake gdrop gtake-while gdrop-while
   gflatten ggroup gmerge gmap gstate-filter
   gdelete gdelete-neighbor-dups gindex gselect
   generator->list generator->reverse-list
   generator->vector generator->vector!  generator->string
   generator-fold generator-map->list generator-for-each generator-find
   generator-count generator-any generator-every generator-unfold
   make-accumulator count-accumulator list-accumulator
   reverse-list-accumulator vector-accumulator
   reverse-vector-accumulator vector-accumulator!
   string-accumulator bytevector-accumulator bytevector-accumulator!
   sum-accumulator product-accumulator

   ;; (scheme fixnum)                        ; SRFI 143

   fx-width            ; NOTE: not R6RS
   fx-greatest         ; NOTE: not R6RS
   fx-least            ; NOTE: not R6RS
;  fixnum?
;  fx=?                ; FIXME: SRFI 143 allows 0 or more arguments
;  fx<?                ; for these comparisons, but does not explain
;  fx>?                ; their semantics for the 0- or 1-argument cases.
;  fx<=?               ; For the time being, we'll assume SRFI 143's
;  fx>=?               ; generalization was just a mistake.
;  fxzero?
;  fxpositive?
;  fxnegative?
;  fxodd?
;  fxeven?
;  fxmax
;  fxmin
;  fx+
;  fx-                 ; FIXME: not restricted to exactly 2 arguments
   fxneg               ; NOTE: not R6RS
;  fx*
   fxquotient          ; NOTE: not R6RS
   fxremainder         ; NOTE: not R6RS
   fxabs               ; NOTE: not R6RS
   fxsquare            ; NOTE: not R6RS
   fxsqrt              ; NOTE: not R6RS
;  fx+/carry
;  fx-/carry
;  fx*/carry
;  fxnot
;  fxand
;  fxior
;  fxxor
;  fxarithmetic-shift
;  fxarithmetic-shift-left
;  fxarithmetic-shift-right
;  fxbit-count
;  fxlength
;  fxif
   fxbit-set?          ; NOTE: incompatible with R6RS fxbit-set?
   fxcopy-bit          ; NOTE: incompatible with R6RS fxcopy-bit
   fxfirst-set-bit     ; NOTE: not R6RS
;  fxbit-field
   fxbit-field-rotate  ; NOTE: not R6RS
   fxbit-field-reverse ; NOTE: not R6RS

   ;; (scheme flonum)                        ; SRFI 144

   fl-e
   fl-1/e
   fl-e-2
   fl-e-pi/4
   fl-log2-e
   fl-log10-e
   fl-log-2
   fl-1/log-2
   fl-log-3
   fl-log-pi
   fl-log-10
   fl-1/log-10
   fl-pi
   fl-1/pi
   fl-2pi
   fl-pi/2
   fl-pi/4
   fl-2/sqrt-pi
   fl-pi-squared
   fl-degree
   fl-2/pi
   fl-sqrt-2
   fl-sqrt-3
   fl-sqrt-5
   fl-sqrt-10
   fl-1/sqrt-2
   fl-cbrt-2
   fl-cbrt-3
   fl-4thrt-2
   fl-phi
   fl-log-phi
   fl-1/log-phi
   fl-euler
   fl-e-euler
   fl-sin-1
   fl-cos-1
   fl-gamma-1/2
   fl-gamma-1/3
   fl-gamma-2/3

   ;; Implementation Constants

   fl-greatest
   fl-least
   fl-epsilon
   fl-fast-fl+*
   fl-integer-exponent-zero
   fl-integer-exponent-nan

   ;; Constructors

   flonum
   fladjacent
   flcopysign
   make-flonum

   ;; Accessors

   flinteger-fraction
   flexponent
   flinteger-exponent
   flnormalized-fraction-exponent
   flsign-bit

   ;; Predicates

;  flonum?
;  fl=?
;  fl<?
;  fl>?
;  fl<=?
;  fl>=?
   flunordered?
;  flmax
;  flmin
;  flinteger?
;  flzero?
;  flpositive?
;  flnegative?
;  flodd?
;  fleven?
;  flfinite?
;  flinfinite?
;  flnan?
   flnormalized?
   fldenormalized?

   ;; Arithmetic

;  fl+
;  fl*
   fl+*
;  fl-
;  fl/
;  flabs
   flabsdiff
   flposdiff
   flsgn
;  flnumerator
;  fldenominator
;  flfloor
;  flceiling
;  flround
;  fltruncate

   ;; Exponents and logarithsm

;  flexp
   flexp2
   flexp-1
   flsquare
;  flsqrt
   flcbrt
   flhypot
;  flexpt
;  fllog
   fllog1+
   fllog2
   fllog10
   make-fllog-base

   ;; Trigonometric functions

;  flsin
;  flcos
;  fltan
;  flasin
;  flacos
;  flatan
   flsinh
   flcosh
   fltanh
   flasinh
   flacosh
   flatanh

   ;; Integer division

   flquotient
   flremainder
   flremquo

   ;; Special functions

   flgamma
   flloggamma
   flfirst-bessel
   flsecond-bessel
   flerf
   flerfc

   ;; (scheme mapping)                       ; SRFI 146

   mapping mapping-unfold
   mapping? mapping-contains? mapping-empty? mapping-disjoint?
   mapping-ref mapping-ref/default mapping-key-comparator
   mapping-set mapping-set! 
   mapping-replace mapping-replace!
   mapping-delete mapping-delete! mapping-delete-all mapping-delete-all!
   mapping-intern mapping-intern!
   mapping-update mapping-update!
   mapping-update/default mapping-update!/default
   mapping-search mapping-search!
   mapping-size mapping-find mapping-count mapping-any? mapping-every?
   mapping-keys mapping-values mapping-entries
   mapping-map mapping-map->list mapping-for-each mapping-fold
   mapping-filter mapping-filter!
   mapping-remove mapping-remove!
   mapping-partition mapping-partition!
   mapping-copy mapping->alist alist->mapping alist->mapping!
   mapping=? mapping<? mapping>? mapping<=? mapping>=?
   mapping-union mapping-intersection mapping-difference mapping-xor
   mapping-union! mapping-intersection! mapping-difference! mapping-xor!
   make-mapping-comparator
   mapping-comparator
   mapping-min-key mapping-max-key
   mapping-min-value mapping-max-value
   mapping-key-predecessor mapping-key-successor
   mapping-range= mapping-range< mapping-range> mapping-range<= mapping-range>=
   mapping-range=! mapping-range<! mapping-range>!
   mapping-range<=! mapping-range>=!
   mapping-split
   mapping-catenate mapping-catenate!
   mapping-map/monotone mapping-map/monotone!
   mapping-fold/reverse

   ;; (scheme mapping hash)

   hashmap hashmap-unfold
   hashmap? hashmap-contains? hashmap-empty? hashmap-disjoint?
   hashmap-ref hashmap-ref/default hashmap-key-comparator
   hashmap-adjoin hashmap-adjoin!
   hashmap-set hashmap-set!
   hashmap-replace hashmap-replace!
   hashmap-delete hashmap-delete! hashmap-delete-all hashmap-delete-all!
   hashmap-intern hashmap-intern!
   hashmap-update hashmap-update!
   hashmap-update/default hashmap-update!/default
   hashmap-pop hashmap-pop!
   hashmap-search hashmap-search!
   hashmap-size hashmap-find hashmap-count hashmap-any? hashmap-every?
   hashmap-keys hashmap-values hashmap-entries
   hashmap-map hashmap-map->list hashmap-for-each hashmap-fold
   hashmap-filter hashmap-filter!
   hashmap-remove hashmap-remove!
   hashmap-partition hashmap-partition!
   hashmap-copy hashmap->alist alist->hashmap alist->hashmap!
   hashmap=? hashmap<? hashmap>? hashmap<=? hashmap>=?
   hashmap-union hashmap-intersection hashmap-difference hashmap-xor
   hashmap-union! hashmap-intersection! hashmap-difference! hashmap-xor!
   make-hashmap-comparator
   hashmap-comparator
;  comparator?         ;; same as for (scheme comparator), (srfi 128)

   ;; (scheme regex)                         ; SRFI 115

   regexp regexp? valid-sre? rx regexp->sre char-set->sre
   regexp-matches regexp-matches? regexp-search
   regexp-replace regexp-replace-all
   regexp-fold regexp-extract regexp-split regexp-partition
   regexp-match? regexp-match-count
   regexp-match-submatch regexp-match-submatch/list
   regexp-match-submatch-start regexp-match-submatch-end
   regexp-match->list regexp-match->sexp

   ;; (scheme show)                          ; SRFI 159, SRFI 166

   ;; basic
   show displayed written written-shared written-simply escaped maybe-escaped
   numeric numeric/comma numeric/si numeric/fitted
   nl fl space-to tab-to nothing each each-in-list
   joined joined/prefix joined/suffix joined/last joined/dot
   joined/range padded padded/right padded/both
   trimmed trimmed/right trimmed/both trimmed/lazy
   fitted fitted/right fitted/both output-default
   ;; computations
   fn with with! forked call-with-output
   ;; state variables
   make-state-variable
   port row col width output writer pad-char ellipsis
   string-width substring/width substring/preserve
   radix precision decimal-sep decimal-align sign-rule
   comma-sep comma-rule word-separator? ambiguous-is-wide?
   pretty-environment
   ;; pretty
   pretty pretty-shared pretty-simply pretty-with-color
   ;; columnar
   columnar tabular wrapped wrapped/list wrapped/char
   justified from-file line-numbers
   ;; unicode
   terminal-aware
   string-terminal-width string-terminal-width/wide
   substring-terminal-width substring-terminal-width/wide
;  substring-terminal-width substring-terminal-width/wide
   substring-terminal-preserve
   upcased downcased
   ;; color
   as-red as-blue as-green as-cyan as-yellow
   as-magenta as-white as-black
   as-bold as-italic as-underline
   as-color as-true-color
   on-red on-blue on-green on-cyan on-yellow
   on-magenta on-white on-black
   on-color on-true-color
   
   ;; (scheme vector base)                   ; SRFI 160

   ;; All of these are redundant with exports from specialized
   ;; vector libraries; see below.

   #|
   make-u8vector make-s8vector make-u16vector make-s16vector
   make-u32vector make-s32vector make-u64vector make-s64vector
   make-f32vector make-f64vector make-c64vector make-c128vector

   u8vector s8vector u16vector s16vector
   u32vector s32vector u64vector s64vector
   f32vector f64vector c64vector c128vector

   u8vector? s8vector? u16vector? s16vector?
   u32vector? s32vector? u64vector? s64vector?
   f32vector? f64vector? c64vector? c128vector?

   u8vector-length s8vector-length u16vector-length s16vector-length
   u32vector-length s32vector-length u64vector-length s64vector-length
   f32vector-length f64vector-length c64vector-length c128vector-length

   u8vector-ref s8vector-ref u16vector-ref s16vector-ref
   u32vector-ref s32vector-ref u64vector-ref s64vector-ref
   f32vector-ref f64vector-ref c64vector-ref c128vector-ref

   u8vector-set! s8vector-set! u16vector-set! s16vector-set!
   u32vector-set! s32vector-set! u64vector-set! s64vector-set!
   f32vector-set! f64vector-set! c64vector-set! c128vector-set!

   u8vector->list s8vector->list u16vector->list s16vector->list
   u32vector->list s32vector->list u64vector->list s64vector->list
   f32vector->list f64vector->list c64vector->list c128vector->list

   list->u8vector list->s8vector list->u16vector list->s16vector
   list->u32vector list->s32vector list->u64vector list->s64vector
   list->f32vector list->f64vector list->c64vector list->c128vector

   u8? s8? u16? s16? u32? s32? u64? s64? f32? f64? c64? c128?
   |#

   ;; (scheme vector u8)

   ;; Constructors 
   make-u8vector u8vector
   u8vector-unfold u8vector-unfold-right
   u8vector-copy u8vector-reverse-copy 
   u8vector-append u8vector-concatenate
   u8vector-append-subvectors
   ;; Predicates 
   u8? u8vector? u8vector-empty? u8vector=
   ;; Selectors
   u8vector-ref u8vector-length
   ;; Iteration 
   u8vector-take u8vector-take-right
   u8vector-drop u8vector-drop-right
   u8vector-segment
   u8vector-fold u8vector-fold-right
   u8vector-map u8vector-map! u8vector-for-each
   u8vector-count u8vector-cumulate
   ;; Searching 
   u8vector-take-while u8vector-take-while-right
   u8vector-drop-while u8vector-drop-while-right
   u8vector-index u8vector-index-right u8vector-skip u8vector-skip-right 
   u8vector-any u8vector-every u8vector-partition
   u8vector-filter u8vector-remove
   ;; Mutators 
   u8vector-set! u8vector-swap! u8vector-fill! u8vector-reverse!
   u8vector-copy! u8vector-reverse-copy!
   u8vector-unfold! u8vector-unfold-right!
   ;; Conversion 
   u8vector->list list->u8vector
   reverse-u8vector->list reverse-list->u8vector
   u8vector->vector vector->u8vector
   ;; Misc
   make-u8vector-generator u8vector-comparator write-u8vector

   ;; (scheme vector s8)

   ;; Constructors 
   make-s8vector s8vector
   s8vector-unfold s8vector-unfold-right
   s8vector-copy s8vector-reverse-copy 
   s8vector-append s8vector-concatenate
   s8vector-append-subvectors
   ;; Predicates 
   s8? s8vector? s8vector-empty? s8vector=
   ;; Selectors
   s8vector-ref s8vector-length
   ;; Iteration 
   s8vector-take s8vector-take-right
   s8vector-drop s8vector-drop-right
   s8vector-segment
   s8vector-fold s8vector-fold-right
   s8vector-map s8vector-map! s8vector-for-each
   s8vector-count s8vector-cumulate
   ;; Searching 
   s8vector-take-while s8vector-take-while-right
   s8vector-drop-while s8vector-drop-while-right
   s8vector-index s8vector-index-right s8vector-skip s8vector-skip-right 
   s8vector-any s8vector-every s8vector-partition
   s8vector-filter s8vector-remove
   ;; Mutators 
   s8vector-set! s8vector-swap! s8vector-fill! s8vector-reverse!
   s8vector-copy! s8vector-reverse-copy!
   s8vector-unfold! s8vector-unfold-right!
   ;; Conversion 
   s8vector->list list->s8vector
   reverse-s8vector->list reverse-list->s8vector
   s8vector->vector vector->s8vector
   ;; Misc
   make-s8vector-generator s8vector-comparator write-s8vector

   ;; (scheme vector u16)

   ;; Constructors 
   make-u16vector u16vector
   u16vector-unfold u16vector-unfold-right
   u16vector-copy u16vector-reverse-copy 
   u16vector-append u16vector-concatenate
   u16vector-append-subvectors
   ;; Predicates 
   u16? u16vector? u16vector-empty? u16vector=
   ;; Selectors
   u16vector-ref u16vector-length
   ;; Iteration 
   u16vector-take u16vector-take-right
   u16vector-drop u16vector-drop-right
   u16vector-segment
   u16vector-fold u16vector-fold-right
   u16vector-map u16vector-map! u16vector-for-each
   u16vector-count u16vector-cumulate
   ;; Searching 
   u16vector-take-while u16vector-take-while-right
   u16vector-drop-while u16vector-drop-while-right
   u16vector-index u16vector-index-right u16vector-skip u16vector-skip-right 
   u16vector-any u16vector-every u16vector-partition
   u16vector-filter u16vector-remove
   ;; Mutators 
   u16vector-set! u16vector-swap! u16vector-fill! u16vector-reverse!
   u16vector-copy! u16vector-reverse-copy!
   u16vector-unfold! u16vector-unfold-right!
   ;; Conversion 
   u16vector->list list->u16vector
   reverse-u16vector->list reverse-list->u16vector
   u16vector->vector vector->u16vector
   ;; Misc
   make-u16vector-generator u16vector-comparator write-u16vector

   ;; (scheme vector s16)

   ;; Constructors 
   make-s16vector s16vector
   s16vector-unfold s16vector-unfold-right
   s16vector-copy s16vector-reverse-copy 
   s16vector-append s16vector-concatenate
   s16vector-append-subvectors
   ;; Predicates 
   s16? s16vector? s16vector-empty? s16vector=
   ;; Selectors
   s16vector-ref s16vector-length
   ;; Iteration 
   s16vector-take s16vector-take-right
   s16vector-drop s16vector-drop-right
   s16vector-segment
   s16vector-fold s16vector-fold-right
   s16vector-map s16vector-map! s16vector-for-each
   s16vector-count s16vector-cumulate
   ;; Searching 
   s16vector-take-while s16vector-take-while-right
   s16vector-drop-while s16vector-drop-while-right
   s16vector-index s16vector-index-right s16vector-skip s16vector-skip-right 
   s16vector-any s16vector-every s16vector-partition
   s16vector-filter s16vector-remove
   ;; Mutators 
   s16vector-set! s16vector-swap! s16vector-fill! s16vector-reverse!
   s16vector-copy! s16vector-reverse-copy!
   s16vector-unfold! s16vector-unfold-right!
   ;; Conversion 
   s16vector->list list->s16vector
   reverse-s16vector->list reverse-list->s16vector
   s16vector->vector vector->s16vector
   ;; Misc
   make-s16vector-generator s16vector-comparator write-s16vector

   ;; (scheme vector u32)

   ;; Constructors 
   make-u32vector u32vector
   u32vector-unfold u32vector-unfold-right
   u32vector-copy u32vector-reverse-copy 
   u32vector-append u32vector-concatenate
   u32vector-append-subvectors
   ;; Predicates 
   u32? u32vector? u32vector-empty? u32vector=
   ;; Selectors
   u32vector-ref u32vector-length
   ;; Iteration 
   u32vector-take u32vector-take-right
   u32vector-drop u32vector-drop-right
   u32vector-segment
   u32vector-fold u32vector-fold-right
   u32vector-map u32vector-map! u32vector-for-each
   u32vector-count u32vector-cumulate
   ;; Searching 
   u32vector-take-while u32vector-take-while-right
   u32vector-drop-while u32vector-drop-while-right
   u32vector-index u32vector-index-right u32vector-skip u32vector-skip-right 
   u32vector-any u32vector-every u32vector-partition
   u32vector-filter u32vector-remove
   ;; Mutators 
   u32vector-set! u32vector-swap! u32vector-fill! u32vector-reverse!
   u32vector-copy! u32vector-reverse-copy!
   u32vector-unfold! u32vector-unfold-right!
   ;; Conversion 
   u32vector->list list->u32vector
   reverse-u32vector->list reverse-list->u32vector
   u32vector->vector vector->u32vector
   ;; Misc
   make-u32vector-generator u32vector-comparator write-u32vector

   ;; (scheme vector s32)

   ;; Constructors 
   make-s32vector s32vector
   s32vector-unfold s32vector-unfold-right
   s32vector-copy s32vector-reverse-copy 
   s32vector-append s32vector-concatenate
   s32vector-append-subvectors
   ;; Predicates 
   s32? s32vector? s32vector-empty? s32vector=
   ;; Selectors
   s32vector-ref s32vector-length
   ;; Iteration 
   s32vector-take s32vector-take-right
   s32vector-drop s32vector-drop-right
   s32vector-segment
   s32vector-fold s32vector-fold-right
   s32vector-map s32vector-map! s32vector-for-each
   s32vector-count s32vector-cumulate
   ;; Searching 
   s32vector-take-while s32vector-take-while-right
   s32vector-drop-while s32vector-drop-while-right
   s32vector-index s32vector-index-right s32vector-skip s32vector-skip-right 
   s32vector-any s32vector-every s32vector-partition
   s32vector-filter s32vector-remove
   ;; Mutators 
   s32vector-set! s32vector-swap! s32vector-fill! s32vector-reverse!
   s32vector-copy! s32vector-reverse-copy!
   s32vector-unfold! s32vector-unfold-right!
   ;; Conversion 
   s32vector->list list->s32vector
   reverse-s32vector->list reverse-list->s32vector
   s32vector->vector vector->s32vector
   ;; Misc
   make-s32vector-generator s32vector-comparator write-s32vector

   ;; (scheme vector u64)

   ;; Constructors 
   make-u64vector u64vector
   u64vector-unfold u64vector-unfold-right
   u64vector-copy u64vector-reverse-copy 
   u64vector-append u64vector-concatenate
   u64vector-append-subvectors
   ;; Predicates 
   u64? u64vector? u64vector-empty? u64vector=
   ;; Selectors
   u64vector-ref u64vector-length
   ;; Iteration 
   u64vector-take u64vector-take-right
   u64vector-drop u64vector-drop-right
   u64vector-segment
   u64vector-fold u64vector-fold-right
   u64vector-map u64vector-map! u64vector-for-each
   u64vector-count u64vector-cumulate
   ;; Searching 
   u64vector-take-while u64vector-take-while-right
   u64vector-drop-while u64vector-drop-while-right
   u64vector-index u64vector-index-right u64vector-skip u64vector-skip-right 
   u64vector-any u64vector-every u64vector-partition
   u64vector-filter u64vector-remove
   ;; Mutators 
   u64vector-set! u64vector-swap! u64vector-fill! u64vector-reverse!
   u64vector-copy! u64vector-reverse-copy!
   u64vector-unfold! u64vector-unfold-right!
   ;; Conversion 
   u64vector->list list->u64vector
   reverse-u64vector->list reverse-list->u64vector
   u64vector->vector vector->u64vector
   ;; Misc
   make-u64vector-generator u64vector-comparator write-u64vector

   ;; (scheme vector s64)

   ;; Constructors 
   make-s64vector s64vector
   s64vector-unfold s64vector-unfold-right
   s64vector-copy s64vector-reverse-copy 
   s64vector-append s64vector-concatenate
   s64vector-append-subvectors
   ;; Predicates 
   s64? s64vector? s64vector-empty? s64vector=
   ;; Selectors
   s64vector-ref s64vector-length
   ;; Iteration 
   s64vector-take s64vector-take-right
   s64vector-drop s64vector-drop-right
   s64vector-segment
   s64vector-fold s64vector-fold-right
   s64vector-map s64vector-map! s64vector-for-each
   s64vector-count s64vector-cumulate
   ;; Searching 
   s64vector-take-while s64vector-take-while-right
   s64vector-drop-while s64vector-drop-while-right
   s64vector-index s64vector-index-right s64vector-skip s64vector-skip-right 
   s64vector-any s64vector-every s64vector-partition
   s64vector-filter s64vector-remove
   ;; Mutators 
   s64vector-set! s64vector-swap! s64vector-fill! s64vector-reverse!
   s64vector-copy! s64vector-reverse-copy!
   s64vector-unfold! s64vector-unfold-right!
   ;; Conversion 
   s64vector->list list->s64vector
   reverse-s64vector->list reverse-list->s64vector
   s64vector->vector vector->s64vector
   ;; Misc
   make-s64vector-generator s64vector-comparator write-s64vector

   ;; (scheme vector f32)

   ;; Constructors 
   make-f32vector f32vector
   f32vector-unfold f32vector-unfold-right
   f32vector-copy f32vector-reverse-copy 
   f32vector-append f32vector-concatenate
   f32vector-append-subvectors
   ;; Predicates 
   f32? f32vector? f32vector-empty? f32vector=
   ;; Selectors
   f32vector-ref f32vector-length
   ;; Iteration 
   f32vector-take f32vector-take-right
   f32vector-drop f32vector-drop-right
   f32vector-segment
   f32vector-fold f32vector-fold-right
   f32vector-map f32vector-map! f32vector-for-each
   f32vector-count f32vector-cumulate
   ;; Searching 
   f32vector-take-while f32vector-take-while-right
   f32vector-drop-while f32vector-drop-while-right
   f32vector-index f32vector-index-right f32vector-skip f32vector-skip-right 
   f32vector-any f32vector-every f32vector-partition
   f32vector-filter f32vector-remove
   ;; Mutators 
   f32vector-set! f32vector-swap! f32vector-fill! f32vector-reverse!
   f32vector-copy! f32vector-reverse-copy!
   f32vector-unfold! f32vector-unfold-right!
   ;; Conversion 
   f32vector->list list->f32vector
   reverse-f32vector->list reverse-list->f32vector
   f32vector->vector vector->f32vector
   ;; Misc
   make-f32vector-generator f32vector-comparator write-f32vector

   ;; (scheme vector f64)

   ;; Constructors 
   make-f64vector f64vector
   f64vector-unfold f64vector-unfold-right
   f64vector-copy f64vector-reverse-copy 
   f64vector-append f64vector-concatenate
   f64vector-append-subvectors
   ;; Predicates 
   f64? f64vector? f64vector-empty? f64vector=
   ;; Selectors
   f64vector-ref f64vector-length
   ;; Iteration 
   f64vector-take f64vector-take-right
   f64vector-drop f64vector-drop-right
   f64vector-segment
   f64vector-fold f64vector-fold-right
   f64vector-map f64vector-map! f64vector-for-each
   f64vector-count f64vector-cumulate
   ;; Searching 
   f64vector-take-while f64vector-take-while-right
   f64vector-drop-while f64vector-drop-while-right
   f64vector-index f64vector-index-right f64vector-skip f64vector-skip-right 
   f64vector-any f64vector-every f64vector-partition
   f64vector-filter f64vector-remove
   ;; Mutators 
   f64vector-set! f64vector-swap! f64vector-fill! f64vector-reverse!
   f64vector-copy! f64vector-reverse-copy!
   f64vector-unfold! f64vector-unfold-right!
   ;; Conversion 
   f64vector->list list->f64vector
   reverse-f64vector->list reverse-list->f64vector
   f64vector->vector vector->f64vector
   ;; Misc
   make-f64vector-generator f64vector-comparator write-f64vector
   
   ;; (scheme vector c64)

   ;; Constructors 
   make-c64vector c64vector
   c64vector-unfold c64vector-unfold-right
   c64vector-copy c64vector-reverse-copy 
   c64vector-append c64vector-concatenate
   c64vector-append-subvectors
   ;; Predicates 
   c64? c64vector? c64vector-empty? c64vector=
   ;; Selectors
   c64vector-ref c64vector-length
   ;; Iteration 
   c64vector-take c64vector-take-right
   c64vector-drop c64vector-drop-right
   c64vector-segment
   c64vector-fold c64vector-fold-right
   c64vector-map c64vector-map! c64vector-for-each
   c64vector-count c64vector-cumulate
   ;; Searching 
   c64vector-take-while c64vector-take-while-right
   c64vector-drop-while c64vector-drop-while-right
   c64vector-index c64vector-index-right c64vector-skip c64vector-skip-right 
   c64vector-any c64vector-every c64vector-partition
   c64vector-filter c64vector-remove
   ;; Mutators 
   c64vector-set! c64vector-swap! c64vector-fill! c64vector-reverse!
   c64vector-copy! c64vector-reverse-copy!
   c64vector-unfold! c64vector-unfold-right!
   ;; Conversion 
   c64vector->list list->c64vector
   reverse-c64vector->list reverse-list->c64vector
   c64vector->vector vector->c64vector
   ;; Misc
   make-c64vector-generator c64vector-comparator write-c64vector

   ;; (scheme vector c128)

   ;; Constructors 
   make-c128vector c128vector
   c128vector-unfold c128vector-unfold-right
   c128vector-copy c128vector-reverse-copy 
   c128vector-append c128vector-concatenate
   c128vector-append-subvectors
   ;; Predicates 
   c128? c128vector? c128vector-empty? c128vector=
   ;; Selectors
   c128vector-ref c128vector-length
   ;; Iteration 
   c128vector-take c128vector-take-right
   c128vector-drop c128vector-drop-right
   c128vector-segment
   c128vector-fold c128vector-fold-right
   c128vector-map c128vector-map! c128vector-for-each
   c128vector-count c128vector-cumulate
   ;; Searching 
   c128vector-take-while c128vector-take-while-right
   c128vector-drop-while c128vector-drop-while-right
   c128vector-index c128vector-index-right
   c128vector-skip c128vector-skip-right 
   c128vector-any c128vector-every c128vector-partition
   c128vector-filter c128vector-remove
   ;; Mutators 
   c128vector-set! c128vector-swap! c128vector-fill! c128vector-reverse!
   c128vector-copy! c128vector-reverse-copy!
   c128vector-unfold! c128vector-unfold-right!
   ;; Conversion 
   c128vector->list list->c128vector
   reverse-c128vector->list reverse-list->c128vector
   c128vector->vector vector->c128vector
   ;; Misc
   make-c128vector-generator c128vector-comparator write-c128vector

   )

  (import ;; R7RS (small) libraries

          (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme complex)
          (scheme cxr)
          (scheme eval)
          (scheme file)
          (scheme inexact)
          (scheme lazy)
          (scheme load)
          (scheme process-context)
          (scheme read) 
          (scheme repl)
          (scheme time)
          (scheme write)
          (scheme r5rs)

          ;; R6RS

          (rename (except (rnrs)
			  flnumerator
			  fldenominator)
		  (bytevector-copy  r6rs:bytevector-copy)
                  (bytevector-copy! r6rs:bytevector-copy!)
                  (remove           r6rs:remove)
		  (fxbit-set?       r6rs:fxbit-set?)
		  (fxcopy-bit       r6rs:fxcopy-bit))

          ;; R7RS Red Edition libraries

          (scheme list)                    ; SRFI 1
          (scheme vector)                  ; SRFI 133
          (scheme sort)                    ; SRFI 132
          (scheme set)                     ; SRFI 113
          (scheme charset)                 ; SRFI 14
          (except (scheme hash-table)      ; SRFI 125
                  string-hash
                  string-ci-hash)
          (scheme ilist)                   ; SRFI 116
          (scheme rlist)                   ; SRFI 101 with renamings
          (scheme ideque)                  ; SRFI 134
          (scheme text)                    ; SRFI 135
          (scheme generator)               ; SRFI 121
          (scheme lseq)                    ; SRFI 127
          (scheme stream)                  ; SRFI 41
          (scheme box)                     ; SRFI 111
          (scheme list-queue)              ; SRFI 117
          (scheme ephemeron)               ; SRFI 124
          (scheme comparator)              ; SRFI 128

	  ;; R7RS Tangerine Edition libraries

	  (scheme bitwise)                       ; SRFI 151
	  (except (scheme bytevector)            ; (rnrs bytevectors)
		  bytevector-copy
		  bytevector-copy!)
	  (scheme division)                      ; SRFI 141
	  (scheme generator)                     ; SRFI 158
	  (scheme fixnum)                        ; SRFI 143
	  (except (scheme flonum)                ; SRFI 144
		  flmax
		  flmin)
	  (scheme mapping)                       ; SRFI 146
	  (scheme mapping hash)
	  (scheme regex)                         ; SRFI 115
	  (scheme show)                          ; SRFI 159, SRFI 166
	  (scheme vector base)                   ; SRFI 160
	  (scheme vector u8)
	  (scheme vector s8)
	  (scheme vector u16)
	  (scheme vector s16)
	  (scheme vector u32)
	  (scheme vector s32)
	  (scheme vector u64)
	  (scheme vector s64)
	  (scheme vector f32)
	  (scheme vector f64)
	  (scheme vector c64)
	  (scheme vector c128)
          )

  (begin

   ;; WG1 voted to break backward compatibility with R6RS by
   ;; requiring let-syntax and letrec-syntax to have non-splicing
   ;; behavior.  The non-splicing semantics is provided by these
   ;; alternatives to Larceny's splicing let-syntax and letrec-syntax.

   (define-syntax r7rs:let-syntax
     (syntax-rules ()
      ((_ bindings . body)
       (let () (let-syntax bindings . body)))))

   (define-syntax r7rs:letrec-syntax
     (syntax-rules ()
      ((_ bindings . body)
       (let () (letrec-syntax bindings . body))))))

  )

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
   bytevector-copy
   bytevector-copy!
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
   fxbit-set?
   fxcopy-bit
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

          (rename (rnrs)
                  (bytevector-copy! r6rs:bytevector-copy!)
                  (remove           r6rs:remove))

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

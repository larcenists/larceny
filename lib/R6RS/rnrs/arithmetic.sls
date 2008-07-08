(library (rnrs arithmetic fixnums (6))

  (export
   fixnum? fixnum-width least-fixnum greatest-fixnum
   fx=? fx>? fx<? fx>=? fx<=?
   fxzero? fxpositive? fxnegative?
   fxodd? fxeven?
   fxmax fxmin
   fx+ fx- fx*
   fxdiv-and-mod fxdiv fxmod
   fxdiv0-and-mod0 fxdiv0 fxmod0
   fx+/carry fx-/carry fx*/carry
   fxnot fxand fxior fxxor
   fxif fxbit-count fxlength
   fxfirst-bit-set fxbit-set? fxcopy-bit fxbit-field fxcopy-bit-field
   fxrotate-bit-field fxreverse-bit-field
   fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right)

  (import
   (primitives
    fixnum? fixnum-width least-fixnum greatest-fixnum
    fx=? fx>? fx<? fx>=? fx<=?
    fxzero? fxpositive? fxnegative?
    fxodd? fxeven?
    fxmax fxmin
    fx+ fx- fx*
    fxdiv-and-mod fxdiv fxmod
    fxdiv0-and-mod0 fxdiv0 fxmod0
    fx+/carry fx-/carry fx*/carry
    fxnot fxand fxior fxxor
    fxif fxbit-count fxlength
    fxfirst-bit-set fxbit-set? fxcopy-bit fxbit-field fxcopy-bit-field
    fxrotate-bit-field fxreverse-bit-field
    fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right)))

(library (rnrs arithmetic flonums (6))
  
  (export
   flonum?
   real->flonum
   fl=? fl<? fl>? fl<=? fl>=?
   flinteger? flzero? flpositive? flnegative? flodd? fleven?
   flfinite? flinfinite? flnan?
   flmax flmin
   fl+ fl* fl- fl/
   flabs
   fldiv-and-mod fldiv flmod
   fldiv0-and-mod0 fldiv0 flmod0
   flnumerator fldenominator
   flfloor flceiling fltruncate flround
   flexp fllog flsin flcos fltan flasin flacos flatan
   flsqrt flexpt
   &no-infinities make-no-infinities-violation no-infinities-violation?
   &no-nans make-no-nans-violation no-nans-violation?
   fixnum->flonum)

  (import
   (primitives
    flonum?
    real->flonum
    fl=? fl<? fl>? fl<=? fl>=?
    flinteger? flzero? flpositive? flnegative? flodd? fleven?
    flfinite? flinfinite? flnan?
    flmax flmin
    fl+ fl* fl- fl/
    flabs
    fldiv-and-mod fldiv flmod
    fldiv0-and-mod0 fldiv0 flmod0
    flnumerator fldenominator
    flfloor flceiling fltruncate flround
    flexp fllog flsin flcos fltan flasin flacos flatan
    flsqrt flexpt
    &no-infinities make-no-infinities-violation no-infinities-violation?
    &no-nans make-no-nans-violation no-nans-violation?
    fixnum->flonum)))

(library (rnrs arithmetic bitwise (6))

  (export

   bitwise-not
   bitwise-and
   bitwise-ior
   bitwise-xor
   bitwise-if
   bitwise-bit-count
   bitwise-length
   bitwise-first-bit-set
   bitwise-bit-set?
   bitwise-copy-bit
   bitwise-bit-field
   bitwise-copy-bit-field
   bitwise-rotate-bit-field
   bitwise-reverse-bit-field
   bitwise-arithmetic-shift
   bitwise-arithmetic-shift-left
   bitwise-arithmetic-shift-right)

  (import
   (primitives

    bitwise-not
    bitwise-and
    bitwise-ior
    bitwise-xor
    bitwise-if
    bitwise-bit-count
    bitwise-length
    bitwise-first-bit-set
    bitwise-bit-set?
    bitwise-copy-bit
    bitwise-bit-field
    bitwise-copy-bit-field
    bitwise-arithmetic-shift
    bitwise-arithmetic-shift-left
    bitwise-arithmetic-shift-right
    bitwise-rotate-bit-field
    bitwise-reverse-bit-field)))


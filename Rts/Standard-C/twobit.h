/* Copyright 1998 Lars T Hansen.
 * 
 * $Id$
 *
 * Petit Larceny -- Twobit instruction and primitive macros.
 *
 * The code in this file makes some assumptions that are not guaranteed
 * by the ANSI/ISO C standard to be true, however they are on most
 * compilers:
 *   - Right-shift of an unsigned quantity shifts in 0s at the high end.
 *     Search for >> in the code to find all places where this happens.
 *
 *     Larceny needs to be delivered with a program that tests this!
 */

#ifndef TWOBIT_H
#define TWOBIT_H

#define MC_DEBUG          0

#define USE_CACHED_STATE  0	/* Experimental; works; must benchmark. */
#define MC_DEBUG_CACHED   0	/* Use this the first time around! */

#include "macros.h"
#include "cdefs.h"
#include "millicode.h"
#include "petit-hacks.h"
#include "assert.h"

#define SECOND   globals[ G_SECOND ]
#define THIRD    globals[ G_THIRD ]
#define TIMER    globals[ G_TIMER ]
#define STKP     globals[ G_STKP ]
#define STKLIM   globals[ G_STKLIM ]

#if USE_CACHED_STATE
  /* Cache the value of RESULT */
# if MC_DEBUG_CACHED
   /* Keep a state of cached or not and check on every save and restore */
   extern int cached_state;
#  define twobit_prologue()       word RESULT_; RESTORE_STATE()
#  define RESULT                  RESULT_
#  define SAVE_STATE() \
    do { assert( cached_state ); globals[ G_RESULT ]=RESULT_; \
         cached_state=0; \
    } while(0)
#  define RESTORE_STATE() \
    do { assert( !cached_state ); RESULT_ = globals[ G_RESULT ]; \
         cached_state=1; \
    } while(0)
# else
   /* Fast, non-checking code */
#  define twobit_prologue()       word RESULT_; RESTORE_STATE()
#  define RESULT                  RESULT_
#  define SAVE_STATE()            globals[ G_RESULT ] = RESULT_
#  define RESTORE_STATE()         RESULT_ = globals[ G_RESULT ]
# endif
#else
  /* Do not cache value of RESULT */
# define twobit_prologue()       (void)0
# define RESULT                  globals[ G_RESULT ]
# define SAVE_STATE()            (void)0
# define RESTORE_STATE()         (void)0
#endif

#define WITH_SAVED_STATE( x ) \
  do { SAVE_STATE(); \
       x; \
       RESTORE_STATE(); \
  } while(0)

/* Helper macros */

#if MC_DEBUG
# define integrity_check( name ) \
  WITH_SAVED_STATE( twobit_integrity_check( globals, name ) )
#else
# define integrity_check( name ) (void)0
#endif

#define reg( k )       globals[ G_REG0+(k) ]
#define stack( k )     *((word*)STKP + (k))
#define get_const( k ) *vec_addr( *proc_addr( reg(0), IDX_PROC_CONST ), k )
#define setcc( cond )  RESULT = ((cond) ? TRUE_CONST : FALSE_CONST)
#define framesize( n ) (n+2)	/* not STK_OVERHEAD -- counts header also */
#define realframesize( n )  roundup_walign(framesize(n)+1)

#define word_addr( x, tag, header_words, fixnum_offset ) \
  ((word*)((byte*)(x)+((header_words)*sizeof(word)-(tag))+(fixnum_offset)))

#define byte_addr( x, tag, header_words, fixnum_offset ) \
  ((byte*)(x)+((header_words)*sizeof(word)-(tag))+((fixnum_offset)>>2))

#define car_addr( x )  word_addr( x, PAIR_TAG, 0, fixnum(0) )
#define cdr_addr( x )  word_addr( x, PAIR_TAG, 0, fixnum(1) )
#define vec_addr( x, y )  word_addr( x, VEC_TAG, VEC_HEADER_WORDS, fixnum(y) )
#define proc_addr( x, y ) word_addr( x, PROC_TAG, PROC_HEADER_WORDS, fixnum(y))

#define the_header( x, tag ) (*(word*)((byte*)x-tag))

#define double_tag_test( x, tag, hdr, tmp ) \
  (tagof(x) == tag && ((tmp = the_header( x, tag ))&255) == hdr)

#define double_tag_predicate( x, tag, hdr ) \
  setcc( tagof(x) == tag && (the_header( x, tag )&255) == hdr )

/* make-vector, make-procedure */
#define make_vectorish( header_words, excode, header, ptrtag, init ) \
   do { int n; word *p; \
        SECOND = init; \
        if (nonnegative_fixnum( RESULT )) { \
          n = RESULT; \
          RESULT = n + fixnum( header_words ); \
          WITH_SAVED_STATE( mc_alloci( globals ) ); \
          p = (word*)RESULT; \
          p[0] = mkheader( n, header ); \
          RESULT = tagptr( p, ptrtag ); \
        } else { SAVE_STATE(); mc_exception( globals, excode ); } \
   } while(0)


#define implicit_label( e, k ) \
   WITH_SAVED_STATE( e ); twobit_skip( k ); \
   } /* END OF PROCEDURE */ \
   /* NOW START NEW PROCEDURE */ \
   static RTYPE k( CONT_PARAMS ) { \
     twobit_prologue(); 

/* From MIPS R4000 manual: Signed add with overflow detection.

   --- compute t0 = t1 + t2, branch to L on signed overflow
   addu t0, t1, t2
   xor  t3, t1, t2
   bltz t3, 1f
   --- t1 and t2 have same sign
   xor  t3, t0, t1
   bltz t3, L
1:
*/
#define twobit_add( x, y, k ) /* addition */ \
   do { word a = x, b = y; \
        if (both_fixnums( a, b )) { \
          word res = a + b; \
          if ((s_word)(a ^ b) < 0 || (s_word)(res ^ a) >= 0) { \
            RESULT = res; \
            twobit_skip(k); \
          } \
        } \
        SECOND = b; \
   } while(0); \
   implicit_label( mc_add( globals, (cont_t)k ), k )

/* From MIPS R4000 manual: Signed subtract with overflow detection.

   --- compute t0 - t1 - t2, branch to L on signed overflow
   subu t0, t1, t2
   xor  t3, t1, t2
   bgez t3, 1f
   --- t1 and t2 have different signs
   xor  t3, t0, t1
   bltz t3, L
1:
*/
#define twobit_subtract( x, y, k ) /* subtraction */ \
   do { word a = x, b = y; \
	if (both_fixnums( a, b )) { \
          word res = a - b; \
          if ((s_word)(a ^ b) >= 0 || (s_word)(res ^ a) >= 0) { \
            RESULT = res; \
            twobit_skip(k); \
          } \
        } \
        SECOND = b; \
   } while(0); \
   implicit_label( mc_sub( globals, (cont_t)k ), k )

/* Comparison (=, <, <=, >, >=) */
#define twobit_compare( x, y, op, generic, k ) /* numeric comparison */ \
  do { word a = x, b = y; \
       if (both_fixnums( a, b )) { \
         setcc( (int)a op (int)b ); \
         twobit_skip( k ); \
       } \
       SECOND = b; \
  } while(0); \
  implicit_label( generic( globals, (cont_t)k ), k )


/* Twobit instructions */

#define twobit_imm_const( w ) \
  RESULT = (word)(w); integrity_check( "imm_const" )

#define twobit_const( k ) \
  RESULT = get_const( k ); integrity_check( "const" )

#define twobit_global( k ) \
  RESULT = global_cell_ref( get_const( k ) ); integrity_check( "global" )

#define twobit_setglbl( k ) \
  do { word b=SECOND=RESULT; \
       word a=RESULT=get_const( k ); \
       global_cell_set( a, b ); \
       WITH_SAVED_STATE( mc_full_barrier( globals ) ); \
       integrity_check( "setglbl" ); \
  } while(0)

#define init_closure( regs, p ) \
  do { if (regs >= LASTREG) { \
         word l = reg(LASTREG); int i=LASTREG; \
         do { p[ i+PROC_REG0 ] = *car_addr( l ); l = *cdr_addr( l ); \
	      regs--; i++; \
         } while (regs >= LASTREG); \
       } \
       while (regs >= 0) { p[regs+PROC_REG0] = reg(regs); regs--; }; \
       RESULT = tagptr( p, PROC_TAG ); \
  } while(0)

#define twobit_lambda( code_name, const_offs, n ) \
  do { int regs = n; word *p; \
       RESULT = fixnum( PROC_HEADER_WORDS+PROC_OVERHEAD+regs+1 ); \
       WITH_SAVED_STATE( mc_alloc( globals ) ); \
       p = (word*)RESULT; \
       p[0] = mkheader( (regs+1+PROC_OVERHEAD)*sizeof(word), PROC_HDR ); \
       p[PROC_CODE] = (word)code_name; \
       p[PROC_CONST] = get_const( const_offs ); \
       init_closure( regs, p ); \
       integrity_check( "lambda" ); \
  } while(0)

#define twobit_lexes( n ) \
  do { int regs = n; word *p; \
       RESULT = fixnum( PROC_HEADER_WORDS+PROC_OVERHEAD+regs+1 ); \
       WITH_SAVED_STATE( mc_alloc( globals ) ); \
       p = (word*)RESULT; \
       p[0] = mkheader( (regs+1+PROC_OVERHEAD)*sizeof(word), PROC_HDR ); \
       p[PROC_CODE] = *proc_addr( reg(0), IDX_PROC_CODE ); \
       p[PROC_CONST] = *proc_addr( reg(0), IDX_PROC_CONST ); \
       init_closure( regs, p ); \
       integrity_check( "lexes" ); \
  } while(0)

#define twobit_argseq( n ) \
   do { if (RESULT != fixnum(n)) { \
          SAVE_STATE(); mc_exception( globals, EX_ARGSEQ ); \
        } \
        integrity_check( "argseq" ); \
   } while(0)

#define twobit_argsge( n ) \
   do { SECOND=fixnum(n); \
        if (RESULT < fixnum(n)) { \
          SAVE_STATE(); mc_exception( globals, EX_ARGSGE ); \
        } \
        else WITH_SAVED_STATE( mc_restargs( globals ) ); \
        integrity_check( "argsge" ); \
   } while(0)

#define twobit_restore( n ) \
  do { int i = n; \
       while ( i >= 0 ) { reg(i) = stack(i+STK_REG0); i--; } \
       integrity_check( "restore" ); \
  } while(0)

#define twobit_pop( n ) \
  STKP = (word)((word*)STKP + realframesize(n+1)); integrity_check( "pop" )

#define twobit_stack( n ) \
  RESULT = stack(n+STK_REG0); integrity_check( "stack" )

#define twobit_setstk( n ) \
  stack(n+STK_REG0) = RESULT; integrity_check( "setstk" )

#define twobit_load( k, n ) \
  reg(k) = stack(n+STK_REG0); integrity_check( "load" )

#define twobit_store( k, n ) \
  stack(n+STK_REG0) = reg(k); integrity_check( "store" )

#define twobit_lexical( m, n ) \
  do { int i; word p=reg(0); \
       for ( i=m ; i > 0 ; i-- ) p=*proc_addr( p, IDX_PROC_REG0 ); \
       RESULT = *proc_addr( p, IDX_PROC_REG0+n ); \
       integrity_check( "lexical" ); \
  } while(0)

#define twobit_setlex( m, n ) \
  do { int i; word p=reg(0); \
       for ( i=m ; i > 0 ; i-- ) p=*proc_addr( p, IDX_PROC_REG0 ); \
       *proc_addr( p, IDX_PROC_REG0+n ) = RESULT; \
       SECOND = RESULT; RESULT = p; \
       WITH_SAVED_STATE( mc_full_barrier( globals ) ); \
       integrity_check( "setlex" ); \
  } while(0)

#define twobit_reg( k ) RESULT = reg(k); integrity_check( "reg" )

#define twobit_setreg( k )  reg(k) = RESULT; integrity_check( "setreg" )

#define twobit_movereg( k, l )  reg(l) = reg(k); integrity_check( "movereg" )

#define twobit_nop()  (void)0 /* nothing */

#define twobit_save( n ) \
  do { int i=n; word *stkp; \
       do { stkp = (word*)STKP-realframesize(n+1); \
            if (stkp >= (word*)STKLIM) break; \
            WITH_SAVED_STATE( mc_stack_overflow( globals ) ); \
       } while(1); \
       STKP = (word)stkp; \
       stkp[ STK_FRAMESIZE ] = fixnum(framesize(n+1)); \
       stkp[ STK_RETURN ] = 0; \
       stkp[ STK_REG0 ] = reg(0); \
       while (i > 0) { stkp[ STK_REG0+i ] = UNSPECIFIED_CONST; i--; } \
       integrity_check( "save" ); \
  } while (0)

#define twobit_setrtn( L )  \
  do { stack( STK_RETURN ) = (word) L; \
       integrity_check( "setrtn" ); \
  } while(0)

#if USE_LONGJUMP
# define twobit_return() \
  do { cont_t retL = (cont_t) stack( STK_RETURN ); \
       integrity_check( "return" ); \
       if (--TIMER == 0) { \
         c_label = retL; \
         WITH_SAVED_STATE( mc_timer_exception( globals, retL ) ); \
       } \
       else { SAVE_STATE(); retL( CONT_ACTUALS ); } \
  } while(0)
#elif USE_RETURN_WITH_VALUE
# define twobit_return() \
  do { word r; \
       integrity_check( "return" ); \
       r = stack( STK_RETURN ); \
       SAVE_STATE(); \
       return (cont_t) r; \
  } while(0)
#elif USE_RETURN_WITHOUT_VALUE
# define twobit_return() \
  do { c_label = (cont_t) stack( STK_RETURN ); \
       integrity_check( "return" ); \
       SAVE_STATE(); \
       return;
  } while(0)
#endif

#define twobit_invoke( n ) \
  do { cont_t invL; word a=RESULT; \
       if (tagof(a) != PROC_TAG) { \
         SAVE_STATE(); mc_exception( globals, EX_NONPROC ); \
       } \
       invL = (cont_t)*proc_addr( a, IDX_PROC_CODE ); \
       reg(0) = a; \
       RESULT = fixnum(n); \
       integrity_check( "invoke" ); \
       twobit_branch( invL ); \
  } while(0)

#define twobit_apply( k, l ) \
  do { cont_t appL = (cont_t)*proc_addr( RESULT, IDX_PROC_CODE ); \
       SECOND=reg(k); THIRD=reg(l); \
       WITH_SAVED_STATE( mc_apply( globals ) ); \
       integrity_check( "apply" ); \
       twobit_branch( appL ); \
  } while (0)

#define twobit_jump( n, L ) \
  do { int i; word p=reg(0); \
       for ( i=n ; i > 0 ; i-- ) p=*proc_addr( p, IDX_PROC_REG0 );\
       reg(0) = p; \
       integrity_check( "jump" ); \
       twobit_branch( L ); \
  } while(0)

#if USE_LONGJUMP
# define twobit_skip( L ) \
  do { c_label = (cont_t)L; SAVE_STATE(); L( CONT_ACTUALS ); } while(0)
#elif USE_RETURN_WITH_VALUE
# define twobit_skip( L ) \
  do { SAVE_STATE(); return (cont_t)L; } while(0)
#elif USE_RETURN_WITHOUT_VALUE 
# define twobit_skip( L ) \
  do { c_label = (cont_t)L; SAVE_STATE(); return; } while(0)
#endif

#if USE_LONGJUMP
# define twobit_branch( L ) \
  do { if (--TIMER == 0) \
         WITH_SAVED_STATE( mc_timer_exception( globals, (cont_t)L ) ); \
       integrity_check( "branch" ); \
       twobit_skip( L ); \
  } while(0)
#elif USE_RETURN_WITH_VALUE
# define twobit_branch( L ) \
  do { if (--TIMER == 0) \
         WITH_SAVED_STATE( mc_timer_exception( globals, (cont_t)L ) ); \
       integrity_check( "branch" ); \
       twobit_skip( L ); \
  } while(0)
#elif USE_RETURN_WITHOUT_VALUE
# define twobit_branch( L ) \
  do { if (--TIMER == 0) \
         WITH_SAVED_STATE( mc_timer_exception( globals, (cont_t)L ) ); \
       integrity_check( "branch" ); \
       twobit_skip( L ); \
  } while(0)
#endif

#define twobit_branchf( L ) \
  if (RESULT == FALSE_CONST) twobit_branch( L )


/* Primitives */

#define twobit_op1_1() /* break -- really 0 args */ \
   WITH_SAVED_STATE( mc_break( globals ) )

#define twobit_op1_3() /* unspecified -- really 0 args */ \
   RESULT = UNSPECIFIED_CONST

#define twobit_op1_4() /* undefined -- really 0 args */ \
   RESULT = UNDEFINED_CONST

#define twobit_op1_5() /* eof-object -- really 0 args */ \
   RESULT = EOF_CONST

#define twobit_op1_6( k ) /* enable-interrupts */ \
   implicit_label( mc_enable_interrupts( globals, (cont_t)k ), k )

#define twobit_op1_7( k ) /* disable-interrupts -- really 0 args */ \
   implicit_label( mc_disable_interrupts( globals, (cont_t)k ), k )

#define twobit_op1_8() /* typetag */ \
   WITH_SAVED_STATE( mc_typetag( globals ) )

#define twobit_op1_9() /* not */ \
   setcc( RESULT == FALSE_CONST )

#define twobit_op1_10() /* null? */ \
   setcc( RESULT == NIL_CONST )

#define twobit_op1_11() /* pair? */ \
   setcc( tagof( RESULT ) == PAIR_TAG )

#define twobit_op1_12() /* eof-object? */ \
   setcc( RESULT == EOF_CONST )

#define twobit_op1_13() /* port? */ \
   double_tag_predicate( RESULT, VEC_TAG, PORT_HDR )

#define twobit_op1_14() /* structure? */ \
   double_tag_predicate( RESULT, VEC_TAG, STRUCT_HDR )

#define twobit_op1_15() /* car */ \
   do { word a=RESULT; \
	if (tagof( a ) == PAIR_TAG) RESULT = *car_addr( a ); \
        else { SAVE_STATE(); mc_exception( globals, EX_CAR ); } \
   } while(0)

#define twobit_op1_16() /* cdr */ \
   do { word a=RESULT; \
        if (tagof( a ) == PAIR_TAG) RESULT = *cdr_addr( a ); \
        else { SAVE_STATE(); mc_exception( globals, EX_CDR ); } \
   } while(0)

#define twobit_op1_17() /* symbol? */ \
   double_tag_predicate( RESULT, VEC_TAG, SYMBOL_HDR )

#define twobit_op1_18() /* number? */ \
   WITH_SAVED_STATE( mc_complexp( globals ) )

#define twobit_op1_19() /* complex? */ \
   WITH_SAVED_STATE( mc_complexp( globals ) )

#define twobit_op1_20() /* real? */ \
   WITH_SAVED_STATE( mc_rationalp( globals ) )

#define twobit_op1_20() /* rational? */ \
   WITH_SAVED_STATE( mc_rationalp( globals ) )

#define twobit_op1_21() /* compnum? */ \
   double_tag_predicate( RESULT, BVEC_TAG, COMPNUM_HDR )

#define twobit_op1_22() /* integer? */ \
   WITH_SAVED_STATE( mc_integerp( globals ) )

#define twobit_op1_23() /* fixnum? */ \
   setcc( is_fixnum(RESULT) )

#define twobit_op1_24() /* flonum? */ \
   double_tag_predicate( RESULT, BVEC_TAG, FLONUM_HDR )

#define twobit_op1_25() /* exact? */ \
   WITH_SAVED_STATE( mc_exactp( globals ) )

#define twobit_op1_26() /* inexact? */ \
   WITH_SAVED_STATE( mc_inexactp( globals ) )

#define twobit_op1_27( k ) /* exact->inexact */ \
   implicit_label( mc_exact2inexact( globals, (cont_t)k ), k )

#define twobit_op1_28( k ) /* inexact->exact */ \
   implicit_label( mc_inexact2exact( globals, (cont_t)k ), k )

#define twobit_op1_29( k ) /* round */ \
   implicit_label( mc_round( globals, (cont_t)k ), k )

#define twobit_op1_30( k ) /* truncate */ \
   implicit_label( mc_truncate( globals, (cont_t)k ), k )

/* Note: label not actually used; millicode handles everything */
#define twobit_op1_31( k ) /* zero? */ \
   WITH_SAVED_STATE( mc_zerop( globals ) )

#define twobit_op1_32( k ) /* -- */ \
   implicit_label( mc_neg( globals, (cont_t)k ), k )

#define twobit_op1_33() /* lognot */ \
   do { word a=RESULT; \
        if (is_fixnum(a)) RESULT = ~a & ~3; \
        else { SAVE_STATE(); mc_exception( globals, EX_LOGNOT ); } \
   } while(0)

#define twobit_op1_34() /* real-part */ \
   WITH_SAVED_STATE( mc_real_part( globals ) )

#define twobit_op1_35() /* imag-part */ \
   WITH_SAVED_STATE( mc_imag_part( globals ) )

#define twobit_op1_36() /* char? */ \
   setcc( is_char( RESULT ) )

#define twobit_op1_37() /* char->integer */ \
   do { word a=RESULT; \
        if (is_char(a)) RESULT = charcode_as_fixnum(a); \
        else { SAVE_STATE(); mc_exception( globals, EX_CHAR2INT ); } \
   } while(0)

#define twobit_op1_38() /* integer->char */ \
   do { word a=RESULT; \
        if (is_fixnum(a)) RESULT = fixnum_to_char(a); \
        else { SAVE_STATE(); mc_exception( globals, EX_INT2CHAR ); } \
   } while(0)

#define twobit_op1_39() /* string? */ \
   double_tag_predicate( RESULT, BVEC_TAG, STR_HDR )

#define twobit_op1_40() /* string-length */ \
   do { word h, a=RESULT; \
        if (double_tag_test( a, BVEC_TAG, STR_HDR, h )) \
          RESULT = sizefield(h) << 2; \
        else { SAVE_STATE(); mc_exception( globals, EX_STRING_LENGTH ); }\
        integrity_check( "string-length" ); \
   } while( 0 )

#define twobit_op1_41() /* vector? */ \
   double_tag_predicate( RESULT, VEC_TAG, VECTOR_HDR )

#define twobit_op1_42() /* vector-length */ \
   do { word h, a=RESULT; \
        if (double_tag_test( a, VEC_TAG, VECTOR_HDR, h )) \
          RESULT = h >> 8; \
        else { SAVE_STATE(); mc_exception( globals, EX_VECTOR_LENGTH ); }\
        integrity_check( "vector-length" ); \
   } while(0)

#define twobit_op1_43() /* bytevector? */ \
   double_tag_predicate( RESULT, BVEC_TAG, BYTEVECTOR_HDR )

#define twobit_op1_44() /* bytevector-length */ \
   do { word h, a=RESULT; \
        if (double_tag_test( a, BVEC_TAG, BYTEVECTOR_HDR, h )) \
          RESULT = fixnum(sizefield(h)); \
        else { SAVE_STATE(); \
	       mc_exception( globals, EX_BYTEVECTOR_LENGTH ); } \
        integrity_check( "bytevector-length" ); \
   } while(0)

#define twobit_op2_45( y ) /* bytevector-fill! */ \
   do { word b=SECOND=reg(y); \
        if (tagof( RESULT ) == BVEC_TAG && is_fixnum( b )) \
          WITH_SAVED_STATE( mc_bytevector_like_fill( globals ) ); \
        else { SAVE_STATE(); mc_exception( globals, EX_BVFILL ); } \
        integrity_check( "bytevector-fill!" ); \
   } while(0)

#define twobit_op1_46() /* make-bytevector */ \
   do { word a=RESULT, *p; \
        if (nonnegative_fixnum(a)) { \
          word size = a >> 2; \
	  RESULT = a + fixnum(BVEC_HEADER_WORDS*sizeof( word )); \
	  WITH_SAVED_STATE( mc_alloc_bv( globals ) ); \
          p =(word*)RESULT; \
          *p = mkheader( size, BV_HDR ); \
	  RESULT = tagptr( p, BVEC_TAG ); \
        } else { SAVE_STATE(); mc_exception( globals, EX_MKBVL ); } \
        integrity_check( "make-bytevector" ); \
   } while(0)

#define twobit_op1_47() /* procedure? */ \
   setcc( tagof( RESULT ) == PROC_TAG )

#define twobit_op1_48() /* procedure-length */ \
   do { word a=RESULT; \
        if (tagof( a ) == PROC_TAG) \
          RESULT = sizefield(*striptag(a,PROC_TAG)) & ~3; \
        else { SAVE_STATE(); \
	       mc_exception( globals, EX_PROCEDURE_LENGTH ); } \
   } while(0)

#define twobit_op1_49() /* make-procedure */ \
   make_vectorish( PROC_HEADER_WORDS, EX_MAKE_PROCEDURE, \
		   PROC_HDR, PROC_TAG, NIL_CONST )

/* 50 missing */

/* 51 missing */

#define twobit_op1_52() /* make-cell */ \
   do { word *p; \
        THIRD = RESULT; \
	RESULT = fixnum(2); \
        WITH_SAVED_STATE( mc_alloc( globals ) ); \
        p = (word*)RESULT;  \
        p[0] = THIRD; \
        p[1] = NIL_CONST; \
        RESULT = tagptr( p, PAIR_TAG ); \
   } while(0)

/* 53 missing */

#define twobit_op1_54() /* cell-ref */ \
   RESULT = *car_addr( RESULT )

#define twobit_op2_55( y ) /* typetag-set! */ \
   SECOND = reg(y); WITH_SAVED_STATE( mc_typetag_set( globals ) )

#define twobit_op2_56( y ) /* eq? */ \
   setcc( RESULT == reg(y) )

#define twobit_op2_57( y, k ) /* eqv? */ \
   implicit_label( SECOND = reg(y); mc_eqv( globals, (cont_t)k ), k )

#define twobit_op2_58( y ) /* cons */ \
   do { word *p; \
        THIRD = RESULT; \
	RESULT = fixnum(2); \
        WITH_SAVED_STATE( mc_alloc( globals ) ); \
        p = (word*)RESULT;  \
        p[0] = THIRD; \
        p[1] = reg(y); \
        RESULT = tagptr( p, PAIR_TAG ); \
        integrity_check( "cons" ); \
   } while(0)

#define twobit_op2_59( y ) /* set-car! */ \
   do { word a=RESULT, b=SECOND=reg(y); \
	if (tagof( a ) == PAIR_TAG) { \
	  *car_addr( a ) = b; \
	  WITH_SAVED_STATE( mc_full_barrier( globals ) ); \
        } \
	else { SAVE_STATE(); mc_exception( globals, EX_SETCAR ); } \
   } while(0)

#define twobit_op2_60( y ) /* set-cdr! */ \
   do { word a=RESULT, b=SECOND=reg(y); \
	if (tagof( a ) == PAIR_TAG) { \
	  *cdr_addr( a ) = b; \
	  WITH_SAVED_STATE( mc_full_barrier( globals ) ); \
        } \
	else { SAVE_STATE(); mc_exception( globals, EX_SETCDR ); } \
   } while(0)

#define twobit_op2_61( y, k ) /* + */ \
   twobit_add( RESULT, reg( y ), k )

#define twobit_op2_62( y, k ) /* - */ \
   twobit_subtract( RESULT, reg( y ), k )

#define twobit_op2_63( y, k ) /* * */ \
   implicit_label( SECOND = reg(y); mc_mul( globals, (cont_t)k ), k )

#define twobit_op2_64( y, k ) /* / */ \
   implicit_label( SECOND = reg(y); mc_div( globals, (cont_t)k ), k )

#define twobit_op2_65( y, k ) /* quotient */ \
   implicit_label( SECOND = reg(y); mc_quo( globals, (cont_t)k ), k )

#define twobit_op2_66( y, k ) /* < */ \
  twobit_compare( RESULT, reg(y), <, mc_lessp, k )

#define twobit_op2_67( y, k ) /* <= */ \
  twobit_compare( RESULT, reg(y), <=, mc_less_or_equalp, k )

#define twobit_op2_68( y, k ) /* = */ \
  twobit_compare( RESULT, reg(y), ==, mc_equalp, k )

#define twobit_op2_69( y, k ) /* > */ \
  twobit_compare( RESULT, reg(y), >, mc_greaterp, k )

#define twobit_op2_70( y, k ) /* >= */ \
  twobit_compare( RESULT, reg(y), >=, mc_greater_or_equalp, k )

#define twobit_op2_71( y ) /* logand */ \
   do { word a=RESULT, b=reg(y); \
        if (both_fixnums( a, b )) \
          RESULT = (a & b); \
        else { SECOND=reg(y); \
	       SAVE_STATE(); mc_exception( globals, EX_LOGAND ); } \
   } while(0)

#define twobit_op2_72( y ) /* logior */ \
   do { word a=RESULT, b=reg(y); \
        if (both_fixnums( a, b )) \
          RESULT = (a | b); \
        else { SECOND=reg(y); \
	       SAVE_STATE(); mc_exception( globals, EX_LOGIOR ); } \
   } while(0)

#define twobit_op2_73( y ) /* logxor */ \
   do { word a=RESULT, b=reg(y); \
        if (both_fixnums( a, b )) \
          RESULT = (a ^ b); \
        else { SECOND=reg(y); \
	       SAVE_STATE(); mc_exception( globals, EX_LOGXOR ); } \
   } while(0)

#define twobit_op2_74( y ) /* lsh */ \
   do { word a=RESULT, b=reg(y); \
        if (both_fixnums( a, b ) && b < fixnum(32)) \
          RESULT = a << (b >> 2); \
        else { SECOND=b; \
	       SAVE_STATE(); mc_exception( globals, EX_LSH ); } \
   } while(0)

#define twobit_op2_75( y ) /* rsha */ \
   do { word a=RESULT, b=reg(y); \
        if (both_fixnums( a, b ) && b < fixnum(32)) \
          RESULT = ((s_word)a >> (b >> 2)) & ~3; \
        else { SECOND=b; \
               SAVE_STATE(); mc_exception( globals, EX_RSHL ); } \
   } while(0)

#define twobit_op2_76( y ) /* rshl */ \
   do { word a=RESULT, b=reg(y); \
        if (both_fixnums( a, b ) && b < fixnum(32)) \
          RESULT = (a >> (b >> 2)) & ~3; \
        else { SECOND=b; \
               SAVE_STATE(); mc_exception( globals, EX_RSHL ); } \
   } while(0)

#define twobit_op2_77( y ) /* rot */ \
   panic( "twobit_rot not implemented." )

#define twobit_op2_78( y ) /* string-ref */ \
   do { word a=RESULT, b=reg(y), h; \
        if (double_tag_test( a, BVEC_TAG, STR_HDR, h ) && \
	    is_fixnum(b) && \
	    (b >> 2) < (h >> 8)) \
	 RESULT = int_to_char(*byte_addr( a, BVEC_TAG, BVEC_HEADER_WORDS, b));\
        else { SECOND=b; \
               SAVE_STATE(); mc_exception( globals, EX_STRING_REF ); } \
   } while(0)

#define twobit_op3_79( y, z ) /* string-set! */ \
   do { word a=RESULT, b=reg(y), c=reg(z), h; \
        if (double_tag_test( a, BVEC_TAG, STR_HDR, h ) && \
	    is_fixnum(b) && \
	    (b >> 2) < (h >> 8) && \
	    is_char( c )) \
	  *byte_addr( a, BVEC_TAG, BVEC_HEADER_WORDS, b ) = charcode(c); \
        else { SECOND=b; THIRD=c; \
               SAVE_STATE(); mc_exception( globals, EX_STRING_SET ); } \
   } while(0)

#define twobit_op2_80( y ) /* make-vector */ \
   make_vectorish( VEC_HEADER_WORDS, EX_MAKE_VECTOR, VEC_HDR, VEC_TAG, reg(y) )

#define twobit_op2_81( y ) /* vector-ref */ \
  do { word a=RESULT, b=reg(y), h; \
       if (double_tag_test( a, VEC_TAG, VECTOR_HDR, h ) && \
           is_fixnum(b) && \
           b < (h >> 8)) \
         RESULT = *word_addr( a, VEC_TAG, VEC_HEADER_WORDS, b ); \
       else { SECOND=b; SAVE_STATE(); \
	      mc_exception( globals, EX_VECTOR_REF ); } \
  } while(0)

#define twobit_op2_82( y ) /* bytevector-ref */ \
   do { word a=RESULT, b=reg( y ), h; \
        if (double_tag_test( a, BVEC_TAG, BV_HDR, h ) && \
	    is_fixnum(b) && \
	    (b >> 2) < (h >> 8)) \
	  RESULT = fixnum(*byte_addr( a, BVEC_TAG, BVEC_HEADER_WORDS, b )); \
        else { SECOND=b; SAVE_STATE(); \
	       mc_exception( globals, EX_BYTEVECTOR_REF ); } \
   } while(0)

#define twobit_op2_83( y ) /* procedure-ref */ \
  do { word a=RESULT, b=reg(y); \
       if (tagof( a ) == PROC_TAG && \
           is_fixnum(b) && \
           (b < (the_header( a, PROC_TAG ) >> 8))) \
        RESULT = *word_addr( a, PROC_TAG, PROC_HEADER_WORDS, b);\
       else { SECOND=b; SAVE_STATE(); \
	      mc_exception( globals, EX_PROCEDURE_REF ); } \
  } while(0)

#define twobit_op2_84( y ) /* cell-set! */ \
  do { word b=SECOND=reg(y); \
       *car_addr( RESULT ) = b; \
       WITH_SAVED_STATE( mc_full_barrier( globals ) ); \
  } while(0)

#define charcmp( op, y, ex ) /* generic character comparison */ \
  do { word a=RESULT, b=SECOND=reg(y); \
       if (is_char(a) && is_char(b)) \
	 setcc( a op b ); \
       else { SAVE_STATE(); mc_exception( globals, ex ); } \
  } while(0)

#define twobit_op2_85( y ) /* char<? */ \
  charcmp( <, y, EX_CHARLT )

#define twobit_op2_86( y ) /* char<=? */ \
  charcmp( <=, y, EX_CHARLE )

#define twobit_op2_87( y ) /* char=? */ \
  charcmp( ==, y, EX_CHAREQ )

#define twobit_op2_88( y ) /* char>? */ \
  charcmp( >, y, EX_CHARGT )

#define twobit_op2_89( y ) /* char>=? */ \
  charcmp( >=, y, EX_CHARGE )

#define twobit_op2_90( y ) /* sys$partial-list->vector */ \
  SECOND = reg(y); WITH_SAVED_STATE( mc_partial_list2vector( globals ) )

#define twobit_op3_91( y, z ) /* vector-set! */ \
  do { word a=RESULT, b=reg(y), c=reg(z), h; \
       if (double_tag_test( a, VEC_TAG, VECTOR_HDR, h ) && \
           is_fixnum(b) && \
           b < (h >> 8)) { \
         *word_addr( a, VEC_TAG, VEC_HEADER_WORDS, b ) = c; \
         SECOND = c; \
	 WITH_SAVED_STATE( mc_full_barrier( globals ) ); \
       } else { SECOND=b; THIRD=c; SAVE_STATE(); \
		mc_exception( globals, EX_VECTOR_SET ); \
       } \
  } while(0)

#define twobit_op3_92( y, z ) /* bytevector-set! */ \
   do { word h, a=RESULT, b=reg( y ), c=reg( z ); \
        if (double_tag_test( a, BVEC_TAG, BV_HDR, h ) && \
	    both_fixnums( b, c ) && \
            (b >> 2) < (h >> 8) && \
	    c < fixnum(256)) \
	  *byte_addr( a, BVEC_TAG, BVEC_HEADER_WORDS, b ) = (c >> 2); \
        else { SECOND=b; THIRD=c; SAVE_STATE(); \
               mc_exception( globals, EX_BYTEVECTOR_SET ); } \
   } while(0)

#define twobit_op3_93( y, z ) /* procedure-set! */ \
  do { word a=RESULT, b=reg(y), c=reg(z), h; \
       if (tagof( a ) == PROC_TAG && \
           is_fixnum(b) && \
           (b < (the_header( a, PROC_TAG ) >> 8))) { \
         *word_addr(a, PROC_TAG, PROC_HEADER_WORDS, b) = c; \
         SECOND=c; \
	 WITH_SAVED_STATE( mc_full_barrier( globals ) ); \
       } else { SECOND=b; THIRD=c; SAVE_STATE(); \
		mc_exception( globals, EX_PROCEDURE_SET ); \
       } \
  } while(0)

#define twobit_op1_94() /* bytevector-like? */ \
  setcc( tagof( RESULT ) == BVEC_TAG )

#define twobit_op1_95() /* vector-like? */ \
  setcc( tagof( RESULT ) == VEC_TAG )

#define twobit_op2_96( y ) /* bytevector-like-ref */ \
   do { word a=RESULT, b=reg( y ); \
        if (tagof( a ) == BVEC_TAG && \
	    is_fixnum(b) && \
	    ((b >> 2) < (the_header( a, BVEC_TAG ) >> 8))) \
	  RESULT = fixnum(*byte_addr( a, BVEC_TAG, BVEC_HEADER_WORDS, b )); \
        else { SECOND=b; SAVE_STATE(); \
	       mc_exception( globals, EX_BYTEVECTOR_LIKE_REF ); } \
   } while(0)

#define twobit_op3_97( y, z ) /* bytevector-like-set! */ \
   do { word a=RESULT, b=reg( y ), c=reg( z ); \
        if (tagof( a ) == BVEC_TAG && \
	    both_fixnums( b, c ) && \
	    ((b >> 2) < (the_header( a, BVEC_TAG ) >> 8)) && \
	    c < fixnum(256)) \
	  *byte_addr( a, BVEC_TAG, BVEC_HEADER_WORDS, b ) = (c >> 2); \
        else { SECOND=b; THIRD=c; SAVE_STATE(); \
               mc_exception( globals, EX_BYTEVECTOR_LIKE_SET ); } \
   } while(0)

#define twobit_op2_98( y ) /* sys$bvlcmp */ \
  SECOND = reg(y); WITH_SAVED_STATE( mc_bytevector_like_compare( globals ) )

#define twobit_op2_99( y ) /* vector-like-ref */ \
  do { word a=RESULT, b=reg(y); \
       if (tagof( a ) == VEC_TAG && \
           is_fixnum(b) && \
           (b < (the_header( a, VEC_TAG ) >> 8))) \
         RESULT = *word_addr( a, VEC_TAG, VEC_HEADER_WORDS, b ); \
       else { SECOND=b; SAVE_STATE(); \
	      mc_exception( globals, EX_VECTOR_LIKE_REF ); } \
  } while(0)

#define twobit_op3_100( y, z ) /* vector-like-set! */ \
  do { word a=RESULT, b=reg(y), c=reg(z); \
       if (tagof( a ) == VEC_TAG && \
           is_fixnum(b) && \
           (b < (the_header( a, VEC_TAG ) >> 8))) { \
         *word_addr( a, VEC_TAG, VEC_HEADER_WORDS, b ) = c; \
	 SECOND=c; \
	 WITH_SAVED_STATE( mc_full_barrier( globals ) ); \
       } else { SECOND=b; THIRD=c; SAVE_STATE(); \
		mc_exception( globals, EX_VECTOR_LIKE_SET ); \
       } \
  } while(0)

#define twobit_op1_101() /* vector-like-length */ \
   do { word a=RESULT; \
        if (tagof(a) == VEC_TAG) \
          RESULT = sizefield( the_header( a, VEC_TAG ) ); \
        else { SAVE_STATE(); \
	       mc_exception( globals, EX_VECTOR_LIKE_LENGTH ); } \
   } while(0)

#define twobit_op1_102() /* bytevector-like-length */ \
   do { word a=RESULT; \
        if (tagof(a) == BVEC_TAG) \
          RESULT = fixnum( sizefield( the_header( a, BVEC_TAG ) ) ); \
        else { SAVE_STATE(); \
	       mc_exception( globals, EX_BYTEVECTOR_LIKE_LENGTH ); } \
   } while(0)

#define twobit_op2_103( y, k ) /* remainder */ \
   implicit_label( SECOND = reg(y); mc_rem( globals, (cont_t)k ), k )

#define twobit_op1_104() /* petit-patch-boot-code */ \
   WITH_SAVED_STATE( mc_petit_patch_boot_code( globals ) )

#define twobit_op1_105( k ) /* syscall */ \
  implicit_label( mc_syscall( globals, (cont_t)k ), k )

#define twobit_op1_106() /* creg -- really 0 args */ \
  WITH_SAVED_STATE( mc_capture_continuation( globals ) )

#define twobit_op1_107() /* creg-set! */ \
  WITH_SAVED_STATE( mc_restore_continuation( globals ) )

#define twobit_op1_108() /* gc-counter */ \
  RESULT = globals[ G_GC_CNT ]

/* FIXME: wrong error code. */
#define twobit_op2_109( y ) /* make-string */ \
   do { word a=RESULT, b=reg(y), *p; \
        if (nonnegative_fixnum(a) && is_char(b)) { \
          word size = a >> 2; \
	  RESULT = a + fixnum(BVEC_HEADER_WORDS*sizeof( word )); \
	  WITH_SAVED_STATE( mc_alloc_bv( globals ) ); \
          p =(word*)RESULT; \
          *p = mkheader( size, STR_HDR ); \
          memset( (char*)p + sizeof(word), (b >> 16), size ); \
	  RESULT = tagptr( p, BVEC_TAG ); \
        } else { SAVE_STATE(); mc_exception( globals, EX_MKBVL ); } \
        integrity_check( "make-string" ); \
   } while(0)
  

#define twobit_op2imm_128( y ) /* typetag-set! */ \
  SECOND = y; WITH_SAVED_STATE( mc_typetag_set( globals ) )

#define twobit_op2imm_129( y ) /* eq? */ \
  setcc( RESULT == y )

#define twobit_op2imm_130( y, k ) /* + */ \
  twobit_add( RESULT, y, k )

#define twobit_op2imm_131( y, k ) /* -  */ \
  twobit_subtract( RESULT, y, k )

#define twobit_op2imm_132( y, k ) /* < */ \
  twobit_compare( RESULT, y, <, mc_lessp, k )

#define twobit_op2imm_133( y, k ) /* <= */ \
  twobit_compare( RESULT, y, <=, mc_less_or_equalp, k )

#define twobit_op2imm_134( y, k ) /* = */ \
  twobit_compare( RESULT, y, ==, mc_equalp, k )

#define twobit_op2imm_135( y, k ) /* > */ \
  twobit_compare( RESULT, y, >, mc_greaterp, k )

#define twobit_op2imm_136( y, k ) /* >= */ \
  twobit_compare( RESULT, y, >=, mc_greater_or_equalp, k )

#define charcmp_imm( op, y, ex ) /* generic character comparison */ \
  do { word a=RESULT, b=y; \
       if (is_char(a)) \
	 setcc( a op b ); \
       else { SECOND = b; SAVE_STATE(); mc_exception( globals, ex ); } \
  } while(0)

#define twobit_op2imm_137( y ) /* char<? */ \
  charcmp_imm( <, y, EX_CHARLT )

#define twobit_op2imm_138( y ) /* char<= */ \
  charcmp_imm( <=, y, EX_CHARLE )

#define twobit_op2imm_139( y ) /* char=? */ \
  charcmp_imm( ==, y, EX_CHAREQ )

#define twobit_op2imm_140( y ) /* char>? */ \
  charcmp_imm( >, y, EX_CHARGT )

#define twobit_op2imm_141( y ) /* char>=? */ \
  charcmp_imm( >=, y, EX_CHARGE )


#endif /* TWOBIT_H */

/* eof */

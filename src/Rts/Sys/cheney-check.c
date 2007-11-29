/* Copyright 1998 Lars T Hansen.              -*- indent-tabs-mode: nil -*-
 * 
 * $Id: cheney.c 5158 2007-11-26 20:55:58Z pnkfelix $
 *
 * Larceny -- support procedures for checking internal memory
 * invariants during a cheney collection.
 *
 * Entry points (from outside cheney-* files):
 *   gclib_check_memory_validity
 *   gclib_check_object
 *
 */


#define GC_INTERNAL

#ifdef UNIX
# include <sys/time.h>
#endif
#include <string.h>
#include "larceny.h"
#include "memmgr.h"
#include "gc_t.h"
#include "semispace_t.h"
#include "los_t.h"
#include "static_heap_t.h"
#include "gclib.h"
#include "barrier.h"
#include "stats.h"
#include "cheney.h"

void gclib_check_memory_validity( word *p, int n )
{
  int i;

  for ( i=0 ; i < n ; i++ ) {
    word x = p[i], y;

    if (ishdr( x )) {
      if (i != 0) {
        hardconsolemsg( "Header 0x%08x found at offset %d in object 0x%08x!",
                        x, i, (word)p );
        conditional_abort();
      }
      else if (sizefield( x ) > 4*1024*1024) {
        /* Bigger than the big array in gcbench! */
        hardconsolemsg( "Implausible but valid size %u in header "
                        "0x%08x in  object 0x%08x.", sizefield(x), x, (word)p);
      }
    }
    else {
      switch (tagof( x )) {
      case 0 : case 4 :         /* fixnum */
        break;
      case 6 :                  /* immediate */
        if ((x & 0xFF) == IMM_CHAR
            || x == TRUE_CONST || x == FALSE_CONST || x == NIL_CONST 
            || x == UNSPECIFIED_CONST || x == UNDEFINED_CONST
            || x == EOF_CONST)
          ;
        else {
          hardconsolemsg( "Invalid immediate 0x%08x found at offset %d"
                         " in object 0x%08x!", x, i, (word)p );
          conditional_abort();
        }
        break;
      case 1 :                  /* pair */
        y = *ptrof( x );
        if (y != FORWARD_HDR && ishdr( y )) {
          hardconsolemsg( "Pair pointer 0x%08x at offset %d in object 0x%08x"
                          " points to a header (0x%08x)!", x, i, (word)p, y );
          conditional_abort();
        }
        break;
      case 3 :                  /* vector */
        y = *ptrof( x );
        if (y != FORWARD_HDR && (!ishdr( y ) || header( y ) != VEC_HDR)) {
          hardconsolemsg( "Vector pointer 0x%08x at offset %d in object 0x%08x"
                          " does not point to a vector header (0x%08x)!", 
                          x, i, (word)p, y );
          conditional_abort();
        }
        break;
      case 5 :                  /* bytevector */
        y = *ptrof( x );
        if (y != FORWARD_HDR && (!ishdr( y ) || header( y ) != BV_HDR)) {
          hardconsolemsg( "Bytevector pointer 0x%08x at offset %d in object "
                          "0x%08x does not point to a bytevector header "
                          "(0x%08x)!",
                          x, i, (word)p, y );
          conditional_abort();
        }
        break;
      case 7 :                  /* procedure */
        y = *ptrof( x );
        if (y != FORWARD_HDR && 
            (!ishdr( y ) || header(y) != header(PROC_HDR))) {
          hardconsolemsg( "Procedure pointer 0x%08x at offset %d in object "
                          "0x%08x does not point to a procedure header "
                          "(0x%08x)!", 
                          x, i, (word)p, y );
          conditional_abort();
        }
        break;
      }
    }
  }
}

/* Obj must be an object pointer */
void gclib_check_object( word obj )
{
  word firstword;

  switch (tagof(obj)) {
  case PAIR_TAG :
    gclib_check_memory_validity( ptrof( obj ), 2 );
    break;
  case VEC_TAG :
    firstword = *ptrof( obj );
    if (!ishdr( firstword ) || header( firstword ) != VEC_HDR) {
      hardconsolemsg( "gclib_check_object: Inconsistent header: 0x%08x 0x%08x",
                      obj, firstword );
      conditional_abort();
    }
    gclib_check_memory_validity( ptrof( obj ), (sizefield( firstword ) + 4)/4);
    break;
  case PROC_TAG :
    firstword = *ptrof( obj );
    /* Procedure headers are weird (known bug, bit me big-time here) */
    if (!ishdr( firstword ) || header( firstword ) != header(PROC_HDR)) {
      hardconsolemsg( "gclib_check_object: Inconsistent header: 0x%08x 0x%08x",
                      obj, firstword );
      conditional_abort();
    }
    gclib_check_memory_validity( ptrof( obj ), (sizefield( firstword ) + 4)/4);
    break;
  case BVEC_TAG :
    firstword = *ptrof( obj );
    if (!ishdr( firstword ) || header( firstword ) != BV_HDR) {
      hardconsolemsg( "gclib_check_object: Inconsistent header: 0x%08x 0x%08x",
                      obj, firstword );
      conditional_abort();
    }
    break;
  default :
    hardconsolemsg( "gclib_check_object: Not a pointer as expected: 0x%08x",
                    obj );
    conditional_abort();
    }
}

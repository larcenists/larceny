/* Rts/Standard-C/multiply.c
 * Petit Larceny -- portable extended-precision multiplication code.
 *
 * $Id: multiply.c,v 1.1.1.1 1998/11/19 21:51:52 lth Exp $ 
 */

#include "larceny-types.h"
#include "millicode.h"

static void neg64( word *, word * );

/* Multiply two 32-bit signed numbers and produce a 64-bit signed 
 * result. 
 */
void mul_32x32_to_64( word a, word b, word *hi, word *lo )
{
  if ((s_word)a < 0) {
    if ((s_word)b < 0)
      umul_32x32_to_64( (word)-(s_word)a, (word)-(s_word)b, hi, lo );
    else {
      umul_32x32_to_64( (word)-(s_word)a, b, hi, lo );
      neg64( hi, lo );
    }
  }
  else if ((s_word)b < 0) {
    umul_32x32_to_64( a, (word)-(s_word)b, hi, lo );
    neg64( hi, lo );
  }
  else 
    umul_32x32_to_64( a, b, hi, lo );
}

/* Multiply two 32-bit unsigned numbers and produce a 64-bit unsigned
 * result.
 */
void umul_32x32_to_64( word a, word b, word *hi, word *lo )
{
  word a1, a2, b1, b2, r1, r2, r3, r4, carry, x, y;

  a1 = a & 0xFFFF;
  a2 = a >> 16;
  b1 = b & 0xFFFF;
  b2 = b >> 16;

  /* a*b = (a2*e+a1) * (b2*e+b1) = (a2*b2*e^2 + a1*b2*e + a2*b1*e + a1*b1) */

  r1 = a1 * b1;
  r2 = a1 * b2;
  r3 = a2 * b1;
  r4 = a2 * b2;

  carry = 0;
  x = r1 + (r2 << 16);
  if (x < r1) carry++;
  y = x + (r3 << 16);
  if (y < x) carry++;      
  *lo = y;
  *hi = r4 + (r2 >> 16) + (r3 >> 16) + carry;
}

static void neg64( word *hi, word *lo )
{
  word carry = 0, y;

  y = ~*lo + 1;
  if (y == 0) carry = 1;
  *lo = y;
  *hi = ~*hi + carry;
}

#if defined( MULTEST )
struct test {
  word a;
  word b;
  word hi;
  word lo;
} tests[] = {{ 0,0,0,0 }, { 100,100,0,10000 }, { 65536, 65536, 1, 0 },
	     { 65535, 65535, 0, 4294836225U }, { 65535, 65538, 1, 65534 },
             { 288230375, 10, 0, 2882303750U },
             { 10, 288230375, 0, 2882303750U },
	    };

main()
{
  int i;
  word hi, lo;

  for ( i=0 ; i < sizeof(tests) / sizeof( struct test ) ; i++ ) {
    umul_32x32_to_64( tests[i].a, tests[i].b, &hi, &lo );
    if (hi != tests[i].hi || lo != tests[i].lo)
      printf( "Error in test %d: result is (%u,%u)\n", i, hi, lo );
  }
}
#endif  /* if defined( MULTEST ) */

/* eof */

/* Copyright 1998 Lars T Hansen
 *
 * $Id$
 *
 * Foreign functions for ffi-test test suite. 
 *
 * Compiling this file:
 *   sunos4 and sunos5:
 *     gcc -fPIC -shared ffi-test-ff.c -o ffi-test-ff.so
 */

#define PRINT  0    /* set to 1 to print a lot of debugging info */

/* C is great. */
#if PRINT
# include <stdio.h>
# define print printf
#else
# define print (void)
#endif

void ffitest1( void )
{
  print( "ffitest1\n" );
}

int ffitest2( int a )
{
  print( "ffitest2 a=%d\n", a );
  return a;
}

int ffitest3( int a, int b )
{
  print( "ffitest3 a=%d b=%d\n", a, b );
  return a+b;
}

/* SPARC: 6 register arguments */
double ffitest4( int a, int b, int c, int d, int e, int f )
{
  print( "ffitest4 a=%d b=%d c=%d d=%d e=%d f=%d\n", a, b, c, d, e, f );
  return (double)(a+b+c+d+e+f);
}

float ffitest5( int a, int b, int c, int d, int e, int f, int g, int h )
{
  print( "ffitest5 a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", 
	 a, b, c, d, e, f, g, h );
  return (float)(a+b+c+d+e+f+g+h);
}

double ffitest6( double a )
{
  print( "ffitest6 a=%f\n", a );
  return a;
}

double ffitest7( double a, double b )
{
  print( "ffitest7 a=%f b=%f\n", a, b );
  return a+b;
}

/* SPARC: This forces c to be split over an odd/even register pair */
float ffitest8( double a, int b, double c )
{
  print( "ffitest8 a=%f b=%d c=%f\n", a, b, c );
  return (float)c;
}

/* SPARC: This forces d to be split over registers and memory */
double ffitest9( double a, double b, int c, double d, int e )
{
  print( "ffitest9 a=%f b=%f c=%d d=%f e=%d\n", a, b, c, d, e );
  return a+b+d;
}

/* SPARC: gcc expects an ieee32 here */
float ffitest10( float a )
{
  print( "ffitest10 a=%f\n", (double)a );
  return a;
}

float ffitest11( float a, float b )
{
  print( "ffitest11 a=%f b=%f\n", (double)a, (double)b );
  return a+b;
}

void fficb1( void (*f)(void) )
{
  print( "fficb1 in\n" );
  f();
  print( "fficb1 out\n" );
}

int fficb2( int (*f)(void) )
{
  int r;

  print( "fficb2 in\n" );
  r = f();
  print( "fficb2 out\n" );
  return r;
}

int fficb3( int x, int (*f)(int) )
{
  int r;

  print( "fficb2 in\n" );
  r = f(x);
  print( "fficb2 out\n" );
  return r;
}

/* eof */

/* Copyright 1999 Lars T Hansen
 *
 * $Id$
 *
 * Foreign functions for std-ffi-test test suite. 
 *
 * Compiling this file:
 *   sunos4 and sunos5:
 *     gcc -fPIC -shared std-ffi-test-ff.c -o std-ffi-test-ff.so
 */

/* Null pointer tests */

int valid_null_pointer_p( void *p ) { return p == 0; }
void *return_null_pointer( void ) { return (void*)0; }

/* Integer tests */

int add_ints( int a, int b ) { return a+b; }
short add_shorts( short a, short b ) { return a+b; }
long add_longs( long a, long b ) { return a+b; }
unsigned add_uints( unsigned a, unsigned b ) { return a+b; }
unsigned short add_ushorts( unsigned short a, unsigned short b ) {return a+b;}
unsigned long add_ulongs( unsigned long a, unsigned long b ) { return a+b; }

/* Char tests */

#include <ctype.h>

char cmp_chars( char a, char b) 
{ 
  char yes, no;
  if (isupper(a) && isupper(b)) {
    yes = 'Y';
    no = 'N';
  }
  else {
    yes = 'y';
    no = 'n';
  }
  if (a == b) return yes; else return no;
}

unsigned char cmp_uchars( unsigned char a, unsigned char b) 
{ 
  unsigned char yes, no;
  if (islower(a) || islower(b)) {
    yes = 'Y';
    no = 'N';
  }
  else {
    yes = 'y';
    no = 'n';
  }
  if (a == b) return yes; else return no;
}

/* Bool tests */

int pass_bool2int( int i ) { return i; }
int pass_bool2bool( int i ) { return i; }

/* Floating tests */

double add_doubles( double a, double b ) { return a+b; }
float add_floats( float a, float b ) { return a+b; }

/* String and boxed tests */

char *return_half( char *s )
{
  return s + strlen( s )/2;
}

void fill_bytevector( void *p, int n )
{
  unsigned char *q = (unsigned char *)p;
  int i;

  for ( i=0 ; i < n ; i++ )
    q[i] = i;
}

char *pass_null_pointer( char *s )
{
  return s;
}

int pass_null_pointer_to_boxed( void *p )
{
  return p == 0;
}

/* Void return test */

void void_return( void *p ) 
{ 
  *(unsigned char*)p = 1;
}

/* eof */

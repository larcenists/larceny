/* FIBFP -- Computes fib(25) using floating point */

#include <stdio.h>

#define FLOAT double

static FLOAT fib (n)
FLOAT n;
{
  if (n < 2.)
    return n;
  else
    return fib (n-1.) + fib (n-2.);
}

int main (argc, argv)
int argc;
char *argv[];
{
  int i;
  FLOAT result;

  for (i=0; i<50; i++)
    result = fib (25.);

  if (result != 75025.)
    printf ("*** wrong result ***\n");

  return 0;
}

/* FIBFP -- Computes fib(30) using floating point */

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
    result = fib (30.);

  if (result != 832040.)
    printf ("*** wrong result ***\n");

  return 0;
}

/* FIB -- A classic benchmark, computes fib(25) inefficiently. */

#include <stdio.h>

static int fib (n)
int n;
{
  if (n < 2)
    return n;
  else
    return fib (n-1) + fib (n-2);
}

int main (argc, argv)
int argc;
char *argv[];
{
  int i;
  int result;

  for (i=0; i<50; i++)
    result = fib (25);

  if (result != 75025)
    printf ("*** wrong result ***\n");

  return 0;
}

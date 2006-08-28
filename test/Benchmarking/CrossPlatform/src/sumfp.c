/* SUMFP -- Compute sum of integers from 0 to 10000 using floating point */

#include <stdio.h>

#define FLOAT double

static FLOAT run ()
{
  FLOAT i = 10000., n = 0.;

  while (i >= 0.)
    {
      n = n+i;
      i = i-1.;
    }

  return n;
}

int main (argc, argv)
int argc;
char *argv[];
{
  int i;
  FLOAT result;

  for (i=0; i<10000; i++)
    result = run ();

  if (result != 50005000.)
    printf ("*** wrong result ***\n");

  return 0;
}

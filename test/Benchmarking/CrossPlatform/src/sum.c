/* SUM -- Compute sum of integers from 0 to 10000 */

#include <stdio.h>

static int run ()
{
  int i = 10000, n = 0;

  while (i >= 0)
    {
      n = n+i;
      i = i-1;
    }

  return n;
}

int main (argc, argv)
int argc;
char *argv[];
{
  int i;
  int result;

  for (i=0; i<10000; i++)
    result = run ();

  if (result != 50005000)
    printf ("*** wrong result ***\n");

  return 0;
}

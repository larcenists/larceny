/* TAK -- A vanilla version of the TAKeuchi function. */

#include <stdio.h>

static int tak (x, y, z)
int x, y, z;
{
  if (y >= x)
    return z;
  else
    return tak (tak (x-1, y, z),
                tak (y-1, z, x),
                tak (z-1, x, y));
}

int main (argc, argv)
int argc;
char *argv[];
{
  int i;
  int result;

  for (i=0; i<1000; i++)
    result = tak (18, 12, 6);

  if (result != 7)
    printf ("*** wrong result ***\n");

  return 0;
}

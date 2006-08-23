/* MBROT -- Generation of Mandelbrot set fractal. */

#include <stdio.h>

#define FLOAT double

#define N 75

#define max_count 64
#define radius2 16.0

static int count (r, i, step, x, y)
FLOAT r, i, step;
int x, y;
{
  FLOAT cr = r + x*step;
  FLOAT ci = i + y*step;
  FLOAT zr = cr;
  FLOAT zi = ci;
  int c = 0;

  while (c != max_count)
    {
      FLOAT zr2 = zr*zr;
      FLOAT zi2 = zi*zi;

      if (zr2+zi2 > radius2)
        return c;

      zi = 2.0*zr*zi + ci;
      zr = (zr2-zi2) + cr;

      c++;
    }

  return c;
}

static void mbrot (matrix, r, i, step, n)
int *matrix;
FLOAT r, i, step;
int n;
{
  int x, y;

  for (y=n-1; y>=0; y--)
    for (x=n-1; x>=0; x--)
      matrix[y*n+x] = count (r, i, step, x, y);
}

static int test ()
{
  int matrix[N*N];

  mbrot (matrix, -1.0, -0.5, 0.005, N);

  return matrix[0];
}

int main (argc, argv)
int argc;
char *argv[];
{
  int i;
  int result;

  for (i=0; i<30; i++)
    result = test ();

  if (result != 5)
    printf ("*** wrong result ***\n");

  return 0;
}

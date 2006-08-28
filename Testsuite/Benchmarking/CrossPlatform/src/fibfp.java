/* FIBFP -- Computes fib(30) using floating point */

class fibfp {

    static double fib (double n)
    {
      if (n < 2.0)
        return n;
      else
        return fib (n-1.0) + fib (n-2.0);
    }

    public static void main (String args[])
    {
      int i;
      double result = 0.0;

      for (i=0; i<50; i++)
        result = fib (30.0);

      if (result != 832040.0)
        System.out.println ("*** wrong result ***");
    }

}

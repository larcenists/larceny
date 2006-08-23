/* FIB -- A classic benchmark, computes fib(25) inefficiently. */

class fib {

    static int fib (int n)
    {
      if (n < 2)
        return n;
      else
        return fib (n-1) + fib (n-2);
    }

    public static void main (String args[])
    {
      int i;
      int result = 0;

      for (i=0; i<50; i++)
        result = fib (25);

      if (result != 75025)
        System.out.println ("*** wrong result ***");
    }

}

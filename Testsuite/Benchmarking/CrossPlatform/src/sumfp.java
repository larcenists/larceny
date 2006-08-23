/* SUMFP -- Compute sum of integers from 0 to 10000 using floating point */

class sumfp {

    static double run ()
    {
      double i = 10000.0;
      double n = 0.0;

      while (i >= 0.0)
        {
          n = n+i;
          i = i-1.0;
        }

      return n;
    }

    public static void main (String args[])
    {
      int i;
      double result = 0.0;

      for (i=0; i<10000; i++)
        result = run ();

      if (result != 50005000.)
        System.out.println ("*** wrong result ***");
    }
}


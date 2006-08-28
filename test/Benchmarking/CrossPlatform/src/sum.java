/* SUM -- Compute sum of integers from 0 to 10000 */

class sum {

    static int run ()
    {
      int i = 10000;
      int n = 0;

      while (i >= 0)
        {
          n = n+i;
          i = i-1;
        }

      return n;
    }

    public static void main (String args[])
    {
      int i;
      int result = 0;

      for (i=0; i<10000; i++)
        result = run ();

      if (result != 50005000)
        System.out.println ("*** wrong result ***");
    }
}


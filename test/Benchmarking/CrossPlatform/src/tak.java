/* TAK -- A vanilla version of the TAKeuchi function. */

class tak {

    static int tak (int x, int y, int z)
    {
      if (y >= x)
        return z;
      else
        return tak (tak (x-1, y, z),
                    tak (y-1, z, x),
                    tak (z-1, x, y));
    }

    public static void main (String args[])
    {
      int i;
      int result = 0;

      for (i=0; i<1000; i++)
        result = tak (18, 12, 6);

      if (result != 7)
        System.out.println ("*** wrong result ***");
    }
}


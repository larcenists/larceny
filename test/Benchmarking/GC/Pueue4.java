// Queue-like object lifetimes with popular objects
//
// Usage:
//
// java Pueue4 m n k p
//
// allocates m lists of length n, storing them in a
// circular buffer that holds k of the lists.  The
// list elements are drawn from p popular objects.
//
// defaults (if omitted):
//     m = 1000
//     n = 1000000
//     k = 10
//     p = 10

public class Pueue4 {

    static final int mDEFAULT = 1000;
    static final int nDEFAULT = 1000000;
    static final int kDEFAULT = 10;
    static final int pDEFAULT = 10;

    public static void main (String[] args) {
        int m = mDEFAULT;
        int n = nDEFAULT;
        int k = kDEFAULT;
        int p = pDEFAULT;
        if (args.length > 0)
            m = Integer.parseInt (args[0], 10);
        if (args.length > 1)
            n = Integer.parseInt (args[1], 10);
        if (args.length > 2)
            k = Integer.parseInt (args[2], 10);
        if (args.length > 3)
            p = Integer.parseInt (args[3], 10);
        System.out.println ("m = " + m);
        System.out.println ("n = " + n);
        System.out.println ("k = " + k);
        System.out.println ("p = " + p);
        Pueue4 q = new Pueue4(m, n, k, p);
        timeBenchmark(q, 1);
    }

    static void timeBenchmark (Pueue4 q, int iterations) {
        long tStart, tFinish;
        int result = 0;
        tStart = System.currentTimeMillis();
        for (int i = 0; i < iterations; ++i) {
            result = q.run();
        }
        tFinish = System.currentTimeMillis();
        long elapsedTime = tFinish - tStart;
        System.out.println(result);
        System.out.println(elapsedTime + " msec");
        long maxPause = q.maxTime - (elapsedTime / (iterations * q.m));
        System.out.println("Max pause time: " + maxPause);
        memoryReport();
    }

    static void memoryReport () {
        long lFreeMemory = Runtime.getRuntime().freeMemory();
        long lTotalMemory = Runtime.getRuntime().totalMemory();

        System.out.print(" Total memory available="
                         + lTotalMemory + " bytes");
        System.out.println("  Free memory=" + lFreeMemory + " bytes");
    }

    // the benchmark itself

    private int m;        // number of lists to allocate
    private int n;        // length of each list
    private int k;        // capacity of circular buffer
    private static int p;        // number of popular objects

    private static Integer[] popular; // array of popular objects

    Pueue4 (int m, int n, int k, int p) {
        this.m = m;
        this.n = n;
        this.k = k;
        Pueue4.p = p;
        Pueue4.popular = new Integer[p];
        for (int i = 0; i < p; i = i + 1)
            Pueue4.popular[i] = new Integer(i);
    }

    // Estimation of maximum pause time.
    // To be computed as the maximum time to create a list
    // minus the average tme to create a list.

    long maxTime = 0;     // maximum time to create a list, in msec

    // Note that this version fixes a bug in Pueue3.java:
    // This version really does create p popular objects,
    // whereas Pueue3 created only 1 extremely popular object.

    int run () {
        IntList[] buffer = new IntList[k];
        int m = this.m;
        int i = 0;
        while (m > 0) {
            long tStart = System.currentTimeMillis();
            buffer[i] = IntList.makeList(this.n, popular[m % p]);  // see note
            long tFinish = System.currentTimeMillis();
            long t = tFinish - tStart;
            if (t > maxTime)
                maxTime = t;
            m = m - 1;
            i = (i + 1) % k;
        }
        return buffer[i].size();
    }

    // an analogue of Scheme pairs
    // but each IntList is probably twice as large as a Scheme pair

    private static class IntList {

        private Integer first;        // the car
        private IntList rest;         // the cdr

        private IntList (Integer first, IntList rest) {
            this.first = first;
            this.rest = rest;
        }

        public static IntList makeList (int n, Integer obj) {
            IntList result = null;
            while (n > 0) {
                result = new IntList(obj, result);
                n = n - 1;
            }
            return result;
        }

        public int size () {
            IntList x = this;
            int result = 0;
            while (x != null) {
                x = x.rest;
                result = result + 1;
            }
            return result;
        }
    }
}

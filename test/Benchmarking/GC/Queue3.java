// Queue-like object lifetimes.
//
// Usage:
//
// java Queue3 m n k
//
// allocates m lists of length n, storing them in a
// circular buffer that holds k of the lists.
//
// defaults (if omitted):
//     m = 1000
//     n = 1000000
//     k = 10

public class Queue3 {

    static final int mDEFAULT = 1000;
    static final int nDEFAULT = 1000000;
    static final int kDEFAULT = 10;

    public static void main (String[] args) {
        int m = mDEFAULT;
        int n = nDEFAULT;
        int k = kDEFAULT;
        if (args.length > 0)
            m = Integer.parseInt (args[0], 10);
        if (args.length > 1)
            n = Integer.parseInt (args[1], 10);
        if (args.length > 2)
            k = Integer.parseInt (args[2], 10);
        System.out.println ("m = " + m);
        System.out.println ("n = " + n);
        System.out.println ("k = " + k);
	Queue3 q = new Queue3(m, n, k);
	timeBenchmark(q, 1);
    }

    static void timeBenchmark (Queue3 q, int iterations) {
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

    Queue3 (int m, int n, int k) {
	this.m = m;
	this.n = n;
	this.k = k;
    }

    // Estimation of maximum pause time.
    // To be computed as the maximum time to create a list
    // minus the average tme to create a list.

    long maxTime = 0;     // maximum time to create a list, in msec

    int run () {
	IntList[] buffer = new IntList[k];
	int m = this.m;
	int i = 0;
	while (m > 0) {
	    long tStart = System.currentTimeMillis();
	    buffer[i] = IntList.makeList(this.n);
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

	private int     first;        // the car
	private IntList rest;         // the cdr

	private IntList (int first, IntList rest) {
	    this.first = first;
	    this.rest = rest;
	}

	public static IntList makeList (int n) {
	    IntList result = null;
	    while (n > 0) {
		result = new IntList(n, result);
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

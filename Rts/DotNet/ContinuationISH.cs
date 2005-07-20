using System;
using System.Collections;
using System.Diagnostics;

using Scheme.Rep;

namespace Scheme.RT {

    /* The stack cache is conceptually implemented as a circular doubly-linked list
     * SENTINEL <- ROOT:frame <-> frame <-> ... <-> cont:frame <-> ... <-> LIMIT:frame -> SENTINEL
     *
     * cont can take the value of any frame in the chain, including ROOT and LIMIT but not null
     * Invariant: all frames strictly above cont are completely empty (null slots?)
     * Invariant: while Scheme code is running, there is at least one full frame on the stack
     *   (initially, the frame with codevector InitialContinuation)
     *
     * The heap contains either false or a vector containing a frame with a parent
     * link... the format is
     * [0: return address (fixnum),
     *  1: dynamic link (another heap frame / #f)
     *  2: slot 0 = Register0 = Procedure, Has codevector of return address
     *  3: slot 1
     *  ...]
     * 
     * The initial continuation will have a parent continuation of #f when a heap frame.
     */

public sealed class Cont {
#if HAS_PERFORMANCE_COUNTERS
  public static PerformanceCounter stackFlushCounter;
  public static PerformanceCounter stackReloadCounter;
#endif

  // How many individual slot fields before we use
  // the overflow array
  // named slot0, slot1, ..., slot{NUM_SLOT_FIELDS-1}

  // If you change this, you must also change the definition of
  // the ContinuationFrame class, the toVector method, the getSlot method,
  // the setSlot method, and the saveRegisters method. (all in this file)
  public const int NUM_SLOT_FIELDS = 8;

  // How many frames to keep in the cache.
  public const int NUM_CACHE_FRAMES = 100; //40; // 10;

  // How many frames to restore on fillCache (max)
  public const int MAX_RESTORE_FRAMES = 25;

  // Heap frames : described later in file
  public static SObject heap = Factory.False;

  // Offsets within HEAP FRAME of return address, dynamic link, return procedure, and first slot (same)
  // Adjusted down by one from C version.
  public const int HC_RETOFFSET = Constants.HC_RETOFFSET - 1;
  public const int HC_DYNLINK = Constants.HC_DYNLINK - 1;
  public const int HC_PROC = Constants.HC_PROC - 1;
  public const int HC_OVERHEAD = Constants.HC_OVERHEAD - 1;

  public const int HC_CONTEXT_REG0 = Cont.HC_PROC + 1;
  public const int HC_CONTEXT_RESULT = Cont.HC_CONTEXT_REG0 + Reg.NREGS;
  public const int HC_CONTEXT_RESTORE_RESULT = Cont.HC_CONTEXT_RESULT + 1;

  // Reference to current top of stack.
  public static StackCacheFrame cont;
  static StackCacheFrame ROOT;
  static StackCacheFrame LIMIT;
  static readonly StackCacheFrame SENTINEL = new StackCacheFrame();

  // static constructor initializes stack cache structure
  // static constructors are slow.  Make an explicit initialization method.
  public static void Initialize ()
  {
    StackCacheFrame limit = SENTINEL;
    for (int i = 0; i < NUM_CACHE_FRAMES; ++i) {
	StackCacheFrame newframe = new StackCacheFrame();
	newframe.before = limit;
	limit.after = newframe;
	limit = newframe;
	}
    limit.after = SENTINEL;
    SENTINEL.before = limit;
    ROOT = SENTINEL.after;
    LIMIT = SENTINEL.before;
    cont = ROOT;
    clear();
    // Install the initial continuation.
    reset();
  }

  // resetCache
  // Clear out any existing continuation and install the initial one.
  public static void reset()
  {
    heap = Factory.False;
    cont = ROOT;
    cont.prepare0();
    cont.Slot0 = InitialContinuation.singletonProcedure;
    cont.returnIndex = 0;
  }

  // clear
  // Clear all frames in the cache (so GC doesn't hold dead objects)
  // Called on every timer interrupt (see Exn.faultTimer).
  public static void clear()
  {
    StackCacheFrame frame = ROOT;
    while (frame != SENTINEL) {
	frame.clear();
	frame = frame.after;
	}
  }

  /* save
   */
  public static void save (int lastslot)
  {
    if (cont == LIMIT) {
	heap = copyOutStack();
	cont = SENTINEL;
	}
    cont = cont.after;
    cont.prepare (lastslot);
    cont.Slot0 = Reg.Register0;
  }

  public static void save0 ()
  {
    if (cont == LIMIT) {
	heap = copyOutStack();
	cont = SENTINEL;
	}
    cont = cont.after;
    cont.prepare0 ();
    cont.Slot0 = Reg.Register0;
  }

  // save that assumes that lastslot is < num slots.
  public static void save_small (int lastslot)
  {
    if (cont == LIMIT) {
	heap = copyOutStack();
	cont = SENTINEL;
	}
    cont = cont.after;
    cont.prepare_small (lastslot);
    cont.Slot0 = Reg.Register0;
  }

  /* pop
   */
  public static void pop()
  {
    cont.lastslot = 0;
    cont = cont.before;
    if (cont == SENTINEL) {
	fillCache();
	}
  }

  /* copyOutStack
   * Flush all frames out of the cache to the heap. Does not change stack cache.
   */
  public static SObject copyOutStack()
  {
#if HAS_PERFORMANCE_COUNTERS
    if (stackFlushCounter != null) stackFlushCounter.Increment();
#endif
    SObject h = heap;
    for (StackCacheFrame f = ROOT; f != cont.after; f = f.after) {
	h = f.toVector (h);
	}
    return h;
  }

  /* fillCache
   */
  public static void fillCache()
  {
#if HAS_PERFORMANCE_COUNTERS
    if (stackReloadCounter != null) stackReloadCounter.Increment();
#endif
    cont = ROOT;
    SVL h = heap as SVL;

    if (h == null) {
	Exn.internalError ("fillCache: Cont.heap is not a vector");
	}
    else {
	heap = h.elements[Cont.HC_DYNLINK];
	cont.fillFromVector (h);
	}
  }

  //        /* fillCache
  //         * Precondition: cont == ROOT.before == SENTINEL; heap != #f
  //         * Postcondition: cont points at a valid stack frame
  //         */
  //        public static void fillCache() {
  //            StackCacheFrame f = cont.after;
  //            int framesToCopy = MAX_RESTORE_FRAMES;
  //            // Invariant: frames f through cont inclusive are valid frames
  //            while (framesToCopy > 0 & heap != Factory.False) {
  //                SVL h = (SVL) heap;
  //                f = f.before;
  //                heap = h.elements[Cont.HC_DYNLINK];
  //                f.fillFromVector (h);
  //                --framesToCopy;
  //                // note: f is a valid frame at this point (by invariant)
  //            }
  //            // f is now the frame closest to the initial continuation copied
  //            SENTINEL = f.before;
  //            ROOT = SENTINEL.after;
  //            LIMIT = SENTINEL.before;
  //            // cont now points to the last of a set of valid frames
  //            // by invariant and fact that fillCache should never be called
  //            // when heap = #f
  //        }

  /* getCC
   * Return the reified current continuation
   */
  public static SObject getCC()
  {
    return copyOutStack();
  }

  /* setCC
   * Install arg as current continuation
   */
  public static void setCC (SObject k)
  {
    heap = k;
    fillCache();
  }
}

public class ContinuationFrame
{
  // If you modify the number of slot fields, remember
  // to change the constant NUM_SLOT_FIELDS above.
  public Procedure s0;
  public SObject   s1;
  public SObject   s2;
  public SObject   s3;
  public SObject   s4;
  public SObject   s5;
  public SObject   s6;
  public SObject   s7;

  public Procedure Slot0 { get { return this.s0; } set { this.s0 = value; } }
  public SObject Slot1 { get { return this.s1; } set { this.s1 = value; } }
  public SObject Slot2 { get { return this.s2; } set { this.s2 = value; } }
  public SObject Slot3 { get { return this.s3; } set { this.s3 = value; } }
  public SObject Slot4 { get { return this.s4; } set { this.s4 = value; } }
  public SObject Slot5 { get { return this.s5; } set { this.s5 = value; } }
  public SObject Slot6 { get { return this.s6; } set { this.s6 = value; } }
  public SObject Slot7 { get { return this.s7; } set { this.s7 = value; } }

  public SObject[] overflowSlots;

  public int returnIndex;

  public int lastslot;
  // indicates that the overflowSlots may contain data beyond the lastslot.
  public bool dirty;

  public ContinuationFrame()
  {
    this.lastslot = 0;
    this.dirty = false;
    this.overflowSlots = new SObject[] {};
    this.clear();
  }

  private int capacity()
  {
    return Cont.NUM_SLOT_FIELDS + this.overflowSlots.Length;
  }

  // this is called frequently enough to warrant special treatment
  // we unroll the fill loop in the small cases, and set things up to
  // use the built-in array copy in the big cases.

  public SObject toVector (SObject parent)
  {
    SObject[] elements = new SObject[this.lastslot + Cont.HC_OVERHEAD + 1];
    elements[Cont.HC_RETOFFSET] = Factory.makeNumber (this.returnIndex);
    elements[Cont.HC_DYNLINK] = parent;
    switch (this.lastslot) {
      case 0:
	elements [Cont.HC_OVERHEAD + 0] = s0;
	break;

      case 1:
	elements [Cont.HC_OVERHEAD + 0] = s0;
	elements [Cont.HC_OVERHEAD + 1] = s1;
	break;

      case 2:
	elements [Cont.HC_OVERHEAD + 0] = s0;
	elements [Cont.HC_OVERHEAD + 1] = s1;
	elements [Cont.HC_OVERHEAD + 2] = s2;
	break;

      case 3:
	elements [Cont.HC_OVERHEAD + 0] = s0;
	elements [Cont.HC_OVERHEAD + 1] = s1;
	elements [Cont.HC_OVERHEAD + 2] = s2;
	elements [Cont.HC_OVERHEAD + 3] = s3;
	break;

      case 4:
	elements [Cont.HC_OVERHEAD + 0] = s0;
	elements [Cont.HC_OVERHEAD + 1] = s1;
	elements [Cont.HC_OVERHEAD + 2] = s2;
	elements [Cont.HC_OVERHEAD + 3] = s3;
	elements [Cont.HC_OVERHEAD + 4] = s4;
	break;

      case 5:
	elements [Cont.HC_OVERHEAD + 0] = s0;
	elements [Cont.HC_OVERHEAD + 1] = s1;
	elements [Cont.HC_OVERHEAD + 2] = s2;
	elements [Cont.HC_OVERHEAD + 3] = s3;
	elements [Cont.HC_OVERHEAD + 4] = s4;
	elements [Cont.HC_OVERHEAD + 5] = s5;
	break;

      case 6:
	elements [Cont.HC_OVERHEAD + 0] = s0;
	elements [Cont.HC_OVERHEAD + 1] = s1;
	elements [Cont.HC_OVERHEAD + 2] = s2;
	elements [Cont.HC_OVERHEAD + 3] = s3;
	elements [Cont.HC_OVERHEAD + 4] = s4;
	elements [Cont.HC_OVERHEAD + 5] = s5;
	elements [Cont.HC_OVERHEAD + 6] = s6;
	break;

      case 7:
	elements [Cont.HC_OVERHEAD + 0] = s0;
	elements [Cont.HC_OVERHEAD + 1] = s1;
	elements [Cont.HC_OVERHEAD + 2] = s2;
	elements [Cont.HC_OVERHEAD + 3] = s3;
	elements [Cont.HC_OVERHEAD + 4] = s4;
	elements [Cont.HC_OVERHEAD + 5] = s5;
	elements [Cont.HC_OVERHEAD + 6] = s6;
	elements [Cont.HC_OVERHEAD + 7] = s7;
	break;

      default:
	elements [Cont.HC_OVERHEAD + 0] = s0;
	elements [Cont.HC_OVERHEAD + 1] = s1;
	elements [Cont.HC_OVERHEAD + 2] = s2;
	elements [Cont.HC_OVERHEAD + 3] = s3;
	elements [Cont.HC_OVERHEAD + 4] = s4;
	elements [Cont.HC_OVERHEAD + 5] = s5;
	elements [Cont.HC_OVERHEAD + 6] = s6;
	elements [Cont.HC_OVERHEAD + 7] = s7;
	// Copy the overflow slots
	System.Array.Copy (overflowSlots, 0, elements, Cont.HC_OVERHEAD + 8, this.lastslot - 7);
	break;
	}
    return Factory.makeVector (elements);
  }

  public void fillFromVector (SVL hframe)
  {
    SObject[] elements = hframe.elements;

    this.prepare (elements.Length - Cont.HC_OVERHEAD - 1);
    this.returnIndex = ((SFixnum)elements[Cont.HC_RETOFFSET]).value;
    for (int si = 0; si <= this.lastslot; ++si) {
	this.setSlot (si, elements[si + Cont.HC_OVERHEAD]);
	}
  }

  public SObject getSlot (int slot)
  {
    switch (slot) {
      case 0: return s0;
      case 1: return s1;
      case 2: return s2;
      case 3: return s3;
      case 4: return s4;
      case 5: return s5;
      case 6: return s6;
      case 7: return s7;
      default:
	return overflowSlots[slot - Cont.NUM_SLOT_FIELDS];
	}
  }

  public void setSlot (int slot, SObject value)
  {
    switch (slot) {
      case 0: s0 = (Procedure) value; return;
      case 1: s1 = value; return;
      case 2: s2 = value; return;
      case 3: s3 = value; return;
      case 4: s4 = value; return;
      case 5: s5 = value; return;
      case 6: s6 = value; return;
      case 7: s7 = value; return;
      default:
	overflowSlots[slot - Cont.NUM_SLOT_FIELDS] = value;
	return;
	}
  }

  // Saving all the registers into the frame is done when we save the
  // context.  This is done so frequently that we can get a substantial
  // improvement by unrolling the entire thing.
  // Unfortunately, this crosses the abstraction barrier around the registers.

  public void saveRegisters (Procedure restore, bool full)
  {
    s0 = restore;
    returnIndex = Reg.implicitContinuation;
    s1 = Reg.Register0;
    s2 = Reg.Register1;
    s3 = Reg.Register2;
    s4 = Reg.Register3;
    s5 = Reg.Register4;
    s6 = Reg.Register5;
    s7 = Reg.Register6;
    overflowSlots  [0] = Reg.Register7;
    overflowSlots  [1] = Reg.Register8;
    overflowSlots  [2] = Reg.Register9;
    overflowSlots  [3] = Reg.Register10;
    overflowSlots  [4] = Reg.Register11;
    overflowSlots  [5] = Reg.Register12;
    overflowSlots  [6] = Reg.Register13;
    overflowSlots  [7] = Reg.Register14;
    overflowSlots  [8] = Reg.Register15;
    overflowSlots  [9] = Reg.Register16;
    overflowSlots [10] = Reg.Register17;
    overflowSlots [11] = Reg.Register18;
    overflowSlots [12] = Reg.Register19;
    overflowSlots [13] = Reg.Register20;
    overflowSlots [14] = Reg.Register21;
    overflowSlots [15] = Reg.Register22;
    overflowSlots [16] = Reg.Register23;
    overflowSlots [17] = Reg.Register24;
    overflowSlots [18] = Reg.Register25;
    overflowSlots [19] = Reg.Register26;
    overflowSlots [20] = Reg.Register27;
    overflowSlots [21] = Reg.Register28;
    overflowSlots [22] = Reg.Register29;
    overflowSlots [23] = Reg.Register30;
    overflowSlots [24] = Reg.Register31;
    overflowSlots [25] = Reg.Result;
    overflowSlots [26] = full ? Factory.True : Factory.False;
  }

  public void prepare (int lastslot)
  {
    // Prepare the frame for a new set of slots.
    // The `lastslot' variable tells us the new capacity, which may
    // be bigger, smaller or the same as before.

    // If we aren't changing the lastslot, nothing need be done.
    if (this.lastslot == lastslot) return;

    // If we don't have enough slots, grow and initialize the overflow vector.
    // Since the grown vector begins clear, we clear the dirty flag, too.
    if (this.capacity() <= lastslot) {
	this.overflowSlots = new SObject[1 + lastslot - Cont.NUM_SLOT_FIELDS];
	for (int oi = 0; oi < overflowSlots.Length; ++oi) {
	    this.overflowSlots[oi] = Factory.False;
	    }
	this.dirty = false;
	}
    else {
	// Otherwise, the number of slots shrunk and we must set
	// the dirty flag.
	this.dirty = true;
	}
    this.lastslot = lastslot;
  }

  public void prepare0 ()
  {
    if (this.lastslot != 0) {
	this.dirty = true;
	this.lastslot = 0;
	}
  }

  // Prepare assuming capacity is sufficient.
  public void prepare_small (int lastslot)
  {
    if (this.lastslot != lastslot) {
	this.dirty = true;
	this.lastslot = lastslot;
	}
  }

  // If we shrink the overflowSlots, we clear them to aid the GC.
  public void clear()
  {
    if (this.dirty) {
	int limit = this.capacity();
	for (int i = this.lastslot + 1; i < limit; ++i) {
	    this.setSlot (i, Factory.False);
	    }
	this.dirty = false;
	}
  }

  public void checkPop (int lastslot, Procedure reg0)
  {
    if (this.lastslot != lastslot) {
	Exn.internalError ("pop: wrong number of slots");
	}
    if (this.s0 != reg0) {
	Exn.internalError ("pop: can't pop someone else's frame!");
	}
  }

  // Combines the effect of checkpop and pop.
  public void SafePop (int lastslot)
  {
    if (this.lastslot != lastslot)
	Exn.internalError ("pop: wrong number of slots.  Expected "
			   + lastslot.ToString()
			   + ", got "
			   + this.lastslot.ToString());

    if (this.s0 != Reg.Register0)
	Exn.internalError ("pop: can't pop someone else's frame!");

    Cont.pop();
  }

  public CodeAddress ReturnAddress
  {
    get {
	return this.s0.entrypoint.Address (this.returnIndex);
	}
  }
}

public sealed class StackCacheFrame : ContinuationFrame
{
  public StackCacheFrame before;
  public StackCacheFrame after;

  public StackCacheFrame()
  {
    this.clear();
  }
}
}

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
     * [0: return address(fixnum),
     *  1: dynamic link (another heap frame / #f)
     *  2: slot 0 = Register0 = Procedure, Has codevector of return address
     *  3: slot 1
     *  ...]
     *
     * The initial continuation will have a parent continuation of #f when a heap frame.
     */

    public class Cont {
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
        public static StackCacheFrame ROOT;
        public static StackCacheFrame LIMIT;
        public static StackCacheFrame SENTINEL;

        // static constructor initializes stack cache structure
        static Cont() {
            SENTINEL = new StackCacheFrame();
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
        public static void reset() {
            heap = Factory.False;
            cont = ROOT;
            cont.prepare(0);
            cont.slot0 = InitialContinuation.singletonProcedure;
            cont.returnIndex = 0;
        }

        // clear
        // Clear all frames in the cache (so GC doesn't hold dead objects)
        // Called on every timer interrupt (see Exn.faultTimer).
        public static void clear() {
            StackCacheFrame frame = ROOT;
            while (frame != SENTINEL) {
                frame.clear();
                frame = frame.after;
            }
        }

        /* save
         */
        public static void save(int lastslot) {
            if (cont == LIMIT) {
                heap = copyOutStack();
                cont = SENTINEL;
            }
            cont = cont.after;
            cont.prepare(lastslot);
            cont.slot0 = Reg.register0;
        }

        /* pop
         */
        public static void pop() {
            cont.lastslot = -1;
            cont = cont.before;
            if (cont == SENTINEL) {
                fillCache();
            }
        }

        /* copyOutStack
         * Flush all frames out of the cache to the heap. Does not change stack cache.
         */
        public static SObject copyOutStack() {
#if HAS_PERFORMANCE_COUNTERS
            if (stackFlushCounter != null) stackFlushCounter.Increment();
#endif
            SObject h = heap;
            for (StackCacheFrame f = ROOT; f != cont.after; f = f.after) {
                h = f.toVector(h);
            }
            return h;
        }

        /* fillCache
         */
        public static void fillCache() {
#if HAS_PERFORMANCE_COUNTERS
            if (stackReloadCounter != null) stackReloadCounter.Increment();
#endif
            cont = ROOT;
            if (!(heap is SVL)) {
                Exn.internalError("fillCache: Cont.heap is not a vector");
            } else {
                SVL h = (SVL) heap;
                heap = h.elements[Cont.HC_DYNLINK];
                cont.fillFromVector(h);
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
//                f.fillFromVector(h);
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
        public static SObject getCC() {
            return copyOutStack();
        }

        /* setCC
         * Install arg as current continuation
         */
        public static void setCC(SObject k) {
            heap = k;
            fillCache();
        }
    }

    public class ContinuationFrame {
        // If you modify the number of slot fields, remember
        // to change the constant NUM_SLOT_FIELDS above.
        public SObject slot0;
        public SObject slot1;
        public SObject slot2;
        public SObject slot3;
        public SObject slot4;
        public SObject slot5;
        public SObject slot6;
        public SObject slot7;
        public SObject[] overflowSlots;

        public int returnIndex;

        public int lastslot;
        // indicates that the overflowSlots may contain data beyond the lastslot.
        public bool dirty;

        public ContinuationFrame() {
            this.lastslot = -1;
            this.dirty = false;
            this.overflowSlots = new SObject[] {};
            this.clear();
        }

        private int capacity() {
            return Cont.NUM_SLOT_FIELDS + this.overflowSlots.Length;
        }

        // this is called frequently enough to warrant special treatment
        // we unroll the fill loop in the small cases, and set things up to
        // use the built-in array copy in the big cases.

        public SObject toVector(SObject parent)
        {
            SObject[] elements = new SObject[this.lastslot + Cont.HC_OVERHEAD + 1];
            elements[Cont.HC_RETOFFSET] = Factory.makeNumber (this.returnIndex);
            elements[Cont.HC_DYNLINK] = parent;
            switch (this.lastslot) {
              case 0:
                elements [Cont.HC_OVERHEAD + 0] = slot0;
                break;

              case 1:
                elements [Cont.HC_OVERHEAD + 0] = slot0;
                elements [Cont.HC_OVERHEAD + 1] = slot1;
                break;

              case 2:
                elements [Cont.HC_OVERHEAD + 0] = slot0;
                elements [Cont.HC_OVERHEAD + 1] = slot1;
                elements [Cont.HC_OVERHEAD + 2] = slot2;
                break;

              case 3:
                elements [Cont.HC_OVERHEAD + 0] = slot0;
                elements [Cont.HC_OVERHEAD + 1] = slot1;
                elements [Cont.HC_OVERHEAD + 2] = slot2;
                elements [Cont.HC_OVERHEAD + 3] = slot3;
                break;

              case 4:
                elements [Cont.HC_OVERHEAD + 0] = slot0;
                elements [Cont.HC_OVERHEAD + 1] = slot1;
                elements [Cont.HC_OVERHEAD + 2] = slot2;
                elements [Cont.HC_OVERHEAD + 3] = slot3;
                elements [Cont.HC_OVERHEAD + 4] = slot4;
                break;

              case 5:
                elements [Cont.HC_OVERHEAD + 0] = slot0;
                elements [Cont.HC_OVERHEAD + 1] = slot1;
                elements [Cont.HC_OVERHEAD + 2] = slot2;
                elements [Cont.HC_OVERHEAD + 3] = slot3;
                elements [Cont.HC_OVERHEAD + 4] = slot4;
                elements [Cont.HC_OVERHEAD + 5] = slot5;
                break;

              case 6:
                elements [Cont.HC_OVERHEAD + 0] = slot0;
                elements [Cont.HC_OVERHEAD + 1] = slot1;
                elements [Cont.HC_OVERHEAD + 2] = slot2;
                elements [Cont.HC_OVERHEAD + 3] = slot3;
                elements [Cont.HC_OVERHEAD + 4] = slot4;
                elements [Cont.HC_OVERHEAD + 5] = slot5;
                elements [Cont.HC_OVERHEAD + 6] = slot6;
                break;

              case 7:
                elements [Cont.HC_OVERHEAD + 0] = slot0;
                elements [Cont.HC_OVERHEAD + 1] = slot1;
                elements [Cont.HC_OVERHEAD + 2] = slot2;
                elements [Cont.HC_OVERHEAD + 3] = slot3;
                elements [Cont.HC_OVERHEAD + 4] = slot4;
                elements [Cont.HC_OVERHEAD + 5] = slot5;
                elements [Cont.HC_OVERHEAD + 6] = slot6;
                elements [Cont.HC_OVERHEAD + 7] = slot7;
                break;

              default:
                elements [Cont.HC_OVERHEAD + 0] = slot0;
                elements [Cont.HC_OVERHEAD + 1] = slot1;
                elements [Cont.HC_OVERHEAD + 2] = slot2;
                elements [Cont.HC_OVERHEAD + 3] = slot3;
                elements [Cont.HC_OVERHEAD + 4] = slot4;
                elements [Cont.HC_OVERHEAD + 5] = slot5;
                elements [Cont.HC_OVERHEAD + 6] = slot6;
                elements [Cont.HC_OVERHEAD + 7] = slot7;
                // Copy the overflow slots
                System.Array.Copy (overflowSlots, 0, elements, Cont.HC_OVERHEAD + 8, this.lastslot - 7);
                break;
               }
            return Factory.makeVector(elements);
        }

        public void fillFromVector(SVL hframe) {
            SObject[] elements = hframe.elements;

            this.prepare(elements.Length - Cont.HC_OVERHEAD - 1);
            this.returnIndex = ((SFixnum)elements[Cont.HC_RETOFFSET]).value;
            for (int si = 0; si <= this.lastslot; ++si) {
                this.setSlot(si, elements[si + Cont.HC_OVERHEAD]);
            }
        }

        public SObject getSlot(int slot) {
            switch (slot) {
            case 0: return slot0;
            case 1: return slot1;
            case 2: return slot2;
            case 3: return slot3;
            case 4: return slot4;
            case 5: return slot5;
            case 6: return slot6;
            case 7: return slot7;
            default: return overflowSlots[slot - Cont.NUM_SLOT_FIELDS];
            }
        }

        public void setSlot(int slot, SObject value) {
            switch (slot) {
            case 0: slot0 = value; return;
            case 1: slot1 = value; return;
            case 2: slot2 = value; return;
            case 3: slot3 = value; return;
            case 4: slot4 = value; return;
            case 5: slot5 = value; return;
            case 6: slot6 = value; return;
            case 7: slot7 = value; return;
            default:
                overflowSlots[slot - Cont.NUM_SLOT_FIELDS] = value;
                return;
            }
        }

        // Saving all the registers into the frame is done when we save the
        // context.  This is done so frequently that we can get a substantial
        // improvement by unrolling the entire thing.
        // Unfortunately, this crosses the abstraction barrier around the registers.

        public void saveRegisters (Procedure restore, bool full) {
            slot0 = restore;
            returnIndex = Reg.implicitContinuation;
            slot1 = Reg.register0;
            slot2 = Reg.register1;
            slot3 = Reg.register2;
            slot4 = Reg.register3;
            slot5 = Reg.register4;
            slot6 = Reg.register5;
            slot7 = Reg.register6;
            overflowSlots  [0] = Reg.register7;
            overflowSlots  [1] = Reg.register8;
            overflowSlots  [2] = Reg.register9;
            overflowSlots  [3] = Reg.register10;
            overflowSlots  [4] = Reg.register11;
            overflowSlots  [5] = Reg.register12;
            overflowSlots  [6] = Reg.register13;
            overflowSlots  [7] = Reg.register14;
            overflowSlots  [8] = Reg.register15;
            overflowSlots  [9] = Reg.register16;
            overflowSlots [10] = Reg.register17;
            overflowSlots [11] = Reg.register18;
            overflowSlots [12] = Reg.register19;
            overflowSlots [13] = Reg.register20;
            overflowSlots [14] = Reg.register21;
            overflowSlots [15] = Reg.register22;
            overflowSlots [16] = Reg.register23;
            overflowSlots [17] = Reg.register24;
            overflowSlots [18] = Reg.register25;
            overflowSlots [19] = Reg.register26;
            overflowSlots [20] = Reg.register27;
            overflowSlots [21] = Reg.register28;
            overflowSlots [22] = Reg.register29;
            overflowSlots [23] = Reg.register30;
            overflowSlots [24] = Reg.register31;
            overflowSlots [25] = Reg.Result;
            overflowSlots [26] = full ? Factory.True : Factory.False;
            }

        public void prepare(int lastslot) {
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
            } else {
              // Otherwise, the number of slots shrunk and we must set
              // the dirty flag.
              this.dirty = true;
            }
            this.lastslot = lastslot;
        }

      // If we shrink the overflowSlots, we clear them to aid the GC.
        public void clear() {
          if (this.dirty) {
              int limit = this.capacity();
              for (int i = this.lastslot + 1; i < limit; ++i) {
                  this.setSlot(i, Factory.False);
                  }
              this.dirty = false;
              }
        }

        public void checkPop(int lastslot, SObject reg0) {
            if (this.lastslot != lastslot) {
                Exn.internalError("pop: wrong number of slots");
            }
            if (this.slot0 != reg0) {
                Exn.internalError("pop: can't pop someone else's frame!");
            }
        }
    }

    public class StackCacheFrame : ContinuationFrame {
        public StackCacheFrame before;
        public StackCacheFrame after;

        public StackCacheFrame() {
            this.clear();
        }
    }
}

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
        // How many individual slot fields before we use
        // the overflow array
        // named slot0, slot1, ..., slot{NUM_SLOT_FIELDS-1}
        public static readonly int NUM_SLOT_FIELDS = 8;

        // How many frames to keep in the cache.
        public static readonly int NUM_CACHE_FRAMES = 100; //40; // 10;

        // How many frames to restore on fillCache (max)
        public static readonly int MAX_RESTORE_FRAMES = 25;             

        // Heap frames : described later in file
        public static SObject heap = Factory.False;

        // Offsets within HEAP FRAME of return address, dynamic link, return procedure, and first slot (same)
        // Adjusted down by one from C version.
        public static readonly int HC_RETOFFSET = Constants.HC_RETOFFSET - 1;
        public static readonly int HC_DYNLINK = Constants.HC_DYNLINK - 1;
        public static readonly int HC_PROC = Constants.HC_PROC - 1;
        public static readonly int HC_OVERHEAD = Constants.HC_OVERHEAD - 1;

        public static readonly int HC_CONTEXT_REG0 = Cont.HC_PROC + 1;
        public static readonly int HC_CONTEXT_RESULT = Cont.HC_CONTEXT_REG0 + Reg.NREGS;
        public static readonly int HC_CONTEXT_RESTORE_RESULT = Cont.HC_CONTEXT_RESULT + 1;

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
            SObject h = heap;
            for (StackCacheFrame f = ROOT; f != cont.after; f = f.after) {
                h = f.toVector(h);
            }
            return h;
        }

        /* fillCache
         */
        public static void fillCache() {
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
        public bool dirty;

        public ContinuationFrame() {
            this.lastslot = -1;
            this.dirty = true;
            this.overflowSlots = new SObject[] {};
            this.clear();
        }

        private int capacity() {
            return Cont.NUM_SLOT_FIELDS + this.overflowSlots.Length;
        }

        public SObject toVector(SObject parent) {
            SObject[] elements = new SObject[this.lastslot + Cont.HC_OVERHEAD + 1];
            elements[Cont.HC_RETOFFSET] = Factory.wrap(this.returnIndex);
            elements[Cont.HC_DYNLINK] = parent;
            for (int si = 0; si <= this.lastslot; ++si) {
                elements[si + Cont.HC_OVERHEAD] = this.getSlot(si);
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

        public void prepare(int lastslot) {
            if (this.capacity() <= lastslot) {
                this.overflowSlots = new SObject[1 + lastslot - Cont.NUM_SLOT_FIELDS];
                for (int oi = 0; oi < overflowSlots.Length; ++oi) {
                    this.overflowSlots[oi] = Factory.False;
                }
            }
            this.lastslot = lastslot;
            this.dirty = true;
        }

        public void clear() {
            if (! this.dirty) return;
            for (int i = this.lastslot + 1; i < this.capacity(); ++i) {
                this.setSlot(i, Factory.False);
            }
            this.dirty = false;
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

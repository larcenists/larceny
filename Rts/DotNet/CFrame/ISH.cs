using System;
using System.Collections;
using SchemeObjects;

namespace MacScheme
{
  /**
   * Cont implements continuation/stack related instructions.
   * pop
   * save
   * rtn
   * setrtn
   * stack
   * setstk
   * load
   * store
   */
  public class Cont
  {
    // Number of extra slots per frame.
    public static readonly int NUM_OVERFLOW_SLOTS = 24;

    // How many frames to keep in the cache.
    public static readonly int NUM_CACHE_FRAMES = 100; //40; // 10;

    // How many individual slot fields before we use
    // the overflow array
    // named slot0, slot1, ..., slot{NUM_SLOT_FIELDS-1}
    public static readonly int NUM_SLOT_FIELDS = 8;

    // Heap frames
    private static HeapFrame heap = null;

    // Reference to current top of stack.
    public static CacheLink cont;
    private static CacheLink TOP;
    private static CacheLink BOT;
  
    static Cont()
      {
	BOT = new CacheLink(null);
	cont = BOT;
      
	for (int i = 1; i < NUM_CACHE_FRAMES; ++i)
	  {
	    CacheLink next = new CacheLink(cont);
	    cont.above = next;
	    cont = next;
	    TOP = next;
	  }
	// No more frames above this.
	cont.above = null;

	cont = BOT;
      }
    
    // flush the stack cache out to the heap and leave
    // frame referencing the bottom of the cache.
    // frame should be at the top of the stack cache when called.
    public static void flushCache()
      {
	for (cont = BOT; cont != TOP; cont = cont.above)
	  {
	    heap = new HeapFrame(cont.frame, heap);
	  }
	heap = new HeapFrame(cont.frame, heap);

	cont = BOT;
      }
    
    // fill the stack cache from the heap
    // expects frame to reference bottom of stack
    // leaves frame referencing top of stack.
    public static void fillCache()
      {
	for (cont = TOP; cont != BOT; cont = cont.below) {
	  cont.copyFrame(heap);
	  heap = heap.parent;
	}
	cont.copyFrame(heap);
	heap = heap.parent;

	cont = TOP;
      }
    
    // leave the reified current continuation in RESULT
    public static void reifyCC() {
      // Reify what's in the heap.
      HeapFrame reified = heap;
      
      // Extend that with what's in the cache.
      for (CacheLink i = BOT; i != cont; i = i.above) {
	reified = new HeapFrame(i.frame, reified);
      }
      reified = new HeapFrame(cont.frame, reified);
      
      Reg.Result = new Continuation(reified);
    }
    
    // take the reified continuation in RESULT
    // and install it as current continuation.
    public static void installContinuation() {
      Continuation k = (Continuation) Reg.Result;
      // We're only getting one frame from the heap, so put
      // it at the bottom.
      cont = BOT;
      // Copy first frame from saved frames into cache.
      cont.copyFrame(k.hf);
      // Leave the rest of it in the heap for now.
      heap = k.hf.parent;
    }

    public static void pop(uint slots)
      {
	if (cont.frame.numUsedSlots != slots)
	  {
	    Exn.fault("pop: wrong number of slots");
	    // TAIL CALL
	    return;
	  }
	
	if (cont.below == null)
	  {
	    if (heap == null)
	      {
		// We're back at the initial continuation... set frame to
		// null to cause a NullReferenceException in Call.call()
		cont.frame = null;
		return;
	      }
	    else
	      {
		fillCache();
		// TAIL CALL
		return;
	      }
	  }
	else
	  {
	    cont = cont.below;
	  }
      }
		
    public static void save(uint slot_count)
      {
	if (cont.above == null)
	  {
	    flushCache();
	  }
	else
	  {
	    cont = cont.above;
	  }

	cont.frame.numUsedSlots = slot_count;
      }
    
    public static void rtn()
      {
      
	Call.call(cont.frame.rtn_point,
		  cont.frame.rtn_index);
	// TAIL CALL
	return;
	
	// frame.rtn_point == null at the initial continuation,
	// throws NullReferenceException when it gets
	// to the original continuation
      }

    public static void setrtn(CodeVector cv, int index)
      {
	cont.frame.rtn_point = cv;
	cont.frame.rtn_index = index;
      }
    
    public static void stack(uint slot)
      {
	switch (slot)
	  {
	  case 0:
	    Reg.Result = cont.frame.slot0;
	    return;
	  case 1:
	    Reg.Result = cont.frame.slot1;
	    return;
	  case 2:
	    Reg.Result = cont.frame.slot2;
	    return;
	  case 3:
	    Reg.Result = cont.frame.slot3;
	    return;
	  case 4:
	    Reg.Result = cont.frame.slot4;
	    return;
	  case 5:
	    Reg.Result = cont.frame.slot5;
	    return;
	  case 6:
	    Reg.Result = cont.frame.slot6;
	    return;
	  case 7:
	    Reg.Result = cont.frame.slot7;
	    return;
	  default:
	    Reg.Result = cont.frame.overflowSlots[slot-NUM_SLOT_FIELDS];
	    return;
	  } 
      }
    public static void setstk(uint slot)
      {
	switch (slot)
	  {
	  case 0:
	    cont.frame.slot0 = Reg.Result;
	    return;
	  case 1:
	    cont.frame.slot1 = Reg.Result;
	    return;
	  case 2:
	    cont.frame.slot2 = Reg.Result;
	    return;
	  case 3:
	    cont.frame.slot3 = Reg.Result;
	    return;
	  case 4:
	    cont.frame.slot4 = Reg.Result;
	    return;
	  case 5:
	    cont.frame.slot5 = Reg.Result;
	    return;
	  case 6:
	    cont.frame.slot6 = Reg.Result;
	    return;
	  case 7:
	    cont.frame.slot7 = Reg.Result;
	    return;
	  default:
	    cont.frame.overflowSlots[slot-NUM_SLOT_FIELDS] = Reg.Result;
	    return;
	  }
      }
 
    public static void load(uint k, uint slot)
      {
	// Instead of repeating the switch above
	SchemeObject tmp = Reg.Result;
	Cont.stack(slot);
	Reg.setreg(k);
	Reg.Result = tmp;
      }
    public static void store(uint k, uint slot)
      {
	// Instead of repeating the switch above.
	SchemeObject tmp = Reg.Result;
	Reg.reg(k);
	Cont.setstk(slot);
	Reg.Result = tmp;
      }
  }

  public class ContinuationFrame
  {
    // If you modify the number of slot fields, remember
    // to change the constant NUM_SLOT_FIELDS above.
    // Also fix stack and setstk instructions.
    public SchemeObject slot0;
    public SchemeObject slot1;
    public SchemeObject slot2;
    public SchemeObject slot3;
    public SchemeObject slot4;
    public SchemeObject slot5;
    public SchemeObject slot6;
    public SchemeObject slot7;

    public SchemeObject[] overflowSlots = new SchemeObject[Cont.NUM_OVERFLOW_SLOTS];
    
    public CodeVector rtn_point;
    public int rtn_index;

    public uint numUsedSlots;
  }

  public class CacheLink {
    static int count = 0;
    int thiscount;

    public ContinuationFrame frame = new ContinuationFrame();
    public CacheLink above;
    public CacheLink below;

    public CacheLink(CacheLink below) {
      this.below = below;
      thiscount = count++;
    }

    public override string ToString() {
      return ("CacheLink " + thiscount);
    }
    
    public void copyFrame(ContinuationFrame f) {
      frame.slot0 = f.slot0;
      frame.slot1 = f.slot1;
      frame.slot2 = f.slot2;
      frame.slot3 = f.slot3;
      frame.slot4 = f.slot4;
      frame.slot5 = f.slot5;
      frame.slot6 = f.slot6;
      frame.slot7 = f.slot7;

      // Probably have to copy this the hard way too
      frame.overflowSlots = f.overflowSlots;

      // CodeVectors are immutable.
      frame.rtn_point = f.rtn_point;
      frame.rtn_index = f.rtn_index;

      frame.numUsedSlots = f.numUsedSlots;
    }
  }

  public class HeapFrame : ContinuationFrame {
    public readonly HeapFrame parent;

    public HeapFrame(ContinuationFrame copy, HeapFrame parent) {
      this.parent = parent;
      this.slot0 = copy.slot0;
      this.slot1 = copy.slot1;
      this.slot2 = copy.slot2;
      this.slot3 = copy.slot3;
      this.slot4 = copy.slot4;
      this.slot5 = copy.slot5;
      this.slot6 = copy.slot6;
      this.slot7 = copy.slot7;

      this.overflowSlots = copy.overflowSlots;

      this.rtn_point = copy.rtn_point;
      this.rtn_index = copy.rtn_index;

      this.numUsedSlots = copy.numUsedSlots;
    }
  }

}

using System;
using Scheme.RT;

namespace Scheme.RT {
    public class Cont {

        // How many individual slot fields before we use
        // the overflow array
        // named slot0, slot1, ..., slot{NUM_SLOT_FIELDS-1}
        public static readonly int NUM_SLOT_FIELDS = 8;

        // Offsets within HEAP FRAME of return address, dynamic link, return procedure, and first slot (same)
        // Adjusted down by one from C version.
        public static readonly int HC_RETOFFSET = Constants.HC_RETOFFSET - 1;
        public static readonly int HC_DYNLINK = Constants.HC_DYNLINK - 1;
        public static readonly int HC_PROC = Constants.HC_PROC - 1;
        public static readonly int HC_OVERHEAD = Constants.HC_OVERHEAD - 1;

        public static readonly int HC_CONTEXT_REG0 = Cont.HC_PROC + 1;
        public static readonly int HC_CONTEXT_RESULT = Cont.HC_CONTEXT_REG0 + Reg.NREGS;
        public static readonly int HC_CONTEXT_RESTORE_RESULT = Cont.HC_CONTEXT_RESULT + 1;

        public static ContinuationFrame cont;

        public static void save(uint lastslot) {
            cont = new ContinuationFrame(cont);
            cont.prepare(lastslot);
            cont.slot0 = Reg.register0;
        }
        
        public static void pop() {
            if (cont == null) {
	            Exn.internalError("pop: underflowed stack");
	            // TAIL CALL
	            return;
            }
            cont = cont.before;
        }
		
        public static SObject getCC() {
            return cont;
        }
        public static void setCC(SObject frame) {
            cont = (ContinuationFrame) frame;
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

        public string createdFile;
        public int createdLine;

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

        // Methods
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
            if (lastslot >= Cont.NUM_SLOT_FIELDS) {
                overflowSlots = new SObject[1 + lastslot - Cont.NUM_SLOT_FIELDS];
                for (int oi = 0; oi < overflowSlots.Length; ++oi) {
                    overflowSlots[oi] = SObject.False;
                }
            }
            this.lastslot = lastslot;
            this.createdFile = Reg.debugFile;
            this.createdLine = Reg.debugLocation;
        }

        public void clear() {
            slot0 = SObject.False;
            slot1 = SObject.False;
            slot2 = SObject.False;
            slot3 = SObject.False;
            slot4 = SObject.False;
            slot5 = SObject.False;
            slot6 = SObject.False;
            slot7 = SObject.False;
            overflowSlots = null;
            returnIndex = 0;
            lastslot = -1;
            createdFile = null;
            createdLine = 0;
        }
        
        public void checkSlotsUsed(int lastslot) {
            if (this.lastslot != lastslot) {
                Exn.internalError("pop: wrong number of slots");
            }
        }
    }
}

using System;
using Scheme.Rep;

namespace Scheme.RT {

public class Instructions {

    /* ===================================================== */
    /*   Registers, Constants, and Top-level Environment     */
    /* ===================================================== */

    /** nop */
    public static void nop() {
        return;
    }

    /** global
     * Retrieves global variable at the given index from the current
     * Procedure's global array. Uses given string name to report error.
     */
    public static void global(int index, string name) {
        Procedure thisProc = Reg.register0;
	// This way of doing it does dynamic casting, which is very slow.
	//
	//        SPair cell = (SPair)thisProc.constants[index];
	//
	//        if (cell.first == Factory.Undefined) {
	//            Exn.fault(Constants.EX_UNDEF_GLOBAL,
	//                      "reference to unbound global: " + name,
	//                      cell);
	//            return;
	//        } else {
	//            Reg.Result = cell.first;
	//        }

	SObject value = thisProc.constants[index].op_cell_ref();
	if (value == Factory.Undefined) {
	    Exn.fault (Constants.EX_UNDEF_GLOBAL,
	                      "reference to unbound global: " + name,
	                      thisProc.constants[index]);
	    return;
	    }
	else {
	    Reg.Result = value;
	    }
    }

    /** setglbl
     * Set global variable at given index in current Procedure's global
     * array.
     */
    public static void setglbl(int index) {
      Reg.register0.constants[index].op_cell_set (Reg.Result);
      return;
    }

    /** constant
     */
    public static void constant(int constIndex) {
        Reg.Result = Reg.register0.constants[constIndex];
    }

    /** imm_constant
     * Loads Reg.Result with immediate constant
     */
    public static void imm_constant(SObject immediate) {
        Reg.Result = immediate;
    }

    /** imm_const_setreg
     * Loads given register with immediate constant
     */
    public static void imm_const_setreg(SObject immediate, int register) {
        Reg.setRegister(register, immediate);
    }

    /** reg
     * Load Register k into Reg.Result
     */
    public static void reg(int k) {
        Reg.Result = Reg.getRegister(k);
    }

    /** setreg
     * Load Reg.Result into Register k
     */
    public static void setreg(int k) {
        Reg.setRegister(k, Reg.Result);
    }

    /** movereg
     * Move Register src to Register dst
     */
    public static void movereg(int src, int dst) {
        Reg.setRegister(dst, Reg.getRegister(src));
    }

    /* ===================================================== */
    /*   Call                                                */
    /* ===================================================== */

    /** invoke
     * Pass control to Procedure in Result; fault if
     * Result doesn't contain Procedure; uses trampoline
     */
    public static void invoke(int argc) {
        Procedure proc = Reg.Result as Procedure;
        if (proc != null)
            Call.call (proc, argc);
        else
            Exn.fault(Constants.EX_NONPROC,
                      "invoke: not a procedure",
                      Reg.Result);
    }

    /** apply
     * Pass control to Procedure in Result; fault if Result doesn't
     * contain Procedure; uses trampoline, arranges arguments from
     * list in register k1
     */
    public static void apply(int k1, int k2) {
        Procedure proc = Reg.Result as Procedure;
        if (proc != null)
            Call.call (proc, Call.applySetup (k1, k2));
        else

            Exn.fault(Constants.EX_NONPROC, "apply: not a procedure", Reg.Result);
     }

    /* ===================================================== */
    /*   Closures                                            */
    /* ===================================================== */


    /** lambda
     * Create a new Procedure based on current Procedure (Register 0),
     * given CodeVector, and given number of registers to close over
     */
    public static void lambda(CodeVector codevector,
                              int constIndex, int numRegs) {
        Procedure thisProc = Reg.register0;

        if (numRegs < Reg.LASTREG) {
            Reg.Result = new Procedure (codevector, thisProc.constants[constIndex], Reg.Close(numRegs));
        } else {
            SObject[] environment = new SObject[numRegs+1];
            for (int i = 0; i <= Reg.LASTREG -1; ++i) {
                environment[i] = Reg.getRegister(i);
            }
            SObject restlist = Reg.getRegister(Reg.LASTREG);
            for (int i = Reg.LASTREG; i < numRegs+1; ++i) {
                SPair restpair = (SPair) restlist;
                environment[i] = restpair.first;
                restlist = restpair.rest;
            }
        Reg.Result = new Procedure(codevector, thisProc.constants[constIndex], environment);
        }
    }

    /** lexes
     * Like lambda, but takes CodeVector and Constants from current Procedure
     */
    public static void lexes(int numRegs) {
        Procedure thisProc = Reg.register0;

        if (numRegs < Reg.LASTREG) {
           Reg.Result = new Procedure (thisProc.entrypoint, thisProc.constantvector, Reg.Close (numRegs));
        } else {
            SObject[] environment = new SObject[numRegs+1];
            for (int i = 0; i <= Reg.LASTREG -1; ++i) {
                environment[i] = Reg.getRegister(i);
            }
            SObject restlist = Reg.getRegister(Reg.LASTREG);
            for (int i = Reg.LASTREG; i < numRegs+1; ++i) {
                SPair restpair = (SPair) restlist;
                environment[i] = restpair.first;
                restlist = restpair.rest;
            }
        Reg.Result = new Procedure(thisProc.entrypoint,
                                   thisProc.constantvector,
                                   environment);
        }
    }

    /** lexical
     * Load the value at the specified lexical address into Reg.Result.
     * Inlined by IL version
     */
    public static void lexical(int rib, int slot) {
        Reg.Result = Reg.register0.lookup(rib, slot);
    }

    /** setlex
     * Store Reg.Result in the specified lexical address.
     * Reg.Result is not destroyed in this implementation,
     * contrary to the warning in the MAL docs.
     */
    public static void setlex(int rib, int slot) {
        Reg.register0.update(rib, slot, Reg.Result);
    }

    /** argseq
     * Fault if Reg.Result isn't a number equal to n.
     * Used to check procedure arity.
     */
    public static void argseq(int n) {
        if (((SFixnum)Reg.Result).value != n)
            Exn.fault(Constants.EX_ARGSEQ, "args= check failed");
    }

    /** argsge
     * FIXME:  Not complete
     * In the docs, r = Reg.NREGS-1 (zero based index of the last register).
     */
    public static void argsge(int n) {
        int j = ((SFixnum)Reg.Result).value;

        if (j < n) {
            Exn.faultVarArgCount(n);
        }

        if (n < Reg.NREGS - 2) {
            if (j < Reg.LASTREG) {
                // Case 0 in instruction set doc
                SObject restlist = Factory.Null;
                for (int i = j; i >= n + 1; i--) {
                    restlist = Factory.makePair(Reg.getRegister(i), restlist);
                }
                Reg.setRegister(n + 1, restlist);
            } else {
                // Case 1 in instruction set doc
                //Exn.msg.WriteLine("argsge case 1: n = {0}; j = {1}", n, j);
                SObject original = Reg.getRegister(Reg.LASTREG);
                SObject copy = Factory.copyList(original);
                SObject restlist = copy;
                for (int i = Reg.LASTREG - 1; i >= n + 1; i--) {
                    restlist = Factory.makePair(Reg.getRegister(i), restlist);
                }
                Reg.setRegister(n + 1, restlist);
            }
        } else if (n == Reg.NREGS-2) {
            // Case 2 in instruction set doc
            //Exn.msg.WriteLine("argsge case 2: n = {0}; j = {1}", n, j);
            SObject original = Reg.getRegister(Reg.LASTREG);
            SObject copy = Factory.copyList(original);
            Reg.setRegister(Reg.LASTREG, Factory.makePair(copy, Factory.Null));
        } else {
            // Case 3 in instruction set doc
            //Exn.msg.WriteLine("argsge case 3: n = {0}; j = {1}", n, j);
            Exn.internalError("args>= case 3 not implemented");
        }
    }

    /* ===================================================== */
    /*   Continuations                                       */
    /* ===================================================== */

    public static void save(int lastslot) {
        Cont.save(lastslot);
    }

    public static void pop(int slots) {
        Cont.cont.checkPop(slots, Reg.register0);
        Cont.pop();
    }

    public static void rtn() {
        Call.call(Cont.cont.slot0.entrypoint, Cont.cont.returnIndex);
    }

    public static void setrtn(CodeVector code, int index) {
        if (code != Cont.cont.slot0.entrypoint) {
            Exn.internalError("setrtn: different codevector from slot0");
        }
        Cont.cont.returnIndex = index;
    }

    public static void stack(int slot) {
        Reg.Result = Cont.cont.getSlot(slot);
    }
    public static void setstk(int slot) {
        Cont.cont.setSlot(slot, Reg.Result);
    }
    public static void load(int k, int slot) {
        Reg.setRegister(k, Cont.cont.getSlot(slot));
    }
    public static void store(int k, int slot) {
        Cont.cont.setSlot(slot, Reg.getRegister(k));
    }

    /* ===================================================== */
    /*   Exceptions and Reporting                            */
    /* ===================================================== */

    /** trap
     * Documentation is lacking for this instruction.
     * Petit Larceny behavior is partially duplicated here;
     * for now we just cause general fault with error message.
     */
    public static void trap(int x, int y, int z, int excode) {
        if (x != 0) Reg.Result = Reg.getRegister(x);
        if (y != 0) Reg.Second = Reg.getRegister(y);
        if (z != 0) Reg.Third  = Reg.getRegister(z);
        Exn.fault(excode, "Trap occurred: " + excode);
    }

}
}

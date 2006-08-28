using System;
using System.Collections;
using Scheme.Rep;

namespace Scheme.RT {

public sealed class Reg {

  // If you change the number of registers, you must change
  // getRegister, setRegister, and spreadRegister in this file.
  // You must also change saveRegisters in ContinuationISH
  public const int NREGS = 32;
  public const int LASTREG = NREGS - 1;

  // General-Purpose Registers
  public static SObject r0;
  public static SObject r1;
  public static SObject r2;
  public static SObject r3;
  public static SObject r4;
  public static SObject r5;
  public static SObject r6;
  public static SObject r7;
  public static SObject r8;
  public static SObject r9;
  public static SObject r10;
  public static SObject r11;
  public static SObject r12;
  public static SObject r13;
  public static SObject r14;
  public static SObject r15;
  public static SObject r16;
  public static SObject r17;
  public static SObject r18;
  public static SObject r19;
  public static SObject r20;
  public static SObject r21;
  public static SObject r22;
  public static SObject r23;
  public static SObject r24;
  public static SObject r25;
  public static SObject r26;
  public static SObject r27;
  public static SObject r28;
  public static SObject r29;
  public static SObject r30;
  public static SObject r31;

  // FIXME: Make a separate shadow field of the appropriate type
  // and get rid of the cast in the accessor for ProcRegister0.
  public static Procedure ProcRegister0 { get { return (Procedure) r0; } set { r0 = value; } }
  public static SObject Register0 { get { return r0; } set { r0 = value; } }
  public static SObject Register1 { get { return r1; } set { r1 = value; } }
  public static SObject Register2 { get { return r2; } set { r2 = value; } }
  public static SObject Register3 { get { return r3; } set { r3 = value; } }
  public static SObject Register4 { get { return r4; } set { r4 = value; } }
  public static SObject Register5 { get { return r5; } set { r5 = value; } }
  public static SObject Register6 { get { return r6; } set { r6 = value; } }
  public static SObject Register7 { get { return r7; } set { r7 = value; } }
  public static SObject Register8 { get { return r8; } set { r8 = value; } }
  public static SObject Register9 { get { return r9; } set { r9 = value; } }
  public static SObject Register10 { get { return r10; } set { r10 = value; } }
  public static SObject Register11 { get { return r11; } set { r11 = value; } }
  public static SObject Register12 { get { return r12; } set { r12 = value; } }
  public static SObject Register13 { get { return r13; } set { r13 = value; } }
  public static SObject Register14 { get { return r14; } set { r14 = value; } }
  public static SObject Register15 { get { return r15; } set { r15 = value; } }
  public static SObject Register16 { get { return r16; } set { r16 = value; } }
  public static SObject Register17 { get { return r17; } set { r17 = value; } }
  public static SObject Register18 { get { return r18; } set { r18 = value; } }
  public static SObject Register19 { get { return r19; } set { r19 = value; } }
  public static SObject Register20 { get { return r20; } set { r20 = value; } }
  public static SObject Register21 { get { return r21; } set { r21 = value; } }
  public static SObject Register22 { get { return r22; } set { r22 = value; } }
  public static SObject Register23 { get { return r23; } set { r23 = value; } }
  public static SObject Register24 { get { return r24; } set { r24 = value; } }
  public static SObject Register25 { get { return r25; } set { r25 = value; } }
  public static SObject Register26 { get { return r26; } set { r26 = value; } }
  public static SObject Register27 { get { return r27; } set { r27 = value; } }
  public static SObject Register28 { get { return r28; } set { r28 = value; } }
  public static SObject Register29 { get { return r29; } set { r29 = value; } }
  public static SObject Register30 { get { return r30; } set { r30 = value; } }
  public static SObject Register31 { get { return r31; } set { r31 = value; } }

  public static SObject Result;
  public static SObject Second;
  public static SObject Third;

  static int argc;
  public static int Argcount
  {
    get { return argc; }
    set { argc = value; }
  }

  // Debugging
  // debugLocation: corresponds to line# in listing file
  public static int debugLocation;
  // debugFile: listing file current code
  public static string debugFile;

  // Timer Interrupts
  public const int TIME_SLICE = 80000;
  public const int SMALL_TIME_SLICE = 1000;

  public static int timer = TIME_SLICE;
  public static bool interruptsEnabled = false;

  // Assembly used to find compiled-in codevectors
  public static System.Reflection.Assembly programAssembly = null;

  // Implicit Continuation for Special Operations
  /* Jump index of implicit continuation in current procedure. Used by
   * (for example) arithmetic operations which may cause recoverable
   * exceptions. Set from call site, should be read by operation fault
   * handler. Not used by timer interrupts.
   */
  public static int implicitContinuation = -1;

  // Globals
  //
  // The literal `2300' is a tad more than the number of globals
  // created by the initial load.  It isn't necessary to get this
  // number correct, but getting it in the ballpark (and slightly
  // larger than necessary) reduces the amount of rehashing done
  // at startup time.
  public static Hashtable globals = new Hashtable (2300);

  /* ===================================================== */
  /*   Support Code                                        */
  /* ===================================================== */

  // Avoid using class initializer because it slows things down
  public static void Initialize (System.Reflection.Assembly pa)
  {
    programAssembly = pa;
    Timer.mainTimer.reset();
  }

  public static SPair globalCell (string identifier)
  {
    SPair cell = globals[identifier] as SPair;
    if (cell == null) {
	cell = new SPair(Factory.Undefined,
			 Factory.internSymbol (identifier));
	globals[identifier] = cell;
        }
    return cell;
  }

  public static SObject globalValue (string identifier)
  {
    return globalCell (identifier).first;
  }

  public static SObject getRegister (int index)
  {
    switch (index) {
    case 0:  return r0;
    case 1:  return r1;
    case 2:  return r2;
    case 3:  return r3;
    case 4:  return r4;
    case 5:  return r5;
    case 6:  return r6;
    case 7:  return r7;
    case 8:  return r8;
    case 9:  return r9;
    case 10: return r10;
    case 11: return r11;
    case 12: return r12;
    case 13: return r13;
    case 14: return r14;
    case 15: return r15;
    case 16: return r16;
    case 17: return r17;
    case 18: return r18;
    case 19: return r19;
    case 20: return r20;
    case 21: return r21;
    case 22: return r22;
    case 23: return r23;
    case 24: return r24;
    case 25: return r25;
    case 26: return r26;
    case 27: return r27;
    case 28: return r28;
    case 29: return r29;
    case 30: return r30;
    case 31: return r31;
    default:
	Exn.internalError("getRegister: register index out of bounds: " + index);
	return null;
        }
  }

  public static void setRegister (int index, SObject value)
  {
    switch (index) {
    case 0: r0 = value; break;
    case 1: r1 = value; break;
    case 2: r2 = value; break;
    case 3: r3 = value; break;
    case 4: r4 = value; break;
    case 5: r5 = value; break;
    case 6: r6 = value; break;
    case 7: r7 = value; break;
    case 8: r8 = value; break;
    case 9: r9 = value; break;
    case 10: r10 = value; break;
    case 11: r11 = value; break;
    case 12: r12 = value; break;
    case 13: r13 = value; break;
    case 14: r14 = value; break;
    case 15: r15 = value; break;
    case 16: r16 = value; break;
    case 17: r17 = value; break;
    case 18: r18 = value; break;
    case 19: r19 = value; break;
    case 20: r20 = value; break;
    case 21: r21 = value; break;
    case 22: r22 = value; break;
    case 23: r23 = value; break;
    case 24: r24 = value; break;
    case 25: r25 = value; break;
    case 26: r26 = value; break;
    case 27: r27 = value; break;
    case 28: r28 = value; break;
    case 29: r29 = value; break;
    case 30: r30 = value; break;
    case 31: r31 = value; break;
    default:
	Exn.internalError("setRegister: register index out of bounds: " + index);
	return;
        }
  }

  // Used by APPLY, this moves elements from a list of arguments
  // into a set of registers.  We try to avoid the overhead of looping
  // by unrolling the different cases.

  public static void spreadRegister (int listreg, int nregs)
  {
    // Spread the list of nregs elements stored in listreg
    // into registers 1 through 1+nregs

    // Fetch the list of arguments into pp.
    SPair pp;
    switch (listreg) {
	// case 0: pp = (SPair) Reg.register0; break;
    case 1: pp = (SPair) r1; break;
    case 2: pp = (SPair) r2; break;
    case 3: pp = (SPair) r3; break;
    case 4: pp = (SPair) r4; break;
    case 5: pp = (SPair) r5; break;
    case 6: pp = (SPair) r6; break;
    case 7: pp = (SPair) r7; break;
    case 8: pp = (SPair) r8; break;
    case 9: pp = (SPair) r9; break;
    case 10: pp = (SPair) r10; break;
    case 11: pp = (SPair) r11; break;
    case 12: pp = (SPair) r12; break;
    case 13: pp = (SPair) r13; break;
    case 14: pp = (SPair) r14; break;
    case 15: pp = (SPair) r15; break;
    case 16: pp = (SPair) r16; break;
    case 17: pp = (SPair) r17; break;
    case 18: pp = (SPair) r18; break;
    case 19: pp = (SPair) r19; break;
    case 20: pp = (SPair) r20; break;
    case 21: pp = (SPair) r21; break;
    case 22: pp = (SPair) r22; break;
    case 23: pp = (SPair) r23; break;
    case 24: pp = (SPair) r24; break;
    case 25: pp = (SPair) r25; break;
    case 26: pp = (SPair) r26; break;
    case 27: pp = (SPair) r27; break;
    case 28: pp = (SPair) r28; break;
    case 29: pp = (SPair) r29; break;
    case 30: pp = (SPair) r30; break;
    case 31: pp = (SPair) r31; break;
    default:
	Exn.internalError("spreadRegister: register index out of bounds: " + listreg);
	return;
        }

    switch (nregs) {
    case 1:
	r1 = pp.first; return;

    case 2:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; return;

    case 3:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; return;

    case 4:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; return;

    case 5:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; return;

    case 6:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; return;

    case 7:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; return;

    case 8:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; return;

    case 9:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; return;

    case 10:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; return;

    case 11:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; return;

    case 12:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; return;

    case 13:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; return;

    case 14:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; return;

    case 15:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; return;

    case 16:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; return;

    case 17:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; return;

    case 18:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; return;

    case 19:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; return;

    case 20:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; pp = (SPair) pp.rest;
	r20 = pp.first; return;

    case 21:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; pp = (SPair) pp.rest;
	r20 = pp.first; pp = (SPair) pp.rest;
	r21 = pp.first; return;

    case 22:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; pp = (SPair) pp.rest;
	r20 = pp.first; pp = (SPair) pp.rest;
	r21 = pp.first; pp = (SPair) pp.rest;
	r22 = pp.first; return;

    case 23:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; pp = (SPair) pp.rest;
	r20 = pp.first; pp = (SPair) pp.rest;
	r21 = pp.first; pp = (SPair) pp.rest;
	r22 = pp.first; pp = (SPair) pp.rest;
	r23 = pp.first; return;
    case 24:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; pp = (SPair) pp.rest;
	r20 = pp.first; pp = (SPair) pp.rest;
	r21 = pp.first; pp = (SPair) pp.rest;
	r22 = pp.first; pp = (SPair) pp.rest;
	r23 = pp.first; pp = (SPair) pp.rest;
	r24 = pp.first; return;

    case 25:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; pp = (SPair) pp.rest;
	r20 = pp.first; pp = (SPair) pp.rest;
	r21 = pp.first; pp = (SPair) pp.rest;
	r22 = pp.first; pp = (SPair) pp.rest;
	r23 = pp.first; pp = (SPair) pp.rest;
	r24 = pp.first; pp = (SPair) pp.rest;
	r25 = pp.first; return;

    case 26:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; pp = (SPair) pp.rest;
	r20 = pp.first; pp = (SPair) pp.rest;
	r21 = pp.first; pp = (SPair) pp.rest;
	r22 = pp.first; pp = (SPair) pp.rest;
	r23 = pp.first; pp = (SPair) pp.rest;
	r24 = pp.first; pp = (SPair) pp.rest;
	r25 = pp.first; pp = (SPair) pp.rest;
	r26 = pp.first; return;

    case 27:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; pp = (SPair) pp.rest;
	r20 = pp.first; pp = (SPair) pp.rest;
	r21 = pp.first; pp = (SPair) pp.rest;
	r22 = pp.first; pp = (SPair) pp.rest;
	r23 = pp.first; pp = (SPair) pp.rest;
	r24 = pp.first; pp = (SPair) pp.rest;
	r25 = pp.first; pp = (SPair) pp.rest;
	r26 = pp.first; pp = (SPair) pp.rest;
	r27 = pp.first; return;

    case 28:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; pp = (SPair) pp.rest;
	r20 = pp.first; pp = (SPair) pp.rest;
	r21 = pp.first; pp = (SPair) pp.rest;
	r22 = pp.first; pp = (SPair) pp.rest;
	r23 = pp.first; pp = (SPair) pp.rest;
	r24 = pp.first; pp = (SPair) pp.rest;
	r25 = pp.first; pp = (SPair) pp.rest;
	r26 = pp.first; pp = (SPair) pp.rest;
	r27 = pp.first; pp = (SPair) pp.rest;
	r28 = pp.first; return;

    case 29:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; pp = (SPair) pp.rest;
	r20 = pp.first; pp = (SPair) pp.rest;
	r21 = pp.first; pp = (SPair) pp.rest;
	r22 = pp.first; pp = (SPair) pp.rest;
	r23 = pp.first; pp = (SPair) pp.rest;
	r24 = pp.first; pp = (SPair) pp.rest;
	r25 = pp.first; pp = (SPair) pp.rest;
	r26 = pp.first; pp = (SPair) pp.rest;
	r27 = pp.first; pp = (SPair) pp.rest;
	r28 = pp.first; pp = (SPair) pp.rest;
	r29 = pp.first; return;

    case 30:
	r1 = pp.first; pp = (SPair) pp.rest;
	r2 = pp.first; pp = (SPair) pp.rest;
	r3 = pp.first; pp = (SPair) pp.rest;
	r4 = pp.first; pp = (SPair) pp.rest;
	r5 = pp.first; pp = (SPair) pp.rest;
	r6 = pp.first; pp = (SPair) pp.rest;
	r7 = pp.first; pp = (SPair) pp.rest;
	r8 = pp.first; pp = (SPair) pp.rest;
	r9 = pp.first; pp = (SPair) pp.rest;
	r10 = pp.first; pp = (SPair) pp.rest;
	r11 = pp.first; pp = (SPair) pp.rest;
	r12 = pp.first; pp = (SPair) pp.rest;
	r13 = pp.first; pp = (SPair) pp.rest;
	r14 = pp.first; pp = (SPair) pp.rest;
	r15 = pp.first; pp = (SPair) pp.rest;
	r16 = pp.first; pp = (SPair) pp.rest;
	r17 = pp.first; pp = (SPair) pp.rest;
	r18 = pp.first; pp = (SPair) pp.rest;
	r19 = pp.first; pp = (SPair) pp.rest;
	r20 = pp.first; pp = (SPair) pp.rest;
	r21 = pp.first; pp = (SPair) pp.rest;
	r22 = pp.first; pp = (SPair) pp.rest;
	r23 = pp.first; pp = (SPair) pp.rest;
	r24 = pp.first; pp = (SPair) pp.rest;
	r25 = pp.first; pp = (SPair) pp.rest;
	r26 = pp.first; pp = (SPair) pp.rest;
	r27 = pp.first; pp = (SPair) pp.rest;
	r28 = pp.first; pp = (SPair) pp.rest;
	r29 = pp.first; pp = (SPair) pp.rest;
	r30 = pp.first; return;

    default:
	Exn.internalError("spreadRegister: register index out of bounds: " + nregs);
	return;
        }
  }

  // Used by LAMBDA and LEXES instructions to create an environment.

  public static SObject[] Close (int n)
  {
    switch (n) {
    case 0:
	return new SObject [] {r0};
    case 1:
	return new SObject [] {r0,
			       r1};
    case 2:
	return new SObject [] {r0,
			       r1,
			       r2};
    case 3:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3};
    case 4:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4};
    case 5:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5};
    case 6:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6};
    case 7:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7};
    case 8:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8};
    case 9:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9};
    case 10:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10};
    case 11:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11};
    case 12:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12};
    case 13:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13};
    case 14:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14};
    case 15:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15};
    case 16:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16};
    case 17:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17};
    case 18:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18};
    case 19:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19};
    case 20:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19,
			       r20};
    case 21:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19,
			       r20,
			       r21};
    case 22:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19,
			       r20,
			       r21,
			       r22};
    case 23:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19,
			       r20,
			       r21,
			       r22,
			       r23};
    case 24:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19,
			       r20,
			       r21,
			       r22,
			       r23,
			       r24};
    case 25:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19,
			       r20,
			       r21,
			       r22,
			       r23,
			       r24,
			       r25};
    case 26:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19,
			       r20,
			       r21,
			       r22,
			       r23,
			       r24,
			       r25,
			       r26};
    case 27:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19,
			       r20,
			       r21,
			       r22,
			       r23,
			       r24,
			       r25,
			       r26,
			       r27};
    case 28:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19,
			       r20,
			       r21,
			       r22,
			       r23,
			       r24,
			       r25,
			       r26,
			       r27,
			       r28};
    case 29:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19,
			       r20,
			       r21,
			       r22,
			       r23,
			       r24,
			       r25,
			       r26,
			       r27,
			       r28,
			       r29};
    case 30:
	return new SObject [] {r0,
			       r1,
			       r2,
			       r3,
			       r4,
			       r5,
			       r6,
			       r7,
			       r8,
			       r9,
			       r10,
			       r11,
			       r12,
			       r13,
			       r14,
			       r15,
			       r16,
			       r17,
			       r18,
			       r19,
			       r20,
			       r21,
			       r22,
			       r23,
			       r24,
			       r25,
			       r26,
			       r27,
			       r28,
			       r29,
			       r30};
    default:
	Exn.internalError("Close: register index out of bounds: " + n);
	return new SObject []{};
	}
  }

  public static void clearRegisters()
  {
    Reg.Result = Factory.Undefined;
    Reg.Second = Factory.Undefined;
    Reg.Third = Factory.Undefined;
    // We don't clear register0, but that always holds the current procedure.
    for (int i = 1; i < Reg.NREGS; ++i) {
	Reg.setRegister (i, Factory.Undefined);
        }
  }
}
}

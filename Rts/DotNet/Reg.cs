using System;
using System.Collections;
using Scheme.Rep;

namespace Scheme.RT {

public class Reg {

    // If you change the number of registers, you must change
    // getRegister, setRegister, and spreadRegister in this file.
    // You must also change saveRegisters in ContinuationISH
    public const int NREGS = 32;
    public const int LASTREG = NREGS - 1;

    // General-Purpose Registers
    public static SObject register0;
    public static SObject register1;
    public static SObject register2;
    public static SObject register3;
    public static SObject register4;
    public static SObject register5;
    public static SObject register6;
    public static SObject register7;
    public static SObject register8;
    public static SObject register9;
    public static SObject register10;
    public static SObject register11;
    public static SObject register12;
    public static SObject register13;
    public static SObject register14;
    public static SObject register15;
    public static SObject register16;
    public static SObject register17;
    public static SObject register18;
    public static SObject register19;
    public static SObject register20;
    public static SObject register21;
    public static SObject register22;
    public static SObject register23;
    public static SObject register24;
    public static SObject register25;
    public static SObject register26;
    public static SObject register27;
    public static SObject register28;
    public static SObject register29;
    public static SObject register30;
    public static SObject register31;

    public static SObject Result;
    public static SObject Second;
    public static SObject Third;

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
    public static Hashtable globals = new Hashtable();

    /* ===================================================== */
    /*   Support Code                                        */
    /* ===================================================== */

    static Reg()
    {
        Timer.mainTimer.reset();
    }

    public static SPair globalCell (string identifier)
    {
        SPair cell = (SPair) globals[identifier];
        if (cell == null) {
            cell = new SPair(Factory.Undefined,
                             Factory.internSymbol(identifier));
            globals[identifier] = cell;
        }
        return cell;
    }

    public static SObject globalValue(string identifier)
    {
        return globalCell(identifier).first;
    }

    public static SObject getRegister (int index)
    {
        switch (index) {
          case 0:  return Reg.register0;
          case 1:  return Reg.register1;
          case 2:  return Reg.register2;
          case 3:  return Reg.register3;
          case 4:  return Reg.register4;
          case 5:  return Reg.register5;
          case 6:  return Reg.register6;
          case 7:  return Reg.register7;
          case 8:  return Reg.register8;
          case 9:  return Reg.register9;
          case 10: return Reg.register10;
          case 11: return Reg.register11;
          case 12: return Reg.register12;
          case 13: return Reg.register13;
          case 14: return Reg.register14;
          case 15: return Reg.register15;
          case 16: return Reg.register16;
          case 17: return Reg.register17;
          case 18: return Reg.register18;
          case 19: return Reg.register19;
          case 20: return Reg.register20;
          case 21: return Reg.register21;
          case 22: return Reg.register22;
          case 23: return Reg.register23;
          case 24: return Reg.register24;
          case 25: return Reg.register25;
          case 26: return Reg.register26;
          case 27: return Reg.register27;
          case 28: return Reg.register28;
          case 29: return Reg.register29;
          case 30: return Reg.register30;
          case 31: return Reg.register31;
          default:
              Exn.internalError("getRegister: register index out of bounds: " + index);
              return null;
        }
    }

    public static void setRegister (int index, SObject value)
    {
        switch (index) {
          case 0: Reg.register0 = value; break;
          case 1: Reg.register1 = value; break;
          case 2: Reg.register2 = value; break;
          case 3: Reg.register3 = value; break;
          case 4: Reg.register4 = value; break;
          case 5: Reg.register5 = value; break;
          case 6: Reg.register6 = value; break;
          case 7: Reg.register7 = value; break;
          case 8: Reg.register8 = value; break;
          case 9: Reg.register9 = value; break;
          case 10: Reg.register10 = value; break;
          case 11: Reg.register11 = value; break;
          case 12: Reg.register12 = value; break;
          case 13: Reg.register13 = value; break;
          case 14: Reg.register14 = value; break;
          case 15: Reg.register15 = value; break;
          case 16: Reg.register16 = value; break;
          case 17: Reg.register17 = value; break;
          case 18: Reg.register18 = value; break;
          case 19: Reg.register19 = value; break;
          case 20: Reg.register20 = value; break;
          case 21: Reg.register21 = value; break;
          case 22: Reg.register22 = value; break;
          case 23: Reg.register23 = value; break;
          case 24: Reg.register24 = value; break;
          case 25: Reg.register25 = value; break;
          case 26: Reg.register26 = value; break;
          case 27: Reg.register27 = value; break;
          case 28: Reg.register28 = value; break;
          case 29: Reg.register29 = value; break;
          case 30: Reg.register30 = value; break;
          case 31: Reg.register31 = value; break;
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
            case 0: pp = (SPair) Reg.register0; break;
            case 1: pp = (SPair) Reg.register1; break;
            case 2: pp = (SPair) Reg.register2; break;
            case 3: pp = (SPair) Reg.register3; break;
            case 4: pp = (SPair) Reg.register4; break;
            case 5: pp = (SPair) Reg.register5; break;
            case 6: pp = (SPair) Reg.register6; break;
            case 7: pp = (SPair) Reg.register7; break;
            case 8: pp = (SPair) Reg.register8; break;
            case 9: pp = (SPair) Reg.register9; break;
            case 10: pp = (SPair) Reg.register10; break;
            case 11: pp = (SPair) Reg.register11; break;
            case 12: pp = (SPair) Reg.register12; break;
            case 13: pp = (SPair) Reg.register13; break;
            case 14: pp = (SPair) Reg.register14; break;
            case 15: pp = (SPair) Reg.register15; break;
            case 16: pp = (SPair) Reg.register16; break;
            case 17: pp = (SPair) Reg.register17; break;
            case 18: pp = (SPair) Reg.register18; break;
            case 19: pp = (SPair) Reg.register19; break;
            case 20: pp = (SPair) Reg.register20; break;
            case 21: pp = (SPair) Reg.register21; break;
            case 22: pp = (SPair) Reg.register22; break;
            case 23: pp = (SPair) Reg.register23; break;
            case 24: pp = (SPair) Reg.register24; break;
            case 25: pp = (SPair) Reg.register25; break;
            case 26: pp = (SPair) Reg.register26; break;
            case 27: pp = (SPair) Reg.register27; break;
            case 28: pp = (SPair) Reg.register28; break;
            case 29: pp = (SPair) Reg.register29; break;
            case 30: pp = (SPair) Reg.register30; break;
            case 31: pp = (SPair) Reg.register31; break;
            default:
                Exn.internalError("spreadRegister: register index out of bounds: " + listreg);
                return;
        }

        switch (nregs) {
          case 1:
            Reg.register1 = pp.first; return;

          case 2:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; return;

          case 3:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; return;

          case 4:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; return;

          case 5:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; return;

          case 6:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; return;

          case 7:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; return;

          case 8:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; return;

          case 9:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; return;

          case 10:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; return;

          case 11:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; return;

          case 12:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; return;

          case 13:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; return;

          case 14:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; return;

          case 15:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; return;

          case 16:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; return;

          case 17:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; return;

          case 18:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; return;

          case 19:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; return;

          case 20:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; pp = (SPair) pp.rest;
            Reg.register20 = pp.first; return;

          case 21:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; pp = (SPair) pp.rest;
            Reg.register20 = pp.first; pp = (SPair) pp.rest;
            Reg.register21 = pp.first; return;

          case 22:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; pp = (SPair) pp.rest;
            Reg.register20 = pp.first; pp = (SPair) pp.rest;
            Reg.register21 = pp.first; pp = (SPair) pp.rest;
            Reg.register22 = pp.first; return;

          case 23:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; pp = (SPair) pp.rest;
            Reg.register20 = pp.first; pp = (SPair) pp.rest;
            Reg.register21 = pp.first; pp = (SPair) pp.rest;
            Reg.register22 = pp.first; pp = (SPair) pp.rest;
            Reg.register23 = pp.first; return;
          case 24:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; pp = (SPair) pp.rest;
            Reg.register20 = pp.first; pp = (SPair) pp.rest;
            Reg.register21 = pp.first; pp = (SPair) pp.rest;
            Reg.register22 = pp.first; pp = (SPair) pp.rest;
            Reg.register23 = pp.first; pp = (SPair) pp.rest;
            Reg.register24 = pp.first; return;

          case 25:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; pp = (SPair) pp.rest;
            Reg.register20 = pp.first; pp = (SPair) pp.rest;
            Reg.register21 = pp.first; pp = (SPair) pp.rest;
            Reg.register22 = pp.first; pp = (SPair) pp.rest;
            Reg.register23 = pp.first; pp = (SPair) pp.rest;
            Reg.register24 = pp.first; pp = (SPair) pp.rest;
            Reg.register25 = pp.first; return;

          case 26:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; pp = (SPair) pp.rest;
            Reg.register20 = pp.first; pp = (SPair) pp.rest;
            Reg.register21 = pp.first; pp = (SPair) pp.rest;
            Reg.register22 = pp.first; pp = (SPair) pp.rest;
            Reg.register23 = pp.first; pp = (SPair) pp.rest;
            Reg.register24 = pp.first; pp = (SPair) pp.rest;
            Reg.register25 = pp.first; pp = (SPair) pp.rest;
            Reg.register26 = pp.first; return;

          case 27:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; pp = (SPair) pp.rest;
            Reg.register20 = pp.first; pp = (SPair) pp.rest;
            Reg.register21 = pp.first; pp = (SPair) pp.rest;
            Reg.register22 = pp.first; pp = (SPair) pp.rest;
            Reg.register23 = pp.first; pp = (SPair) pp.rest;
            Reg.register24 = pp.first; pp = (SPair) pp.rest;
            Reg.register25 = pp.first; pp = (SPair) pp.rest;
            Reg.register26 = pp.first; pp = (SPair) pp.rest;
            Reg.register27 = pp.first; return;

          case 28:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; pp = (SPair) pp.rest;
            Reg.register20 = pp.first; pp = (SPair) pp.rest;
            Reg.register21 = pp.first; pp = (SPair) pp.rest;
            Reg.register22 = pp.first; pp = (SPair) pp.rest;
            Reg.register23 = pp.first; pp = (SPair) pp.rest;
            Reg.register24 = pp.first; pp = (SPair) pp.rest;
            Reg.register25 = pp.first; pp = (SPair) pp.rest;
            Reg.register26 = pp.first; pp = (SPair) pp.rest;
            Reg.register27 = pp.first; pp = (SPair) pp.rest;
            Reg.register28 = pp.first; return;

          case 29:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; pp = (SPair) pp.rest;
            Reg.register20 = pp.first; pp = (SPair) pp.rest;
            Reg.register21 = pp.first; pp = (SPair) pp.rest;
            Reg.register22 = pp.first; pp = (SPair) pp.rest;
            Reg.register23 = pp.first; pp = (SPair) pp.rest;
            Reg.register24 = pp.first; pp = (SPair) pp.rest;
            Reg.register25 = pp.first; pp = (SPair) pp.rest;
            Reg.register26 = pp.first; pp = (SPair) pp.rest;
            Reg.register27 = pp.first; pp = (SPair) pp.rest;
            Reg.register28 = pp.first; pp = (SPair) pp.rest;
            Reg.register29 = pp.first; return;

          case 30:
            Reg.register1 = pp.first; pp = (SPair) pp.rest;
            Reg.register2 = pp.first; pp = (SPair) pp.rest;
            Reg.register3 = pp.first; pp = (SPair) pp.rest;
            Reg.register4 = pp.first; pp = (SPair) pp.rest;
            Reg.register5 = pp.first; pp = (SPair) pp.rest;
            Reg.register6 = pp.first; pp = (SPair) pp.rest;
            Reg.register7 = pp.first; pp = (SPair) pp.rest;
            Reg.register8 = pp.first; pp = (SPair) pp.rest;
            Reg.register9 = pp.first; pp = (SPair) pp.rest;
            Reg.register10 = pp.first; pp = (SPair) pp.rest;
            Reg.register11 = pp.first; pp = (SPair) pp.rest;
            Reg.register12 = pp.first; pp = (SPair) pp.rest;
            Reg.register13 = pp.first; pp = (SPair) pp.rest;
            Reg.register14 = pp.first; pp = (SPair) pp.rest;
            Reg.register15 = pp.first; pp = (SPair) pp.rest;
            Reg.register16 = pp.first; pp = (SPair) pp.rest;
            Reg.register17 = pp.first; pp = (SPair) pp.rest;
            Reg.register18 = pp.first; pp = (SPair) pp.rest;
            Reg.register19 = pp.first; pp = (SPair) pp.rest;
            Reg.register20 = pp.first; pp = (SPair) pp.rest;
            Reg.register21 = pp.first; pp = (SPair) pp.rest;
            Reg.register22 = pp.first; pp = (SPair) pp.rest;
            Reg.register23 = pp.first; pp = (SPair) pp.rest;
            Reg.register24 = pp.first; pp = (SPair) pp.rest;
            Reg.register25 = pp.first; pp = (SPair) pp.rest;
            Reg.register26 = pp.first; pp = (SPair) pp.rest;
            Reg.register27 = pp.first; pp = (SPair) pp.rest;
            Reg.register28 = pp.first; pp = (SPair) pp.rest;
            Reg.register29 = pp.first; pp = (SPair) pp.rest;
            Reg.register30 = pp.first; return;

          default:
              Exn.internalError("spreadRegister: register index out of bounds: " + nregs);
              return;
        }
        }

    public static void clearRegisters() {
        Reg.Result = Factory.Undefined;
        Reg.Second = Factory.Undefined;
        Reg.Third = Factory.Undefined;
        for (int i = 0; i < Reg.NREGS; ++i) {
            Reg.setRegister(i, Factory.Undefined);
        }
    }
}
}

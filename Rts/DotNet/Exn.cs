using System;
using System.Collections;
using System.IO;
using System.Diagnostics;
using Scheme.Rep;

namespace Scheme.RT {

    public class Exn {
        
        public static readonly Hashtable namespaces = new Hashtable();
        public static void registerNamespace(string ns, string source) {
            namespaces[ns] = source;
        }

        public static TextWriter msg = System.Console.Error;
        public static readonly TextWriter debug = System.Console.Error;

        public static void internalError(string message) {
            throw new Exception("Internal Error:" + message);
        }
        
        /** fault
         * The basic interface to the exception system. Excode is a code
         * defined in Rts/except.cfg. A fault may be but is not necessarily 
         * continuable.
         * Exception codes are defined in Constants.cs, created by csharp-config.scm
         */
        public static void fault(int excode) {
            fault(excode, null);
        }
        public static void fault(int blame, string m, SObject arg) {
            Reg.Result = arg;
            Exn.fault(blame, m);
        }
        public static void fault(int blame, string m, SObject arg1, SObject arg2) {
            Reg.Result = arg1;
            Reg.Second = arg2;
            Exn.fault(blame, m);
        }
        public static void fault(int blame, string m, 
                                 SObject arg1, SObject arg2, SObject arg3) {
            Reg.Result = arg1;
            Reg.Second = arg2;
            Reg.Third = arg3;
            Exn.fault(blame, m);
        }

        /** fault
         * Like fault, above, but more user-readable information
         * A compromise for now.
         */
        public static void fault(int excode, string message) {
            Procedure p = (Procedure) Reg.register0;
            msg.WriteLine("** exn {0}: {1}", excode, message);
            if (excode == -1) {
                throw new Exception("exn at " + p);
            }
            Call.callExceptionHandler(Reg.Result, Reg.Second, Reg.Third, (int)excode);
        }

        /** faultTimer
         * Specialized fault for timer interrupt.
         * Called directly from IL.
         */
        public static void faultTimer(int j) {
            checkSignals();
            Cont.clear();

            if (Reg.interruptsEnabled) {
                Reg.interruptsEnabled = false;
                Reg.implicitContinuation = j;
                Call.callInterruptHandler(Constants.EX_TIMER);
                return;
            } else {
                //debug.WriteLine("Unhandled timer interrupt");
                Reg.timer = Reg.SMALL_TIME_SLICE;
                Procedure p = (Procedure)Reg.getRegister(0);
                throw new CodeVectorCallException(p.entrypoint, j);
            }
        }

        /** checkSignals
         * Check for keyboard interrupt or other signal
         */
        public static void checkSignals() {
            // ?? FIXME
        }

        /* Reporting Framework
         * These methods are called to report machine state when exception 
         * occurs and also at end of execution
         */
        public static void dumpSpecialRegisters(bool includeReg0) {
            msg.WriteLine("*PC*    = {0}", Reg.debugLocation);
            msg.WriteLine("*FILE*  = {0}", Reg.debugFile);
            msg.WriteLine("Result  = {0}", Reg.Result);
            msg.WriteLine("Second  = {0}", Reg.Second);
            msg.WriteLine("Third   = {0}", Reg.Third);
            if (includeReg0) {
                msg.WriteLine("register 0 = {0}", Reg.getRegister(0));
            }
        }
        public static void dumpGeneralRegisters() {
            for (int i = 0; i < Reg.NREGS; ++i) {
                msg.WriteLine("register {0} = {1}", i, Reg.getRegister(i));
            }
        }
        public static void dumpEnvironment() {
            msg.WriteLine("Static link/Environment:");
            Procedure p = (Procedure)Reg.getRegister(0);
            if (p.rib != null) {
                for (int i = 0; i < p.rib.Length; ++i) {
                    msg.WriteLine("  lexical(0,{0}) = {1}", i, p.rib[i]);
                }
            } else {
                msg.WriteLine("  no environment");
            }
            msg.WriteLine();
        }
        public static void dumpContinuation() {
            msg.WriteLine("Continuation:");
            SObject sframe = Cont.getCC();
            int i = 0;
            while (sframe is SVL) {
                SVL frame = (SVL) sframe;
                dumpFrame(frame.elements, i);
                sframe = frame.elements[Cont.HC_DYNLINK];
                i++;
            }
            msg.WriteLine("  Continuation ended with {0}", sframe);
            msg.WriteLine();
        }
        
        public static void dumpFrame(SObject[] frame, int i) {
            if (frame[Cont.HC_PROC] == RestoreContextCode.singletonProcedure) {
                msg.WriteLine("  Saved Context Frame {0}", i);
                msg.WriteLine("    return point {0}", frame[Cont.HC_RETOFFSET]);
                for (int reg = 0; reg < Reg.NREGS; ++reg) {
                    msg.WriteLine("    saved register {0} = {1}", reg, frame[Cont.HC_CONTEXT_REG0 + reg]);
                }
                msg.WriteLine("    saved Result = {0}", frame[Cont.HC_CONTEXT_RESULT]);
                msg.WriteLine("    restore Result? = {0}", frame[Cont.HC_CONTEXT_RESTORE_RESULT]);
            } else {
                msg.WriteLine("  Frame {0}", i);
                msg.WriteLine("    return point jump index {0}", frame[Cont.HC_RETOFFSET]);
                for (int slot = 0; slot < frame.Length - Cont.HC_OVERHEAD; slot++) {
                    msg.WriteLine("    slot {0} = {1}",
                        slot, frame[Cont.HC_OVERHEAD + slot]);
                }
            }
        }
        public static void dumpNativeStack(Exception e) {
            msg.WriteLine("Native Stack:");
            StackTrace stack = new StackTrace(e);
            for (int i = 0; i < stack.FrameCount; ++i) {
                StackFrame frame = stack.GetFrame(i);
                msg.WriteLine("  Frame {0}", i);
                msg.WriteLine("    method {0}::{1}, offset {2}",
                              frame.GetMethod().DeclaringType,
                              frame.GetMethod().Name,
                              frame.GetILOffset());
                msg.WriteLine("    list file {0}, offset {1}",
                              frame.GetFileName(), frame.GetFileLineNumber());
            }
            msg.WriteLine();
        }
        public static void dumpTotalTime() {
            msg.WriteLine("Total time (s) = {0:f3}",
                          (double)Timer.mainTimer.getElapsedTime() / 1000000.0);
        }

        // :-)
        public static void miniCoreDump(Timer timer) {
            msg.WriteLine("Time for program (s) = {0:f3}", 
                          (double)timer.getElapsedTime() / 1000000.0);
        }
        public static void coredump() {
            dumpSpecialRegisters(true);
            dumpTotalTime();
        }
        public static void fullCoreDump() {
            fullCoreDump(null);
        }
        public static void fullCoreDump(Exception e) {
            TextWriter oldmsg = msg;
            msg = new StreamWriter(File.Open("CORE-LARCENYCS", FileMode.Create));
            dumpSpecialRegisters(false);
            dumpGeneralRegisters();
            dumpEnvironment();
            dumpContinuation();
            if (e != null) {
                dumpNativeStack(e);
            }
            msg.Flush();
            msg.Close();
            msg = oldmsg;
            msg.WriteLine("Core dumped (CORE-LARCENYCS)");
        }
    }

    public class Timer {
        public static readonly Timer mainTimer;
        static Timer() {
            mainTimer = new Timer();
            mainTimer.reset();
        }

        public Timer() {
            this.reset();
        }
    
        // Start time in microseconds
        private long startTime = 0;

        public void reset() {
            startTime = DateTime.Now.Ticks / 10; // :-)
        }
    
        // time in microseconds
        public long getElapsedTime() {
            long stopTime = DateTime.Now.Ticks / 10; // :-)
            return stopTime - startTime;
        }
    }

}


using System;
using System.Collections;
using System.IO;
using System.Diagnostics;
using Scheme.Rep;

namespace Scheme.RT {

public class Exn {

  // About 100 namespaces.  Size estimate here does not have to
  // be correct.
  public static readonly Hashtable namespaces = new Hashtable (100);

  public static void registerNamespace (string ns, string source)
  {
    namespaces[ns] = source;
  }

  public static TextWriter msg = System.Console.Error;
  public static readonly TextWriter debug = System.Console.Error;
  // new StreamWriter (File.Open ("CORE-DEBUG", FileMode.Create));

  public static void internalError (string message)
  {
    throw new Exception ("Internal Error:" + message);
  }

  /** fault
   * The basic interface to the exception system. Excode is a code
   * defined in Rts/except.cfg. A fault may be but is not necessarily
   * continuable.
   * Exception codes are defined in Constants.cs, created by csharp-config.scm
   */
  public static CodeAddress fault (int excode)
  {
    return fault (excode, null);
  }

  public static CodeAddress fault (int blame, string m, SObject arg)
  {
    Reg.Result = arg;
    return Exn.fault (blame, m);
  }

  public static CodeAddress fault (int blame, string m, SObject arg1, SObject arg2)
  {
    Reg.Result = arg1;
    Reg.Second = arg2;
    return Exn.fault (blame, m);
  }

  public static CodeAddress fault (int blame, string m,
				   SObject arg1, SObject arg2, SObject arg3)
  {
    Reg.Result = arg1;
    Reg.Second = arg2;
    Reg.Third = arg3;
    return Exn.fault (blame, m);
  }

  /** fault
   * Like fault, above, but more user-readable information
   * A compromise for now.
   */
  public static CodeAddress fault (int excode, string message)
  {
    Procedure p = Reg.ProcRegister0;
    //msg.WriteLine ("** exn {0}: {1}", excode, message);
    if (excode == -1) {
	throw new Exception ("exn at " + p);
	}
    return Call.callExceptionHandler (Reg.Result, Reg.Second, Reg.Third, (int)excode);
  }

  /** faultTimer
   * Specialized fault for timer interrupt.
   * Called directly from IL.
   */
  public static void faultTimer (int j)
  {
    checkSignals();
    Cont.clear();

    //Exn.debug.WriteLine ("#timer {2} @ {0} @ {1}", Reg.Register0, j, count);
    if (Reg.interruptsEnabled) {
	Reg.interruptsEnabled = false;
	Reg.implicitContinuation = j;
	Call.callInterruptHandler (Constants.EX_TIMER);
	return;
	}
    else {
	Reg.timer = Reg.SMALL_TIME_SLICE;
	Procedure p = Reg.ProcRegister0;
	throw new CodeVectorCallException (p.entrypoint, j);
	}
  }

  public static CodeAddress faultGlobal (int global)
  {
    Reg.Result = Reg.ProcRegister0.constants[global];
    return fault (Constants.EX_UNDEF_GLOBAL);
  }

  public static CodeAddress faultArgCount (int expectedc)
  {
    Reg.Second = Factory.makeNumber (expectedc);
    Reg.Third = Reg.Register0;
    return fault (Constants.EX_ARGSEQ);
  }

  public static CodeAddress faultVarArgCount (int expectedc)
  {
    Reg.Second = Factory.makeNumber (expectedc);
    Reg.Third = Reg.Register0;
    return fault (Constants.EX_ARGSGE);
  }

  public static CodeAddress faultInvokeNonProc (int argc)
  {
    return fault (Constants.EX_NONPROC);
  }

  public static CodeAddress faultApplyNonProc (int k1, int k2)
  {
    Reg.Second = Reg.getRegister (k1);
    return fault (Constants.EX_APPLY);
  }

  private static System.ConsoleCancelEventHandler installed_handler = 
    new System.ConsoleCancelEventHandler(myHandler);

  static Exn() {
    System.Console.CancelKeyPress += installed_handler;
    try {
      // System.Console.TreatControlCAsInput = true;
    } catch (IOException) {
      /* when you run under Emacs shell, you can't set the above
       * without throwing an IO exception.  This is not a fatal
       * situation for us, so ignore the exception. */
      Console.WriteLine("NOTE: Could not TreatControlCAsInput");
    }
  }

  /** checkSignals
   * Check for keyboard interrupt or other signal
   */
  public static void checkSignals()
  {
    // Console.WriteLine("Checking Signals, woo!");
    bool mark_seen_it = saw_keyboard_interrupt;
    saw_keyboard_interrupt = false;
    if (mark_seen_it)
      Call.callInterruptHandler (Constants.EX_KBDINTR);
  }
  private static bool saw_keyboard_interrupt = false;
  private static void myHandler(object sender, 
				System.ConsoleCancelEventArgs args) {
// Announce that the event handler has been invoked.
    Console.WriteLine("\nThe read operation has been interrupted.");

// Announce which key combination was pressed.
    Console.WriteLine("  Key pressed: {0}", args.SpecialKey);

// Announce the initial value of the Cancel property.
    Console.WriteLine("  Cancel property: {0}", args.Cancel);

// Set the Cancel property to true to prevent the process from terminating.
    Console.WriteLine("Setting the Cancel property to true...");
    args.Cancel = true;

// Announce the new value of the Cancel property.
    Console.WriteLine("  Cancel property: {0}", args.Cancel);
    Console.WriteLine("The read operation will resume...\n");

    saw_keyboard_interrupt = true;
  }

  public static CodeAddress error (string msg)
  {
    debug.WriteLine ("** called Exn.error (string) " + msg);
    SObject string0 = Factory.makeUString ("");
    return Call.callExceptionHandler
	(new SObject[] {Factory.makeUString (msg), string0, string0, Factory.Null});
  }

  public static CodeAddress error (string msg, SObject value)
  {
    debug.WriteLine ("** called Exn.error (string) " + msg);
    return Call.callExceptionHandler
	(new SObject[] {Factory.makeUString (msg), value, Factory.False, Factory.Null});
  }

  // =================================================

  /* Reporting Framework
   * These methods are called to report machine state when exception
   * occurs and also at end of execution
   */

  public static void dumpSpecialRegisters (bool includeReg0)
  {
    msg.WriteLine ("*PC*    = {0}", Reg.debugLocation);
    msg.WriteLine ("*FILE*  = {0}", Reg.debugFile);
    msg.WriteLine ("Result  = {0}", Reg.Result);
    msg.WriteLine ("Second  = {0}", Reg.Second);
    msg.WriteLine ("Third   = {0}", Reg.Third);
    if (includeReg0) {
	msg.WriteLine ("register 0 = {0}", Reg.Register0);
	}
  }
  public static void dumpGeneralRegisters()
  {
    for (int i = 0; i < Reg.NREGS; ++i) {
	msg.WriteLine ("register {0} = {1}", i, Reg.getRegister (i));
	}
  }

  public static void dumpEnvironment()
  {
    msg.WriteLine ("Static link/Environment:");
    Procedure p = Reg.ProcRegister0;
    if (p.rib != null) {
	for (int i = 0; i < p.rib.Length; ++i) {
	    msg.WriteLine ("  lexical (0,{0}) = {1}", i, p.rib[i]);
	    }
	} else {
	msg.WriteLine ("  no environment");
	}
    msg.WriteLine();
  }

  public static void dumpContinuation()
  {
    msg.WriteLine ("Continuation:");
    SObject sframe = Cont.getCC();
    int i = 0;
    while (sframe is SVL) {
	SVL frame = (SVL) sframe;
	dumpFrame (frame.elements, i);
	sframe = frame.elements[Cont.HC_DYNLINK];
	i++;
	}
    msg.WriteLine ("  Continuation ended with {0}", sframe);
    msg.WriteLine();
  }

  public static void dumpFrame (SObject[] frame, int i)
  {
    if (frame[Cont.HC_PROC] == RestoreContextCode.singletonProcedure) {
	msg.WriteLine ("  Saved Context Frame {0}", i);
	msg.WriteLine ("    return point {0}", frame[Cont.HC_RETOFFSET]);
	for (int reg = 0; reg < Reg.NREGS; ++reg) {
	    msg.WriteLine ("    saved register {0} = {1}", reg, frame[Cont.HC_CONTEXT_REG0 + reg]);
	    }
	msg.WriteLine ("    saved Result = {0}", frame[Cont.HC_CONTEXT_RESULT]);
	msg.WriteLine ("    restore Result? = {0}", frame[Cont.HC_CONTEXT_RESTORE_RESULT]);
	}
    else {
	msg.WriteLine ("  Frame {0}", i);
	msg.WriteLine ("    return point jump index {0}", frame[Cont.HC_RETOFFSET]);
	for (int slot = 0; slot < frame.Length - Cont.HC_OVERHEAD; slot++) {
	    msg.WriteLine ("    slot {0} = {1}",
			  slot, frame[Cont.HC_OVERHEAD + slot]);
	    }
	}
  }

  public static void dumpNativeStack (Exception e)
  {
    msg.WriteLine ("Native Stack:");
    StackTrace stack = new StackTrace (e);
    for (int i = 0; i < stack.FrameCount; ++i) {
	StackFrame frame = stack.GetFrame (i);
	msg.WriteLine ("  Frame {0}", i);
	msg.WriteLine ("    method {0}::{1}, offset {2}",
		      frame.GetMethod().DeclaringType,
		      frame.GetMethod().Name,
		      frame.GetILOffset());
	msg.WriteLine ("    list file {0}, offset {1}",
		      frame.GetFileName(), frame.GetFileLineNumber());
	}
    msg.WriteLine();
  }

  public static void dumpTotalTime()
  {
    msg.WriteLine ("Total time (s) = {0:f3}",
		  (double)Timer.mainTimer.getElapsedTime() / 1000000.0);
  }

  // :-)
  public static void miniCoreDump (Timer timer)
  {
    msg.WriteLine ("Time for program (s) = {0:f3}",
		  (double)timer.getElapsedTime() / 1000000.0);
  }

  public static void coredump()
  {
    dumpSpecialRegisters (true);
    dumpTotalTime();
  }

  public static void fullCoreDump()
  {
    fullCoreDump (null);
  }

  public static void fullCoreDump (Exception e)
  {
    TextWriter oldmsg = msg;
    msg = new StreamWriter (File.Open ("CORE-LARCENYCS", FileMode.Create));
    dumpSpecialRegisters (false);
    dumpGeneralRegisters();
    dumpEnvironment();
    dumpContinuation();
    if (e != null) {
	dumpNativeStack (e);
	}
    msg.Flush();
    msg.Close();
    msg = oldmsg;
    msg.WriteLine ("Core dumped (CORE-LARCENYCS)");
  }
}

public class Timer {
  public static readonly Timer mainTimer;

  static Timer()
  {
    mainTimer = new Timer();
    mainTimer.reset();
  }

  public Timer()
  {
    this.reset();
  }

  // Start time in microseconds
  private long startTime = 0;

  public void reset()
  {
    startTime = DateTime.Now.Ticks / 10; // :-)
  }

  // time in microseconds
  public long getElapsedTime()
  {
    long stopTime = DateTime.Now.Ticks / 10; // :-)
    return stopTime - startTime;
  }
}
}

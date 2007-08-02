using System; // for Exception
using System.Diagnostics;

using Scheme.Rep;

namespace Scheme.RT {

    // Uses exception classes defined in Exn

    public class Call {
#if HAS_PERFORMANCE_COUNTERS
        public static PerformanceCounter applySetupCounter;
        public static PerformanceCounter bounceCounter;
        public static PerformanceCounter schemeCallCounter;
        public static PerformanceCounter millicodeSupportCallCounter;
#endif

        /* Utility Methods */

        /** apply_setup
         * Unpacks arguments to Procedure in Result from list
         * Pre: Result contains Procedure; register k1 a list;
         *      register k2 the length of that list (fixnum)
         * Returns number of arguments
         * Called by inlined IL of apply
         */
        public static int applySetup(int k1, int k2) {
#if HAS_PERFORMANCE_COUNTERS
            if (applySetupCounter != null) applySetupCounter.Increment();
#endif
            int n = ((SFixnum)Reg.getRegister(k2)).value;
            if (n < Reg.NREGS-1) {
                // Load registers 1 through n with elts of list in k1
                Reg.spreadRegister (k1, n);
//                SObject p = Reg.getRegister(k1);
//                for (int i = 1; i <= n; ++i) {
//                    SPair pp = (SPair) p;
//                    Reg.setRegister(i, pp.getFirst());
//                    p = pp.getRest();
//                }
            } else {
                // Load registers 1 to NREGS-2 with first NREGS-2 elts of
                // list in k1 and put remaining tail in register NREG-1
                SPair p = (SPair)Reg.getRegister(k1);

                for (int i = 1; i <= Reg.NREGS-2; ++i) {
                    Reg.setRegister(i, p.getFirst());
                    p = (SPair)p.getRest();
                }
                Reg.setRegister(Reg.NREGS-1, p);
            }
            return n;
        }

        // call
      public static CodeAddress call1 (Procedure p, int argc)
        {
#if HAS_PERFORMANCE_COUNTERS
           if (Call.schemeCallCounter != null) Call.schemeCallCounter.Increment();
#endif
           throw new SchemeCallException (p, argc);
        }

        public static CodeAddress call (CodeVector code, int jumpIndex)
        {
#if HAS_PERFORMANCE_COUNTERS
           if (Call.bounceCounter != null) Call.bounceCounter.Increment();
#endif
#if ALWAYS_THROW_TO_TRAMPOLINE
           throw new CodeVectorCallException (code, jumpIndex);
#else
           return code.Address (jumpIndex);
#endif
        }

        public static CodeAddress call0 (Procedure p)
        {
#if HAS_PERFORMANCE_COUNTERS
           if (Call.schemeCallCounter != null) Call.schemeCallCounter.Increment();
#endif
#if ALWAYS_THROW_TO_TRAMPOLINE
           throw new SchemeCallException (p, 0);
#else
           Reg.Second = Reg.Register0;
           Reg.Register0 = p;
           Reg.Result = SFixnum.zero;
           return p.InitialCodeAddress;
#endif
        }

        public static CodeAddress call1 (Procedure p)
        {
#if HAS_PERFORMANCE_COUNTERS
           if (Call.schemeCallCounter != null) Call.schemeCallCounter.Increment();
#endif
#if ALWAYS_THROW_TO_TRAMPOLINE
           throw new SchemeCallException (p, 1);
#else
           Reg.Second = Reg.Register0;
           Reg.Register0 = p;
           Reg.Result = SFixnum.one;
           return p.InitialCodeAddress;
#endif
        }

        public static CodeAddress call2 (Procedure p)
        {
#if HAS_PERFORMANCE_COUNTERS
           if (Call.schemeCallCounter != null) Call.schemeCallCounter.Increment();
#endif
#if ALWAYS_THROW_TO_TRAMPOLINE
           throw new SchemeCallException (p, 2);
#else
           Reg.Second = Reg.Register0;
           Reg.Register0 = p;
           Reg.Result = SFixnum.two;
           return p.InitialCodeAddress;
#endif
        }

        public static CodeAddress call3 (Procedure p)
        {
#if HAS_PERFORMANCE_COUNTERS
           if (Call.schemeCallCounter != null) Call.schemeCallCounter.Increment();
#endif
#if ALWAYS_THROW_TO_TRAMPOLINE
           throw new SchemeCallException (p, argc);
#else
           Reg.Second = Reg.Register0;
           Reg.Register0 = p;
           Reg.Result = SFixnum.three;
           return p.InitialCodeAddress;
#endif
        }

        public static CodeAddress call (Procedure p, int argc)
        {
#if HAS_PERFORMANCE_COUNTERS
           if (Call.schemeCallCounter != null) Call.schemeCallCounter.Increment();
#endif
#if ALWAYS_THROW_TO_TRAMPOLINE
           throw new SchemeCallException (p, argc);
#else
           Reg.Second = Reg.Register0;
           Reg.Register0 = p;
           Reg.Result = Factory.makeFixnum (argc);
           return p.InitialCodeAddress;
#endif
        }

        // callback
        // For when .NET calls back into Scheme.
        public static SObject callback (Procedure p) {
            SObject result;
            SObject cont = Cont.getCC();
            try {
                Cont.reset();
                trampoline (p, 0);
                result = Reg.Result;
            }
            finally {
                Cont.setCC (cont);
            }
            return result;
        }

        public static SObject callback (Procedure p, SObject arg0) {
            SObject result;
            SObject cont = Cont.getCC();
            try {
                Cont.reset();
                Reg.setRegister (1, arg0);
                trampoline (p, 1);
                result = Reg.Result;
            }
            finally {
                Cont.setCC (cont);
            }
            return result;
        }

        public static SObject callback (Procedure p, SObject arg0, SObject arg1) {
            SObject result;
            SObject cont = Cont.getCC();
            try {
                Cont.reset();
                Reg.setRegister (1, arg0);
                Reg.setRegister (2, arg1);
                trampoline (p, 2);
                result = Reg.Result;
            }
            finally {
                Cont.setCC (cont);
            }
            return result;
        }

        public static SObject callback (Procedure p, SObject arg0, SObject arg1, SObject arg2) {
            SObject result;
            SObject cont = Cont.getCC();
            try {
                Cont.reset();
                Reg.setRegister (1, arg0);
                Reg.setRegister (2, arg1);
                Reg.setRegister (3, arg2);
                trampoline (p, 3);
                result = Reg.Result;
            }
            finally {
                Cont.setCC (cont);
            }
            return result;
        }

        public static SObject callback (Procedure p, SObject arg0, SObject arg1, SObject arg2, SObject arg3) {
            SObject result;
            SObject cont = Cont.getCC();
            try {
                Cont.reset();
                Reg.setRegister (1, arg0);
                Reg.setRegister (2, arg1);
                Reg.setRegister (3, arg2);
                Reg.setRegister (4, arg3);
                trampoline (p, 4);
                result = Reg.Result;
            }
            finally {
                Cont.setCC (cont);
            }
            return result;
        }

      public static void trampoline (Procedure p, int argc)
      {
        Reg.Register0 = p;
        Reg.Result = Factory.makeFixnum (argc);

        CodeAddress pc = p.InitialCodeAddress;
        bool pending = true;

        while (pending) {
            pending = false;
            try {
                do {
                    pc = pc.code.call (pc.label);
                    } while (pc != null);
                }
            catch (BounceException bcve) {
                bcve.prepareForBounce();
                pc = bcve.target;
                pending = true;
                }
            }
      }

        public static void contagion(SObject a, SObject b, SObject retry) {
            callMillicodeSupport3(Constants.MS_CONTAGION, a, b, retry);
        }
        public static void pcontagion(SObject a, SObject b, SObject retry) {
            callMillicodeSupport3(Constants.MS_PCONTAGION, a, b, retry);
        }
        public static void econtagion(SObject a, SObject b, SObject retry) {
            callMillicodeSupport3(Constants.MS_ECONTAGION, a, b, retry);
        }

        public static CodeAddress callExceptionHandler (SObject result, SObject second,
                                                   SObject third, int excode)
        {
            saveContext (false);
            Reg.setRegister (1, result);
            Reg.setRegister (2, second);
            Reg.setRegister (3, third);
            Reg.setRegister (4, Factory.makeNumber (excode));
            /*
            Console.WriteLine("{0} {1} {2} {3}", result, second,
                                                 third, excode);
            */
            return call1 (getSupportProcedure (Constants.MS_EXCEPTION_HANDLER), 4);
        }

        public static CodeAddress callExceptionHandler (SObject[] values)
        {
          saveContext(false);

          if (values.Length > Reg.LASTREG) {
              for (int ri = 1; ri < Reg.LASTREG; ++ri) {
                  Reg.setRegister (ri, values[ri-1]);
                  }
              Reg.setRegister (Reg.LASTREG, Factory.arrayToList (values, Reg.LASTREG - 1));
              }
          else {
              for (int ri = 1; ri < values.Length + 1; ++ri) {
                  Reg.setRegister (ri, values[ri-1]);
                  }
              }
          return call1 (getSupportProcedure (Constants.MS_EXCEPTION_HANDLER), values.Length);
        }

        public static void callInterruptHandler(int excode)
        {
            saveContext(true);
            Reg.setRegister(1, Factory.False);
            Reg.setRegister(2, Factory.False);
            Reg.setRegister(3, Factory.False);
            Reg.setRegister(4, Factory.makeNumber (excode));
            call1(getSupportProcedure(Constants.MS_EXCEPTION_HANDLER), 4);
        }

        public static void callMillicodeSupport3(int procIndex, SObject a,
                                                 SObject b, SObject c) {
#if HAS_PERFORMANCE_COUNTERS
            if (millicodeSupportCallCounter != null) millicodeSupportCallCounter.Increment();
#endif
            saveContext(false);
            Reg.setRegister(1, a);
            Reg.setRegister(2, b);
            Reg.setRegister(3, c);
            call1(getSupportProcedure(procIndex), 3);
        }

        public static void callMillicodeSupport2(int procIndex, SObject a,
                                                 SObject b) {
#if HAS_PERFORMANCE_COUNTERS
            if (millicodeSupportCallCounter != null) millicodeSupportCallCounter.Increment();
#endif
            saveContext(false);
            Reg.setRegister(1, a);
            Reg.setRegister(2, b);
            call1(getSupportProcedure(procIndex), 2);
        }

        public static void callMillicodeSupport1(int procIndex, SObject a) {
#if HAS_PERFORMANCE_COUNTERS
            if (millicodeSupportCallCounter != null) millicodeSupportCallCounter.Increment();
#endif
            saveContext(false);
            Reg.setRegister(1, a);
            call1(getSupportProcedure(procIndex), 1);
        }

        public static Procedure getSupportProcedure(int index) {
            SObject support = Reg.globalValue("millicode-support");
            if (support is SVL) {
                SVL procedures = (SVL) support;
                Procedure p = procedures.elementAt(index) as Procedure;
                if (p != null) {
                    return p;
                } else {
                    Exn.internalError("millicode support " + index + " not a procedure");
                    return null;
                }
            } else if (support == Factory.Undefined) {
                Exn.internalError("millicode-support is not defined (index: "+index+")");
                return null;
            } else {
                Exn.internalError("millicode-support is not a vector");
                return null;
            }
        }

        // saveContext: saves numbered registers and Reg.implicitContinuation to frame
        //   will restore registers and return to Reg.implicitContinuation of Reg.Register0
        //   clears Reg.implicitContinuation
        //   Also saves Result and flag whether to restore Result
        public static void saveContext(bool full) {
            // Save current context: Create frame described below, before RestoreContextCode
            Instructions.save(1 + Reg.LASTREG + 2);
            ContinuationFrame frame = Cont.cont;
            frame.saveRegisters(RestoreContextCode.singletonProcedure, full);
//            frame.setSlot(0, RestoreContextCode.singletonProcedure);
//            frame.returnIndex = Reg.implicitContinuation;
//            for (int i = 0; i <= Reg.LASTREG; ++i) {
//                frame.setSlot(i + 1, Reg.getRegister(i));
//            }
//            frame.setSlot(Reg.LASTREG + 2, Reg.Result);
//            frame.setSlot(Reg.LASTREG + 3, full ? Factory.True : Factory.False);
            Reg.implicitContinuation = -1;
        }

        // exit: exit the trampoline
        public static void exit(int returnCode) {
            throw new SchemeExitException(returnCode);
        }
    }

    // SPECIAL CONTINUATIONS

    /* InitialContinuation returns normally to the trampoline, which returns
     * normally to the caller.
     */
    public class InitialContinuation : CodeVector {
        public static readonly InitialContinuation singleton = new InitialContinuation();
        public static readonly Procedure singletonProcedure =
            new Procedure(InitialContinuation.singleton);
        private InitialContinuation() : base (1) {}

        public override CodeAddress call (int jump_index)
        {
            return null;
        }
    }

    /* RestoreContextCode
     * The frame contains complete saved context (reg0-LASTREG), with return
     * points RestoreContextCode and jumpIndex (= saved "implicit continuation")
     * RestoreContextCode restores the context and signals the trampoline
     * to call the procedure in reg0 with the jumpIndex.
     */
    public class RestoreContextCode : CodeVector {
        public static RestoreContextCode singleton = new RestoreContextCode();
        public static readonly Procedure singletonProcedure =
            new Procedure(RestoreContextCode.singleton);

        private RestoreContextCode() : base (CONTROL_POINT_LIMIT) {}

        public override CodeAddress call (int jumpIndex)
        {
          ContinuationFrame frame = Cont.cont;
          for (int i = 0; i < Reg.NREGS; ++i) {
              Reg.setRegister (i, frame.getSlot (i+1));
              }
          if (frame.getSlot (Reg.LASTREG + 3) == Factory.True) {
              Reg.Result = frame.getSlot (Reg.LASTREG + 2);
              }
          Procedure p0 = Reg.ProcRegister0;
          Cont.cont.checkPop (Reg.NREGS + 2, singletonProcedure);
          Cont.pop();
          return Call.call (p0.entrypoint, jumpIndex);
        }
    }

    /* EXCEPTIONS */

    /* BounceException is the superclass of all exceptions thrown
     * as a way of signaling the trampoline to continue execution.
     * It is used in a very specific way by Scheme.RT.Call.trampoline.
     */
    public class BounceException : Exception
    {
      public readonly CodeAddress target;

      public BounceException (CodeAddress pc)
      {
        this.target = pc;
      }

      public virtual void prepareForBounce()
      {
      }
    }

    /* CodeVectorCallException indicates that the given codevector
     * should be called with the given jump index.
     */
    public class CodeVectorCallException : BounceException
    {
      public CodeVectorCallException (CodeVector rtn, int j)
          : base (rtn.Address (j))
      {}
    }

    /* SchemeCallException is used to invoke a millicode support
     * procedure written in Scheme. It not only jumps to the right location,
     * but also does the invocation setup (sets reg0, sets Result=argc).
     * The arguments to the procedure should be in registers 1..argc
     */
    public class SchemeCallException : BounceException
    {
        private readonly Procedure p;
        private readonly SFixnum argc;
        public SchemeCallException (Procedure p, int argc) :
            base (p.InitialCodeAddress)
        {
            this.p = p;
            this.argc = Factory.makeFixnum (argc);
        }
        public override void prepareForBounce() {
            Reg.Second = Reg.Register0;
            Reg.Register0 = this.p;
            Reg.Result = argc;
        }
    }

    /* SchemeExitException signals that program execution should cease. */
    public class SchemeExitException : Exception {
        public readonly int returnCode;
        public SchemeExitException(int returnCode) {
            this.returnCode = returnCode;
        }
    }
}

using System;
using Scheme.Rep;

namespace Scheme.RT {

public class OpsSpecial {

    /* Misc */
    /* ---- */

    public static void op1_enable_interrupts(SObject arg) {
        Ops.expect1(arg.isFixnum(), arg, Constants.EX_EINTR);
        int time = ((SFixnum)arg).value;

        if (time > 0) {
            Reg.interruptsEnabled = true;
            Reg.timer = time;
        } else {
            Exn.fault(Constants.EX_EINTR,
                      "enable-interrupts: expected positive value");
        }
        Reg.Result = Factory.Unspecified;
        Exn.checkSignals();
    }

    public static void op1_disable_interrupts(SObject arg) {
        if (Reg.interruptsEnabled) {
            Reg.interruptsEnabled = false;
            Reg.Result = Factory.makeFixnum((int)Reg.timer);
        } else {
            Reg.Result = Factory.makeBoolean(false);
        }
        Exn.checkSignals();
    }

    /* // from <larceny_src>/Lib/Common/malcode.mal:
       ; Syscall has to be coded in mal because the arguments are passed in a
       ; non-standard way and because the compiler cannot handle a primitive
       ; with a variable, large, number of parameters.  Syscall is simply a
       ; trampoline into a millicode procedure.  RESULT has the number of 
       ; arguments, and the arguments are passed in registers as usual.
    */
    public static void op1_syscall(SObject arg) {
        // subtract one 'cuz the first arg is just the value
        // to which we want to dispatch.
//        System.Console.WriteLine("*** syscall {0}", Reg.register2);
        int num_args = ((SFixnum)arg).intValue() - 1;
        Sys num_syscall = (Sys) ((SFixnum)Reg.register1).intValue();
        Syscall.dispatch(num_args, num_syscall);
    }

    /* Comparison Operations */
    /* --------------------- */

    public static void op1_zerop(SObject arg) {
        if (arg is SFixnum) {
            Reg.Result = Factory.makeBoolean(((SFixnum)arg).value == 0);
            return;
        } else if (arg is SVL) {
            SVL a = (SVL) arg;
            if (a.tag == Tags.RatnumTag) {
                Reg.Result = Factory.False; // FIXME???
            } else if (a.tag == Tags.RectnumTag) {
                op2_numeric_equals(arg, Factory.makeFixnum(0));
                return;
            }
        } else if (arg is SByteVL) {
            SByteVL a = (SByteVL) arg;
            if (a.tag == Tags.BignumTag) {
                Reg.Result = Factory.makeBoolean(Number.getBignumLength(a) == 0);
                return;
            } else if (a.tag == Tags.FlonumTag) {
                Reg.Result = Factory.makeBoolean(a.unsafeAsDouble(0) == 0.0);
                return;
            } else if (a.tag == Tags.CompnumTag) {
                Reg.Result = Factory.makeBoolean
                    (a.unsafeAsDouble(0) == 0.0 &
                     a.unsafeAsDouble(1) == 0.0);
                return;
            }
        }
        Reg.Result = Factory.makeBoolean(false);
        return;
    }

    public static void op2_eqvp(SObject arg1, SObject arg2) {
        // EQ test first, get that out of the way.
        if (arg1 == arg2) {
            Reg.Result = Factory.True;
            return;
        } else if (arg1 is SChar & arg2 is SChar) {
            Reg.Result = Factory.wrap(((SChar)arg1).val == ((SChar)arg2).val);
            return;
        } else if (arg1 is SFixnum & arg2 is SFixnum) {
            bool result =
                ((SFixnum)arg1).value == ((SFixnum)arg2).value;
            Reg.Result = Factory.makeBoolean(result);
            return;
        } else if (arg1 is SVL & arg2 is SVL) {
            SVL a = (SVL)arg1;
            SVL b = (SVL)arg2;
            if (a.tag == Tags.RatnumTag & b.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RATNUM_EQUAL, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.RectnumTag & b.tag == Tags.RectnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RECTNUM_EQUAL, a, b);
                return; // TAIL CALL
            } else {
                Reg.Result = Factory.False;
                return;
            }
        } else if (arg1 is SByteVL & arg2 is SByteVL) {
            SByteVL a = (SByteVL)arg1;
            SByteVL b = (SByteVL)arg2;
            if (a.tag == Tags.BignumTag & b.tag == Tags.BignumTag) {
                Call.callMillicodeSupport2(Constants.MS_BIGNUM_EQUAL, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.FlonumTag & b.tag == Tags.FlonumTag) {
                double av = a.unsafeAsDouble(0);
                double bv = b.unsafeAsDouble(0);
                Reg.Result = Factory.makeBoolean(av == bv);
                return;
            } else if (a.tag == Tags.CompnumTag & b.tag == Tags.CompnumTag) {
                double ar = a.unsafeAsDouble(0);
                double ai = a.unsafeAsDouble(1);
                double br = b.unsafeAsDouble(0);
                double bi = b.unsafeAsDouble(1);
                Reg.Result = Factory.makeBoolean(ar == br & ai == bi);
                return;
            } else {
                Reg.Result = Factory.False;
                return;
            }
        } else {
            Reg.Result = Factory.False;
            return;
        }
    }

    public static void op2_numeric_equals(SObject arg1, SObject arg2) {
        if (arg1 is SFixnum & arg2 is SFixnum) {
            bool result = ((SFixnum)arg1).value == ((SFixnum)arg2).value;
            Reg.Result = Factory.makeBoolean(result);
            return;
        } else if (arg1 is SVL & arg2 is SVL) {
            SVL a = (SVL)arg1;
            SVL b = (SVL)arg2;
            if (a.tag == Tags.RatnumTag & b.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RATNUM_EQUAL, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.RectnumTag & b.tag == Tags.RectnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RECTNUM_EQUAL, a, b);
                return; // TAIL CALL
            }
        } else if (arg1 is SByteVL & arg2 is SByteVL) {
            SByteVL a = (SByteVL)arg1;
            SByteVL b = (SByteVL)arg2;
            if (a.tag == Tags.BignumTag &
                b.tag == Tags.BignumTag) {
                Call.callMillicodeSupport2(Constants.MS_BIGNUM_EQUAL, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.FlonumTag & b.tag == Tags.FlonumTag) {
                double av = a.unsafeAsDouble(0);
                double bv = b.unsafeAsDouble(0);
                Reg.Result = Factory.makeBoolean(av == bv);
                return;
            } else if (a.tag == Tags.CompnumTag & b.tag == Tags.CompnumTag) {
                double ar = a.unsafeAsDouble(0);
                double ai = a.unsafeAsDouble(1);
                double br = b.unsafeAsDouble(0);
                double bi = b.unsafeAsDouble(1);
                Reg.Result = Factory.makeBoolean(ar == br & ai == bi);
                return;
            }
        }
        Procedure generic
            = Call.getSupportProcedure(Constants.MS_GENERIC_EQUAL);
        Call.econtagion(arg1, arg2, generic);
        return; // TAIL CALL
    }

    public static void op2_less_than(SObject arg1, SObject arg2) {
        if (arg1 is SFixnum & arg2 is SFixnum) {
            bool result =
                ((SFixnum)arg1).value < ((SFixnum)arg2).value;
            Reg.Result = Factory.makeBoolean(result);
            return;
        } else if (arg1 is SVL & arg2 is SVL) {
            SVL a = (SVL)arg1;
            SVL b = (SVL)arg2;
            if (a.tag == Tags.RatnumTag & b.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RATNUM_LESS, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.RectnumTag & b.tag == Tags.RectnumTag) {
                Exn.fault(Constants.EX_LESSP, "< cannot compare rectnums", arg1, arg2);
                return; // TAIL CALL
            }
        } else if (arg1 is SByteVL & arg2 is SByteVL) {
            SByteVL a = (SByteVL)arg1;
            SByteVL b = (SByteVL)arg2;
            if (a.tag == Tags.BignumTag & b.tag == Tags.BignumTag) {
                Call.callMillicodeSupport2(Constants.MS_BIGNUM_LESS, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.FlonumTag & b.tag == Tags.FlonumTag) {
                double av = a.unsafeAsDouble(0);
                double bv = b.unsafeAsDouble(0);
                Reg.Result = Factory.makeBoolean(av < bv);
                return;
            } else if (a.tag == Tags.CompnumTag & b.tag == Tags.CompnumTag) {
                Exn.fault(Constants.EX_LESSP, "< cannot compare compnums", arg1, arg2);
                return;
            }
        }
        Procedure generic
            = Call.getSupportProcedure(Constants.MS_GENERIC_LESS);
        Call.pcontagion(arg1, arg2, generic);
        return; // TAIL CALL
    }
    public static void op2_greater_than(SObject arg1, SObject arg2) {
        if (arg1 is SFixnum & arg2 is SFixnum) {
            bool result =
                ((SFixnum)arg1).value > ((SFixnum)arg2).value;
            Reg.Result = Factory.makeBoolean(result);
            return;
        } else if (arg1 is SVL & arg2 is SVL) {
            SVL a = (SVL)arg1;
            SVL b = (SVL)arg2;
            if (a.tag == Tags.RatnumTag & b.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RATNUM_GREATER, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.RectnumTag & b.tag == Tags.RectnumTag) {
                Exn.fault(Constants.EX_GREATERP, "> cannot compare rectnums");
                return; // TAIL CALL
            }
        } else if (arg1 is SByteVL & arg2 is SByteVL) {
            SByteVL a = (SByteVL)arg1;
            SByteVL b = (SByteVL)arg2;
            if (a.tag == Tags.BignumTag & b.tag == Tags.BignumTag) {
                Call.callMillicodeSupport2(Constants.MS_BIGNUM_GREATER, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.FlonumTag & b.tag == Tags.FlonumTag) {
                double av = a.unsafeAsDouble(0);
                double bv = b.unsafeAsDouble(0);
                Reg.Result = Factory.makeBoolean(av > bv);
                return;
            } else if (a.tag == Tags.CompnumTag & b.tag == Tags.CompnumTag) {
                Exn.fault(Constants.EX_GREATERP, "> cannot compare compnums");
                return;
            }
        }
        Procedure generic
            = Call.getSupportProcedure(Constants.MS_GENERIC_GREATER);
        Call.pcontagion(arg1, arg2, generic);
        return; // TAIL CALL
    }
        
    public static void op2_less_or_equal(SObject arg1, SObject arg2) {
        if (arg1 is SFixnum & arg2 is SFixnum) {
            bool result =
                ((SFixnum)arg1).value <= ((SFixnum)arg2).value;
            Reg.Result = Factory.makeBoolean(result);
            return;
        } else if (arg1 is SVL & arg2 is SVL) {
            SVL a = (SVL)arg1;
            SVL b = (SVL)arg2;
            if (a.tag == Tags.RatnumTag & b.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RATNUM_LESSEQ, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.RectnumTag & b.tag == Tags.RectnumTag) {
                Exn.fault(Constants.EX_LESSEQP, "<= cannot compare rectnums");
                return; // TAIL CALL
            }
        } else if (arg1 is SByteVL & arg2 is SByteVL) {
            SByteVL a = (SByteVL)arg1;
            SByteVL b = (SByteVL)arg2;
            if (a.tag == Tags.BignumTag & b.tag == Tags.BignumTag) {
                Call.callMillicodeSupport2(Constants.MS_BIGNUM_LESSEQ, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.FlonumTag & b.tag == Tags.FlonumTag) {
                double av = a.unsafeAsDouble(0);
                double bv = b.unsafeAsDouble(0);
                Reg.Result = Factory.makeBoolean(av <= bv);
                return;
            } else if (a.tag == Tags.CompnumTag & b.tag == Tags.CompnumTag) {
                Exn.fault(Constants.EX_LESSEQP, "<= cannot compare compnums");
                return;
            }
        }
        Procedure generic
            = Call.getSupportProcedure(Constants.MS_GENERIC_LESSEQ);
        Call.pcontagion(arg1, arg2, generic);
        return; // TAIL CALL
    }
        
    public static void op2_greater_or_equal(SObject arg1, SObject arg2) {
        if (arg1 is SFixnum & arg2 is SFixnum) {
            bool result =
                ((SFixnum)arg1).value >= ((SFixnum)arg2).value;
            Reg.Result = Factory.makeBoolean(result);
            return;
        } else if (arg1 is SVL & arg2 is SVL) {
            SVL a = (SVL)arg1;
            SVL b = (SVL)arg2;
            if (a.tag == Tags.RatnumTag & b.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RATNUM_GREATEREQ, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.RectnumTag & b.tag == Tags.RectnumTag) {
                Exn.fault(Constants.EX_GREATEREQP, ">= cannot compare rectnums");
                return; // TAIL CALL
            }
        } else if (arg1 is SByteVL & arg2 is SByteVL) {
            SByteVL a = (SByteVL)arg1;
            SByteVL b = (SByteVL)arg2;
            if (a.tag == Tags.BignumTag & b.tag == Tags.BignumTag) {
                Call.callMillicodeSupport2(Constants.MS_BIGNUM_GREATEREQ, a, b);
                return; // TAIL CALL
            } else if (a.tag == Tags.FlonumTag & b.tag == Tags.FlonumTag) {
                double av = a.unsafeAsDouble(0);
                double bv = b.unsafeAsDouble(0);
                Reg.Result = Factory.makeBoolean(av >= bv);
                return;
            } else if (a.tag == Tags.CompnumTag & b.tag == Tags.CompnumTag) {
                Exn.fault(Constants.EX_GREATEREQP, ">= cannot compare compnums");
                return;
            }
        }
        Procedure generic
            = Call.getSupportProcedure(Constants.MS_GENERIC_GREATEREQ);
        Call.pcontagion(arg1, arg2, generic);
        return; // TAIL CALL
    }

    /* Arithmetic Operations */
    /* --------------------- */

    public static void op2_plus(SObject arg1, SObject arg2) {
        if (arg1 is SFixnum & arg2 is SFixnum) {
            int a = ((SFixnum)arg1).value;
            int b = ((SFixnum)arg2).value;
            Reg.Result = Factory.makeNumber(a + b);
            return;
        } else if (arg1 is SVL & arg2 is SVL) {
            SVL a = (SVL)arg1;
            SVL b = (SVL)arg2;
            if (a.tag == Tags.RatnumTag & b.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RATNUM_ADD,a,b);
                return; // TAIL CALL
            } else if (a.tag == Tags.RectnumTag & b.tag == Tags.RectnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RECTNUM_ADD,a,b);
                return; // TAIL CALL
            }
        } else if (arg1 is SByteVL & arg2 is SByteVL) {
            SByteVL a = (SByteVL)arg1;
            SByteVL b = (SByteVL)arg2;
            if (a.tag == Tags.BignumTag & b.tag == Tags.BignumTag) {
                Call.callMillicodeSupport2(Constants.MS_BIGNUM_ADD,a,b);
                return; // TAIL CALL
            } else if (a.tag == Tags.FlonumTag & b.tag == Tags.FlonumTag) {
                double av = a.unsafeAsDouble(0);
                double bv = b.unsafeAsDouble(0);
                double result = av + bv;
                Reg.Result = Factory.makeFlonum(result);
                return;
            } else if (a.tag == Tags.CompnumTag & b.tag == Tags.CompnumTag) {
                double ar = a.unsafeAsDouble(0);
                double ai = a.unsafeAsDouble(1);
                double br = b.unsafeAsDouble(0);
                double bi = b.unsafeAsDouble(1);
                Reg.Result = Factory.makeCompnum(ar + br, ai + bi);
                return;
            }
        }
        Procedure genericAdd 
            = Call.getSupportProcedure(Constants.MS_GENERIC_ADD);
        Call.contagion(arg1, arg2, genericAdd);
        return; // TAIL CALL
    }

    public static void op2_minus(SObject arg1, SObject arg2) {
        if (arg1 is SFixnum & arg2 is SFixnum) {
            int a = ((SFixnum)arg1).value;
            int b = ((SFixnum)arg2).value;
            Reg.Result = Factory.makeNumber(a - b);
            return;
        } else if (arg1 is SVL & arg2 is SVL) {
            SVL a = (SVL)arg1;
            SVL b = (SVL)arg2;
            if (a.tag == Tags.RatnumTag & b.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RATNUM_SUB,a,b);
                return; // TAIL CALL
            } else if (a.tag == Tags.RectnumTag & b.tag == Tags.RectnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RECTNUM_SUB,a,b);
                return; // TAIL CALL
            }
        } else if (arg1 is SByteVL & arg2 is SByteVL) {
            SByteVL a = (SByteVL)arg1;
            SByteVL b = (SByteVL)arg2;
            if (a.tag == Tags.BignumTag & b.tag == Tags.BignumTag) {
                Call.callMillicodeSupport2(Constants.MS_BIGNUM_SUB,a,b);
                return; // TAIL CALL
            } else if (a.tag == Tags.FlonumTag & b.tag == Tags.FlonumTag) {
                double av = a.unsafeAsDouble(0);
                double bv = b.unsafeAsDouble(0);
                double result = av - bv;
                Reg.Result = Factory.makeFlonum(result);
                return;
            } else if (a.tag == Tags.CompnumTag & b.tag == Tags.CompnumTag) {
                double ar = a.unsafeAsDouble(0);
                double ai = a.unsafeAsDouble(1);
                double br = b.unsafeAsDouble(0);
                double bi = b.unsafeAsDouble(1);
                Reg.Result = Factory.makeCompnum(ar - br, ai - bi);
                return;
            }
        }
        Procedure generic
            = Call.getSupportProcedure(Constants.MS_GENERIC_SUB);
        Call.contagion(arg1, arg2, generic);
        return; // TAIL CALL
    }
	
	

    public static void op2_multiply(SObject arg1, SObject arg2) {
        if (arg1 is SFixnum & arg2 is SFixnum) {
            long a = ((SFixnum)arg1).value;
            long b = ((SFixnum)arg2).value;
            Reg.Result = Factory.makeNumber(a * b);
            return;
        } else if (arg1 is SVL & arg2 is SVL) {
            SVL a = (SVL)arg1;
            SVL b = (SVL)arg2;
            if (a.tag == Tags.RatnumTag &
                b.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RATNUM_MUL,a,b);
                return; // TAIL CALL
            } else if (a.tag == Tags.RectnumTag & b.tag == Tags.RectnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RECTNUM_MUL,a,b);
                return; // TAIL CALL
            }
        } else if (arg1 is SByteVL & arg2 is SByteVL) {
            SByteVL a = (SByteVL)arg1;
            SByteVL b = (SByteVL)arg2;
            if (a.tag == Tags.BignumTag & b.tag == Tags.BignumTag) {
                Call.callMillicodeSupport2(Constants.MS_BIGNUM_MUL,a,b);
                return; // TAIL CALL
            } else if (a.tag == Tags.FlonumTag & b.tag == Tags.FlonumTag) {
                double av = a.unsafeAsDouble(0);
                double bv = b.unsafeAsDouble(0);
                double result = av * bv;
                Reg.Result = Factory.makeFlonum(result);
                return;
			} else if (a.tag == Tags.CompnumTag) {
				double ar = a.unsafeAsDouble(0);
				double ai = a.unsafeAsDouble(1);
				if (b.tag == Tags.CompnumTag) {
					double br = b.unsafeAsDouble(0);
					double bi = b.unsafeAsDouble(1);
					// We have to consider separately the case where one 
					// of the imaginary parts is 0 in order to avoid 
					// getting NaN.  
					if (ai == 0) {
						double real = ar * br;
						double imag = ar * bi;
						Reg.Result = Factory.makeCompnum(real, imag);
					} else if (bi == 0) {
						double real = ar * br;
						double imag = ai * br;
						Reg.Result = Factory.makeCompnum(real, imag);
					} else {
						double real = ar * br - ai * bi;
						double imag = ar * bi + ai * br;
						Reg.Result = Factory.makeCompnum(real, imag);
					}
					return;
				} else if (b.tag == Tags.FlonumTag & ai == 0.0) {
					double br = b.unsafeAsDouble(0);
					Reg.Result = Factory.makeFlonum(ar * br);
					return;
				}
            } 
        }
        Procedure generic
            = Call.getSupportProcedure(Constants.MS_GENERIC_MUL);
        Call.contagion(arg1, arg2, generic);
        return; // TAIL CALL
    }

    public static void op2_divide(SObject arg1, SObject arg2) {
        if (arg1 is SFixnum & arg2 is SFixnum) {
            int a = ((SFixnum)arg1).value;
            int b = ((SFixnum)arg2).value;
            if (b != 0) {
                int result = a / b;
                if (result * b == a) {
                    Reg.Result = Factory.makeFixnum(result);
                    return;
                }
            } else {
                Exn.fault(Constants.EX_DIV, "division by zero", arg1, arg2);
                return;
            }
            Call.callMillicodeSupport2(Constants.MS_FIXNUM2RATNUM_DIV,
                                       arg1, arg2);
            return; // TAIL CALL
        } else if (arg1 is SVL & arg2 is SVL) {
            SVL a = (SVL)arg1;
            SVL b = (SVL)arg2;
            if (a.tag == Tags.RatnumTag & b.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RATNUM_DIV,a,b);
                return; // TAIL CALL
            } else if (a.tag == Tags.RectnumTag & b.tag == Tags.RectnumTag) {
                Call.callMillicodeSupport2(Constants.MS_RECTNUM_DIV,a,b);
                return; // TAIL CALL
            }
        } else if (arg1 is SByteVL & arg2 is SByteVL) {
            SByteVL a = (SByteVL)arg1;
            SByteVL b = (SByteVL)arg2;
            if (a.tag == Tags.BignumTag & b.tag == Tags.BignumTag) {
                Call.callMillicodeSupport2(Constants.MS_BIGNUM_DIV,a,b);
                return; // TAIL CALL
            } else if (a.tag == Tags.FlonumTag & b.tag == Tags.FlonumTag) {
                double av = a.unsafeAsDouble(0);
                double bv = b.unsafeAsDouble(0);
                double result = av / bv;
                Reg.Result = Factory.makeFlonum(result);
                return;
            } else if (a.tag == Tags.CompnumTag & b.tag == Tags.CompnumTag) {
                double ar = a.unsafeAsDouble(0);
                double ai = a.unsafeAsDouble(1);
                double br = b.unsafeAsDouble(0);
                double bi = b.unsafeAsDouble(1);
                double denom = br * br + bi * bi;
                Reg.Result = Factory.makeCompnum
                    ((ar * br + ai * bi) / denom,
                     (ai * br - ar * bi) / denom);
                return;
            }
        }
        Procedure generic
            = Call.getSupportProcedure(Constants.MS_GENERIC_DIV);
        Call.contagion(arg1, arg2, generic);
        return; // TAIL CALL
    }

    public static void op2_quotient(SObject arg1, SObject arg2) {
        if (arg1 is SFixnum & arg2 is SFixnum) {
            int a = ((SFixnum)arg1).value;
            int b = ((SFixnum)arg2).value;
            if (b != 0) {
                int result = a / b;
                Reg.Result = Factory.makeFixnum(result);
                return;
            } else {
                Exn.fault(Constants.EX_QUOTIENT, "division by zero", arg1, arg2);
                return;
            }
        } else if (arg1 is SByteVL & arg2 is SFixnum) {
            SByteVL a = (SByteVL) arg1;
            int b = ((SFixnum)arg2).value;
            if (b == 0) {
                Exn.fault(Constants.EX_QUOTIENT, "division by zero", arg1, arg2);
                return;
            }
            if (b > 0
                && a.tag == Tags.BignumTag
                && Number.getBignumLength(a) == 1
                && Number.getBignumSign(a) == Number.BIGNUM_POSITIVE) {
                // Exn.msg.WriteLine("++++ doing bignum quotient in millicode");
                uint av = a.getUInt32(1);
                uint result = av / (uint)b;
                Reg.Result = Factory.makeNumber(result);
                return;
            }
        }
        Call.callMillicodeSupport2(Constants.MS_HEAVY_QUOTIENT, arg1, arg2);
        return; // TAIL CALL
    }

    public static void op2_remainder(SObject arg1, SObject arg2) {
        if (arg1 is SFixnum & arg2 is SFixnum) {
            int a = ((SFixnum)arg1).value;
            int b = ((SFixnum)arg2).value;
            if (b != 0) {
                Reg.Result = Factory.makeFixnum(a % b); // FIXME: is % same as Scheme remainder?
                return;
            } else {
                Exn.fault(Constants.EX_REMAINDER, "division by zero", arg1, arg2);
                return;
            }
        } else if (arg1 is SByteVL & arg2 is SFixnum) {
            SByteVL a = (SByteVL) arg1;
            int b = ((SFixnum)arg2).value;
            if (b == 0) {
                Exn.fault(Constants.EX_QUOTIENT, "division by zero", arg1, arg2);
                return;
            }
            if (b > 0
                && a.tag == Tags.BignumTag
                && Number.getBignumLength(a) == 1
                && Number.getBignumSign(a) == Number.BIGNUM_POSITIVE) {
                //Exn.msg.WriteLine("++++ doing bignum remainder in millicode");
                uint av = a.getUInt32(1);
                uint result = av % (uint)b;  // FIXME: % same as scheme remainder?
                Reg.Result = Factory.makeNumber(result);
                return;
            }
        }
        Call.callMillicodeSupport2(Constants.MS_HEAVY_REMAINDER,arg1,arg2);
        return; // TAIL CALL
    }
    
    public static void op1_truncate(SObject arg) {
        if (arg is SFixnum) {
            Reg.Result = arg;
            return;
        } else if (arg is SVL) {
            SVL a = (SVL) arg;
            if (a.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RATNUM_TRUNCATE, arg);
                return;
            }
        } else if (arg is SByteVL) {
            SByteVL a = (SByteVL) arg;
            if (a.tag == Tags.FlonumTag) {
                double d = a.unsafeAsDouble(0);
                if (d < 0) {
                    Reg.Result = Factory.makeFlonum(System.Math.Ceiling(d));
                    return;
                } else {
                    Reg.Result = Factory.makeFlonum(System.Math.Floor(d));
                    return;
                }
            } else if (a.tag == Tags.BignumTag) {
                return;
            }
        }
        Exn.fault(Constants.EX_TRUNC, "truncate: expected real number");
    }

    public static void op1_round(SObject arg) {
        if (arg is SFixnum) {
            return;
        } else if (arg is SVL) {
            SVL a = (SVL) arg;
            if (a.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RATNUM_ROUND, arg);
                return;
            }
        } else if (arg is SByteVL) {
            SByteVL a = (SByteVL) arg;
            if (a.tag == Tags.FlonumTag) {
                double d = a.unsafeAsDouble(0);
                Reg.Result = Factory.makeFlonum(System.Math.Round(d));
                return;
            } else if (a.tag == Tags.BignumTag) {
                return;
            }
        }
        Exn.fault(Constants.EX_ROUND, "round: expected real number");
    }

    public static void op1_negative(SObject arg) {
        if (arg is SFixnum) {
            int a = ((SFixnum)arg).value;
            Reg.Result = Factory.makeNumber(-a);
            return;
        } else if (arg is SVL) {
            SVL a = (SVL) arg;
            if (a.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RATNUM_NEGATE, a);
                return; // TAIL CALL
            } else if (a.tag == Tags.RectnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RECTNUM_NEGATE, a);
                return;
            }
        } else if (arg is SByteVL) {
            SByteVL a = (SByteVL) arg;
            if (a.tag == Tags.BignumTag) {
                Call.callMillicodeSupport1(Constants.MS_BIGNUM_NEGATE, a);
                return;
            } else if (a.tag == Tags.FlonumTag) {
                Reg.Result = Factory.makeFlonum(-a.unsafeAsDouble(0));
                return;
            } else if (a.tag == Tags.CompnumTag) {
                double real = a.unsafeAsDouble(0);
                double imag = a.unsafeAsDouble(1);
                Reg.Result = Factory.makeCompnum(-real, -imag);
                return;
            }
        }
        Exn.fault(Constants.EX_NEG, "not a number");
        return;
    }

    public static void op1_exact2inexact(SObject arg) {
        if (arg is SFixnum) {
            int a = ((SFixnum)arg).value;
            Reg.Result = Factory.makeFlonum((double) a);
            return;
        } else if (arg is SByteVL) {
            SByteVL a = (SByteVL) arg;
            if (a.tag == Tags.BignumTag) {
                ; // fallthrough
            } else if (a.tag == Tags.FlonumTag) {
                return;
            } else if (a.tag == Tags.CompnumTag) {
                return;
            }
        }
        Call.callMillicodeSupport1(Constants.MS_GENERIC_EXACT2INEXACT, arg);
        return;

    }
    public static void op1_inexact2exact(SObject arg) {
        if (arg is SFixnum) {
            return;
        } else if (arg is SVL) {
            SVL a = (SVL) arg;
        } else if (arg is SByteVL) {
            SByteVL a = (SByteVL) arg;
            if (a.tag == Tags.FlonumTag
                | a.tag == Tags.CompnumTag) {
                Call.callMillicodeSupport1(Constants.MS_GENERIC_INEXACT2EXACT, arg);
                return;
            } else if (a.tag == Tags.BignumTag) {
                return;
            }
        } else if (arg is SVL) {
            SVL a = (SVL) arg;
            if (a.tag == Tags.RatnumTag) {
                return;
            } else if (a.tag == Tags.RectnumTag) {
                return;
            }
        }
        Exn.fault(Constants.EX_I2E, "not a number");
        return;
    }

}
}

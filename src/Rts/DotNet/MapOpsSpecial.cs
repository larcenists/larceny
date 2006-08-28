using System;
using Scheme.Rep;

namespace Scheme.RT {

public class OpsSpecial {

    /* Misc */
    /* ---- */

    public static void op1_enable_interrupts(SObject arg) {
        arg.op_enable_interrupts();
    }

    public static void op1_disable_interrupts(SObject arg) {
        arg.op_disable_interrupts();
    }

    public static void op1_syscall(SObject arg) {
        arg.op_syscall();
    }

    public static void op1_zerop(SObject arg) {
        arg.op_zerop();
    }

    public static void op2_eqvp(SObject arg1, SObject arg2) {
        arg1.op_eqvp(arg2);
    }

    public static void op2_numeric_equals(SObject arg1, SObject arg2) {
        arg1.op_numeric_equals(arg2);
    }

    public static void op2_less_than(SObject arg1, SObject arg2) {
        arg1.op_less_than(arg2);
    }
    public static void op2_greater_than(SObject arg1, SObject arg2) {
        arg1.op_greater_than(arg2);
    }
        
    public static void op2_less_or_equal(SObject arg1, SObject arg2) {
        arg1.op_less_or_equal(arg2);
    }
        
    public static void op2_greater_or_equal(SObject arg1, SObject arg2) {
        arg1.op_greater_or_equal(arg2);
    }

    public static void op2_plus(SObject arg1, SObject arg2) {
        arg1.op_plus(arg2);
    }

    public static void op2_minus(SObject arg1, SObject arg2) {
        arg1.op_minus(arg2);
    }

    public static void op2_multiply(SObject arg1, SObject arg2) {
        arg1.op_multiply(arg2);
    }

    public static void op2_divide(SObject arg1, SObject arg2) {
        arg1.op_divide(arg2);
    }

    public static void op2_quotient(SObject arg1, SObject arg2) {
        arg1.op_quotient(arg2);
    }

    public static void op2_remainder(SObject arg1, SObject arg2) {
        arg1.op_remainder(arg2);
    }
    
    public static void op1_truncate(SObject arg) {
        arg.op_truncate();
    }

    public static void op1_round(SObject arg) {
        arg.op_round();
    }

    public static void op1_negative(SObject arg) {
        arg.op_negative();
    }

    public static void op1_exact2inexact(SObject arg) {
        arg.op_exact2inexact();
    }
    public static void op1_inexact2exact(SObject arg) {
        arg.op_inexact2exact();
    }

}
}

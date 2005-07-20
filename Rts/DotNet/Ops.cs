using System;
using Scheme.Rep;

namespace Scheme.RT {

public class Ops {

    /* ===================================================== */
    /*   Utility                                             */
    /* ===================================================== */

   public static void expect1(bool b, SObject arg, int blame) {
        if (!b) {
            Exn.fault(blame, "bad argument: " + arg, arg);
        }
    }

    public static void expect2(bool b1, SObject arg1,
                               bool b2, SObject arg2,
                               int blame) {
        if (!b1) {
            Exn.fault(blame, "bad argument 1: " + arg1, arg1, arg2);
        }
        if (!b2) {
            Exn.fault(blame, "bad argument 2: " + arg2, arg1, arg2);
        }
    }

    public static void expect3(bool b1, SObject arg1,
                               bool b2, SObject arg2,
                               bool b3, SObject arg3, int blame) {
        if (!b1) {
            Exn.fault(blame, "bad argument 1: " + arg1, arg1, arg2, arg3);
        }
        if (!b2) {
            Exn.fault(blame, "bad argument 2: " + arg2, arg1, arg2, arg3);
        }
        if (!b3) {
            Exn.fault(blame, "bad argument 3: " + arg3, arg1, arg2, arg3);
        }
    }

    public static void rangeCheckBVL(SObject arg1, SObject arg2, int blame) {
        SByteVL bv = (SByteVL) arg1;
        int index = ((SFixnum) arg2).value;
        if (index >= 0 && index < bv.elements.Length) {
            
        } else {
            Exn.fault(blame, "index out of range", arg1, arg2);
        }
    }

    public static void rangeCheckBVL(SObject arg1, SObject arg2, SObject arg3, int blame) {
        SByteVL bv = (SByteVL) arg1;
        int index = ((SFixnum) arg2).value;
        if (index >= 0 && index < bv.elements.Length) {
            
        } else {
            Exn.fault(blame, "index out of range", arg1, arg2, arg3);
        }
    }

    public static void rangeCheckVL(SObject arg1, SObject arg2, int blame) {
        SVL bv = (SVL) arg1;
        int index = ((SFixnum) arg2).value;
        if (index >= 0 && index < bv.elements.Length) {
            
        } else {
            Exn.fault(blame, "index out of range", arg1, arg2);
        }
    }

    public static void rangeCheckVL(SObject arg1, SObject arg2, SObject arg3, int blame) {
        SVL bv = (SVL) arg1;
        int index = ((SFixnum) arg2).value;
        if (index >= 0 && index < bv.elements.Length) {
            
        } else {
            Exn.fault(blame, "index out of range", arg1, arg2, arg3);
        }
    }



    /* ===================================================== */
    /*   Misc                                                */
    /* ===================================================== */

    /* Continuation manipulation */
    /* ------------------------- */

    public static SObject op1_creg(SObject unused) {
        return Cont.getCC();
    }

    public static SObject op1_creg_set(SObject arg) {
        Cont.setCC(arg);
        return Factory.Unspecified;
    }

    public static SObject op1_break(SObject arg) {
        Exn.fault(Constants.EX_BREAKPOINT);
        return Factory.Impossible;
    }
    
    public static SObject op1_gc_counter(SObject arg) {
        Exn.fault(Constants.EX_UNSUPPORTED);
        return Factory.Unspecified;
    }

    public static SObject op1_not(SObject arg) {
        return (Reg.Result == Factory.False) ? Factory.True : Factory.False;
    }
        
    /* ===================================================== */
    /*   Predicates                                          */
    /* ===================================================== */

    /* Numeric Predicates */
    /* ------------------ */

    public static SObject op1_complexp(SObject arg) {
        return arg.op_complexp();
    }
    public static SObject op1_realp(SObject arg) {
        return arg.op_realp();
    }
    public static SObject op1_rationalp(SObject arg) {
        return arg.op_rationalp();
    }
    public static SObject op1_integerp(SObject arg) {
        return arg.op_integerp();
    }
    public static SObject op1_exactp(SObject arg) {
        return arg.op_exactp();
    }
    public static SObject op1_inexactp(SObject arg) {
        return arg.op_inexactp();
    }

    /* General Predicates */
    /* ------------------ */

    public static SObject op2_eqp(SObject arg1, SObject arg2) {
        return (arg1 == arg2) ? Factory.True : Factory.False;
    }

    public static SObject op1_fixnump(SObject arg) {
        return arg.op_fixnump();
    }
    public static SObject op1_pairp(SObject arg) {
        return arg.op_pairp();
    }
    public static SObject op1_flonump(SObject arg) {
        return arg.op_flonump();
    }
    public static SObject op1_compnump(SObject arg) {
        return arg.op_compnump();
    }
    public static SObject op1_symbolp(SObject arg) {
        return arg.op_symbolp();
    }
    public static SObject op1_vectorp(SObject arg) {
        return arg.op_vectorp();
    }
    public static SObject op1_vector_likep(SObject arg) {
        return arg.op_vector_likep();
    }
    public static SObject op1_portp(SObject arg) {
        return arg.op_portp();
    }
    public static SObject op1_structurep(SObject arg) {
        return arg.op_structurep();
    }
    public static SObject op1_bytevector_likep(SObject arg) {
        return arg.op_bytevector_likep();
    }
    public static SObject op1_stringp(SObject arg) {
        return arg.op_stringp();
    }
    public static SObject op1_bytevectorp(SObject arg) {
        return arg.op_bytevectorp();
    }
    public static SObject op1_charp(SObject arg) {
        return arg.op_charp();
    }
    public static SObject op1_procedurep(SObject arg) {
        return arg.op_procedurep();
    }

    /* Special Value predicates */
    /* ------------------------ */
    public static SObject op1_nullp(SObject arg) {
        return arg.op_nullp();
    }
    public static SObject op1_eof_objectp(SObject arg) {
        return arg.op_eof_objectp();
    }
    public static SObject op1_unspecifiedp(SObject arg) {
        return arg.op_unspecifiedp();
    }
    public static SObject op1_undefinedp(SObject arg) {
        return arg.op_undefinedp();
    }

    /* ===================================================== */
    /*   DATA                                                */
    /* ===================================================== */


    /* Constants as operations */
    /* ----------------------- */

    public static SObject op1_unspecified(SObject unused) {
        return Factory.Unspecified;
    }
    public static SObject op1_undefined(SObject unused) {
        return Factory.Undefined;
    }
    public static SObject op1_eof_object(SObject unused) {
        return Factory.Eof;
    }

    /* Char Operations */
    /* --------------- */

    public static SObject op2_char_lt(SObject arg1, SObject arg2) {
        return arg1.op_char_lt(arg2);
    }

    public static SObject op2_char_le(SObject arg1, SObject arg2) {
        return arg1.op_char_le(arg2);
    }

    public static SObject op2_char_gt(SObject arg1, SObject arg2) {
        return arg1.op_char_gt(arg2);
    }

    public static SObject op2_char_ge(SObject arg1, SObject arg2) {
        return arg1.op_char_ge(arg2);
    }

    public static SObject op2_char_equals(SObject arg1, SObject arg2) {
        return arg1.op_char_equals(arg2);
    }

    public static SObject op1_char2integer(SObject arg) {
        return arg.op_char2integer();
    }

    public static SObject op1_integer2char(SObject arg) {
        return arg.op_integer2char();
    }

    /* Cell Operations */
    /* --------------- */

    public static SObject op1_make_cell(SObject arg) {
        return arg.op_make_cell();
    }
    public static SObject op1_cell_ref(SObject arg) {
        return arg.op_cell_ref();
    }
    public static SObject op2_cell_set(SObject arg1, SObject arg2) {
        return arg1.op_cell_set(arg2);
    }

    /* List operations */
    /* --------------- */

    public static SObject op2_cons(SObject arg1, SObject arg2) {
        return arg1.op_cons(arg2);
    }

    public static SObject op1_car(SObject arg) {
        return arg.op_car();
    }
    public static SObject op1_car_pair(SObject arg) {
        return arg.op_car_pair();
    }

    public static SObject op1_cdr(SObject arg) {
        return arg.op_cdr();
    }
    public static SObject op1_cdr_pair(SObject arg) {
        return arg.op_cdr_pair();
    }

    public static SObject op2_set_car(SObject arg1, SObject arg2) {
        return arg1.op_set_car(arg2);
    }
    public static SObject op2_set_car_pair(SObject arg1, SObject arg2) {
        return arg1.op_set_car_pair(arg2);
    }

    public static SObject op2_set_cdr(SObject arg1, SObject arg2) {
        return arg1.op_set_cdr(arg2);
    }
    public static SObject op2_set_cdr_pair(SObject arg1, SObject arg2) {
        return arg1.op_set_cdr_pair(arg2);
    }

    /* VectorLike and ByteVectorLike operations */
    /* ---------------------------------------- */

    public static SObject op1_typetag(SObject arg) {
        return arg.op_typetag();
    }
    public static SObject op2_typetag_set(SObject arg1, SObject arg2) {
        return arg1.op_typetag_set(arg2);
    }

    /* VectorLike operations */
    /* --------------------- */

    public static SObject op1_vector_like_length(SObject arg) {
        return arg.op_vector_like_length();
    }
    public static SObject op2_vector_like_ref(SObject arg1, SObject arg2) {
        return arg1.op_vector_like_ref(arg2);
    }

    public static SObject op3_vector_like_set(SObject arg1, SObject arg2,
                                           SObject arg3) {
        return arg1.op_vector_like_set(arg2, arg3);
    }

    /* Vector operations */
    /* ----------------- */

    public static SObject op2_make_vector(SObject arg1, SObject arg2) {
        return arg1.op_make_vector(arg2);
    }

    public static SObject op1_vector_length(SObject arg) {
        return arg.op_vector_length();
    }
    public static SObject op1_vector_length_vec(SObject arg) {
        return arg.op_vector_length_vec();
    }

    public static SObject op2_vector_ref(SObject arg1, SObject arg2) {
        return arg1.op_vector_ref(arg2);
    }
    public static SObject op2_vector_ref_trusted(SObject arg1, SObject arg2) {
        return arg1.op_vector_ref_trusted(arg2);
    }

    public static SObject op3_vector_set(SObject arg1, SObject arg2, SObject arg3) {
        return arg1.op_vector_set(arg2, arg3);
    }
    public static SObject op3_vector_set_trusted(SObject arg1, SObject arg2, SObject arg3) {
        return arg1.op_vector_set_trusted(arg2, arg3);
    }

    public static SObject op2_sys_partial_list__vector(SObject arg1, SObject arg2) {
        expect2(arg1 == Factory.Null || arg1 is SPair, arg1, arg2.isFixnum(), arg2, -1);
        int n = ((SFixnum)arg2).value;
        SObject[] items = new SObject[n];
        for (int i = 0; i < n; ++i) {
            items[i] = ((SPair)arg1).first;
            arg1 = ((SPair)arg1).rest;
        }
        return Factory.makeVector(items);
    }

    /* Procedure operations */
    /* -------------------- */

    public static SObject op1_procedure_length(SObject arg) {
        return arg.op_procedure_length();
    }

    public static SObject op1_make_procedure(SObject arg) {
        return arg.op_make_procedure();
    }

    public static SObject op2_procedure_ref(SObject arg1, SObject arg2) {
        return arg1.op_procedure_ref(arg2);
    }

    public static SObject op3_procedure_set(SObject arg1, SObject arg2, SObject arg3) {
        return arg1.op_procedure_set(arg2, arg3);
    }

    /* String operations */
    /* ----------------- */

    public static SObject op2_make_string(SObject arg1, SObject arg2) {
        return arg1.op_make_string(arg2);
    }

    public static SObject op1_string_length(SObject arg) {
        return arg.op_string_length();
    }
    public static SObject op1_string_length_str(SObject arg) {
        return arg.op_string_length_str();
    }

    public static SObject op2_string_ref(SObject arg1, SObject arg2) {
        return arg1.op_string_ref(arg2);
    }
    public static SObject op2_string_ref_trusted(SObject arg1, SObject arg2) {
        return arg1.op_string_ref_trusted(arg2);
    }

    public static SObject op3_string_set(SObject arg1, SObject arg2, SObject arg3) {
        return arg1.op_string_set(arg2, arg3);
    }
    public static SObject op3_string_set_trusted(SObject arg1, SObject arg2, SObject arg3) {
        return arg1.op_string_set_trusted(arg2, arg3);
    }

    /* Bytevector operations */
    /* --------------------- */

    public static SObject op1_make_bytevector(SObject arg) {
        return arg.op_make_bytevector();
    }
    public static SObject op1_bytevector_length(SObject arg) {
        return arg.op_bytevector_length();
    }
    public static SObject op2_bytevector_ref(SObject arg1, SObject arg2) {
        return arg1.op_bytevector_ref(arg2);
    }
    public static SObject op3_bytevector_set(SObject arg1, SObject arg2,
                                          SObject arg3) {
        return arg1.op_bytevector_set(arg2, arg3);
    }
    public static SObject op2_bytevector_equal(SObject arg1, SObject arg2) {
        return arg1.op_bytevector_equal(arg2);
    }

    public static SObject op2_bytevector_fill(SObject arg1, SObject arg2) {
        return arg1.op_bytevector_fill(arg2);
    }

    /* Bytevector-like operations */
    /* -------------------------- */

    public static SObject op1_bytevector_like_length(SObject arg) {
        return arg.op_bytevector_like_length();
    }
    public static SObject op2_bytevector_like_ref(SObject arg1, SObject arg2) {
        return arg1.op_bytevector_like_ref(arg2);
    }
    public static SObject op3_bytevector_like_set(SObject arg1, SObject arg2, SObject arg3) {
        return arg1.op_bytevector_like_set(arg2, arg3);
    }
    public static SObject op2_sys_bvlcmp(SObject arg1, SObject arg2) {
        return arg1.op_sys_bvlcmp(arg2);
    }
    
    /* Fixnum Ops                */
    /* ------------------------- */

    public static SObject op1_fxzerop(SObject arg) {
        return arg.op_fxzerop();
    }
    public static SObject op1_fxpositivep(SObject arg) {
        return arg.op_fxpositivep();
    }
    public static SObject op1_fxnegativep(SObject arg) {
        return arg.op_fxnegativep();
    }
    public static SObject op1_fxnegative(SObject arg) {
        return arg.op_fxnegative();
    }
    public static SObject op2_fxplus(SObject arg1, SObject arg2) {
        return arg1.op_fxplus(arg2);
    }
    public static SObject op2_fxminus(SObject arg1, SObject arg2) {
        return arg1.op_fxminus(arg2);
    }
	public static SObject op2_fxmul(SObject arg1, SObject arg2) {
	    return arg1.op_fxmul(arg2);
	}
    public static SObject op2_fxless(SObject arg1, SObject arg2) {
        return arg1.op_fxless(arg2);
    }
    public static SObject op2_fxgreater(SObject arg1, SObject arg2) {
        return arg1.op_fxgreater(arg2);
    }
    public static SObject op2_fxless_equal(SObject arg1, SObject arg2) {
        return arg1.op_fxless_equal(arg2);
    }
    public static SObject op2_fxgreater_equal(SObject arg1, SObject arg2) {
        return arg1.op_fxgreater_equal(arg2);
    }
    public static SObject op2_fxequal(SObject arg1, SObject arg2) {
        return arg1.op_fxequal(arg2);
    }

    public static SObject op1_most_positive_fixnum(SObject unused) {
        return Factory.makeFixnum(SFixnum.MAX);
    }
    public static SObject op1_most_negative_fixnum(SObject unused) {
        return Factory.makeFixnum(SFixnum.MIN);
    }

    /* Logical (aka Bitwise) Ops */
    /* ------------------------- */
        
    /* Logical operations operate on fixnums
     * What about bignums? See SRFI... ?
     */
    public static SObject op1_lognot(SObject arg) {
        return arg.op_lognot();
    }
    public static SObject op2_logand(SObject arg1, SObject arg2) {
        return arg1.op_logand(arg2);
    }
    public static SObject op2_logior(SObject arg1, SObject arg2) {
        return arg1.op_logior(arg2);
    }
    public static SObject op2_logxor(SObject arg1, SObject arg2) {
        return arg1.op_logxor(arg2);
    }
    public static SObject op2_lsh(SObject arg1, SObject arg2) {
        return arg1.op_lsh(arg2);
    }
    public static SObject op2_rsh(SObject arg1, SObject arg2) {
        return arg1.op_rsh(arg2);
    }
    public static SObject op2_rsha(SObject arg1, SObject arg2) {
        return arg1.op_rsha(arg2);
    }
    public static SObject op2_rshl(SObject arg1, SObject arg2) {
        return arg1.op_rshl(arg2);
    }
    // rot

    /* Arithmetic operations */
    /* --------------------- */

    public static SObject op1_real_part(SObject arg) {
        return arg.op_real_part();
    }

    public static SObject op1_imag_part(SObject arg) {
        return arg.op_imag_part();
    }

    public static SObject op2_plus_idx_idx(SObject arg1, SObject arg2) {
        return Factory.makeNumber
            (((SFixnum)arg1).value + ((SFixnum)arg2).value);
    }

    public static SObject op2_minus_idx_idx(SObject arg1, SObject arg2) {
        return Factory.makeNumber 
            (((SFixnum)arg1).value - ((SFixnum)arg2).value);
    }

    public static SObject op2_equal_fix_fix(SObject arg1, SObject arg2) {
        return
            (((SFixnum)arg1).value == ((SFixnum)arg2).value) ? Factory.True : Factory.False;
    }
    public static SObject op2_less_fix_fix(SObject arg1, SObject arg2) {
        return
            (((SFixnum)arg1).value < ((SFixnum)arg2).value) ? Factory.True : Factory.False;
    }
    public static SObject op2_lessequal_fix_fix(SObject arg1, SObject arg2) {
        return
            (((SFixnum)arg1).value <= ((SFixnum)arg2).value) ? Factory.True : Factory.False;
    }
    public static SObject op2_greater_fix_fix(SObject arg1, SObject arg2) {
        return
            (((SFixnum)arg1).value > ((SFixnum)arg2).value) ? Factory.True : Factory.False;
    }
    public static SObject op2_greaterequal_fix_fix(SObject arg1, SObject arg2) {
        return
            (((SFixnum)arg1).value >= ((SFixnum)arg2).value) ? Factory.True : Factory.False;
    }

}
}

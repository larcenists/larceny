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
        throw new Exception("got break + arg: " + arg);
//        Exn.fault(Constants.EX_UNSUPPORTED);
//        return Factory.Unspecified;
    }
    
    public static SObject op1_gc_counter(SObject arg) {
        Exn.fault(Constants.EX_UNSUPPORTED);
        return Factory.Unspecified;
    }

    public static SObject op1_not(SObject arg) {
        return Factory.makeBoolean(Reg.Result == Factory.False);
    }
        
    /* ===================================================== */
    /*   Predicates                                          */
    /* ===================================================== */

    /* Numeric Predicates */
    /* ------------------ */

    public static SObject op1_complexp(SObject arg) {
        return Factory.makeBoolean(arg.isComplex());
    }
    public static SObject op1_realp(SObject arg) {
        return Factory.makeBoolean(arg.isReal());
    }
    public static SObject op1_rationalp(SObject arg) {
        return Factory.makeBoolean(arg.isRational());
    }
    public static SObject op1_integerp(SObject arg) {
        return Factory.makeBoolean(arg.isInteger());
    }
    public static SObject op1_exactp(SObject arg) {
        expect1(arg.isNumber(), arg, Constants.EX_EXACTP);
        return Factory.makeBoolean(arg.isExact());
    }
    public static SObject op1_inexactp(SObject arg) {
        expect1(arg.isNumber(), arg, Constants.EX_INEXACTP);
        return Factory.makeBoolean(arg.isInexact());
    }

    /* General Predicates */
    /* ------------------ */

    public static SObject op2_eqp(SObject arg1, SObject arg2) {
        return Factory.makeBoolean(arg1 == arg2);
    }

    public static SObject op1_fixnump(SObject arg) {
        return Factory.makeBoolean(arg is SFixnum);
    }
    public static SObject op1_pairp(SObject arg) {
        return Factory.makeBoolean(arg is SPair);
    }
    public static SObject op1_flonump(SObject arg) {
        return Factory.makeBoolean(arg.isFlonum());
    }
    public static SObject op1_compnump(SObject arg) {
        return Factory.makeBoolean(arg.isCompnum());
    }
    public static SObject op1_symbolp(SObject arg) {
        return Factory.makeBoolean(arg.isSymbol());
    }
    public static SObject op1_vectorp(SObject arg) {
        return Factory.makeBoolean(arg.isVector());
    }
    public static SObject op1_vector_likep(SObject arg) {
        return Factory.makeBoolean(arg is SVL);
    }
    public static SObject op1_portp(SObject arg) {
        return Factory.makeBoolean(arg.isPort());
    }
    public static SObject op1_structurep(SObject arg) {
        return Factory.makeBoolean(arg.isStructure());
    }
    public static SObject op1_bytevector_likep(SObject arg) {
        return Factory.makeBoolean(arg is SByteVL);
    }
    public static SObject op1_stringp(SObject arg) {
        return Factory.makeBoolean(arg.isString());
    }
    public static SObject op1_bytevectorp(SObject arg) {
        return Factory.makeBoolean(arg.isByteVector());
    }
    public static SObject op1_charp(SObject arg) {
        return Factory.makeBoolean(arg is SChar);
    }
    public static SObject op1_procedurep(SObject arg) {
        return Factory.makeBoolean(arg is Procedure);
    }

    /* Special Value predicates */
    /* ------------------------ */
    public static SObject op1_nullp(SObject arg) {
        return Factory.makeBoolean(arg == Factory.Null);
    }
    public static SObject op1_eof_objectp(SObject arg) {
        return Factory.makeBoolean(arg == Factory.Eof);
    }
    public static SObject op1_unspecifiedp(SObject arg) {
        return Factory.makeBoolean(arg == Factory.Unspecified);
    }
    public static SObject op1_undefinedp(SObject arg) {
        return Factory.makeBoolean(arg == Factory.Undefined);
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
        expect2(arg1.isChar(), arg1, arg2.isChar(), arg2, Constants.EX_CHARLT);
        return Factory.makeBoolean(((SChar)arg1).val < ((SChar)arg2).val);
    }

    public static SObject op2_char_le(SObject arg1, SObject arg2) {
        expect2(arg1.isChar(), arg1, arg2.isChar(), arg2, Constants.EX_CHARLE);
        return Factory.makeBoolean(((SChar)arg1).val <= ((SChar)arg2).val);
    }

    public static SObject op2_char_gt(SObject arg1, SObject arg2) {
        expect2(arg1.isChar(), arg1, arg2.isChar(), arg2, Constants.EX_CHARGT);
        return Factory.makeBoolean(((SChar)arg1).val > ((SChar)arg2).val);
    }

    public static SObject op2_char_ge(SObject arg1, SObject arg2) {
        expect2(arg1.isChar(), arg1, arg2.isChar(), arg2, Constants.EX_CHARGE);
        return Factory.makeBoolean(((SChar)arg1).val >= ((SChar)arg2).val);
    }

    public static SObject op2_char_equals(SObject arg1, SObject arg2) {
        expect2(arg1.isChar(), arg1, arg2.isChar(), arg2, Constants.EX_CHAREQ);
        return Factory.makeBoolean(((SChar)arg1).val == ((SChar)arg2).val);
    }

    public static SObject op1_char2integer(SObject arg) {
        expect1(arg.isChar(), arg, Constants.EX_CHAR2INT);
        return Factory.makeFixnum(((SChar)arg).val);
    }

    public static SObject op1_integer2char(SObject arg) {
        expect1(arg.isFixnum(), arg, Constants.EX_INT2CHAR);
        return Factory.makeChar(((SFixnum)arg).value);
    }

    /* Cell Operations */
    /* --------------- */

    public static SObject op1_make_cell(SObject arg) {
        return Factory.makePair(arg, Factory.False);
    }
    public static SObject op1_cell_ref(SObject arg) {
        return ((SPair)arg).first;
    }
    public static SObject op2_cell_set(SObject arg1, SObject arg2) {
        ((SPair)arg1).first = arg2;
        return Factory.Unspecified;
    }


    /* List operations */
    /* --------------- */

    public static SObject op2_cons(SObject arg1, SObject arg2) {
        return Factory.makePair(arg1, arg2);
    }

    public static SObject op1_car(SObject arg) {
        expect1(arg.isPair(), arg, Constants.EX_CAR);
        return ((SPair)arg).first;
    }
    public static SObject op1_car_pair(SObject arg) {
        return ((SPair)arg).first;
    }

    public static SObject op1_cdr(SObject arg) {
        expect1(arg.isPair(), arg, Constants.EX_CDR);
        return ((SPair)arg).rest;
    }
    public static SObject op1_cdr_pair(SObject arg) {
        return ((SPair)arg).rest;
    }

    public static SObject op2_set_car(SObject arg1, SObject arg2) {
        expect2(arg1.isPair(), arg1, true, arg2, Constants.EX_SETCAR);
        ((SPair)arg1).first = arg2;
        return Factory.Unspecified;
    }
    public static SObject op2_set_car_pair(SObject arg1, SObject arg2) {
        ((SPair)arg1).first = arg2;
        return Factory.Unspecified;
    }

    public static SObject op2_set_cdr(SObject arg1, SObject arg2) {
        expect2(arg1.isPair(), arg1, true, arg2, Constants.EX_SETCDR);
        ((SPair)arg1).rest = arg2;
        return Factory.Unspecified;
    }
    public static SObject op2_set_cdr_pair(SObject arg1, SObject arg2) {
        ((SPair)arg1).rest = arg2;
        return Factory.Unspecified;
    }

    /* VectorLike and ByteVectorLike operations */
    /* ---------------------------------------- */

    public static SObject op1_typetag(SObject arg) {
        expect1(arg.isVectorLike() || arg.isByteVectorLike() || arg.isProcedure(), arg,
                Constants.EX_TYPETAG);
        if (arg is SVL) {
            return Factory.makeFixnum(((SVL)arg).tag);
        } else if (arg is SByteVL) {
            return Factory.makeFixnum(((SByteVL)arg).tag);
        } else {
            return Factory.makeFixnum(((Procedure)arg).tag);
        }
    }
    public static SObject op2_typetag_set(SObject arg1, SObject arg2) {
        expect2(arg1.isVectorLike() || arg1.isByteVectorLike() || arg1.isProcedure(), arg1,
                arg2.isFixnum(), arg2,
                Constants.EX_TYPETAGSET);
        if (arg1 is SVL) {
            ((SVL)arg1).tag = ((SFixnum)arg2).value;
        } else if (arg1 is SByteVL) {
            ((SByteVL)arg1).tag = ((SFixnum)arg2).value;
        } else if (arg1 is Procedure) {
            ((Procedure)arg1).tag = ((SFixnum)arg2).value;
        }
        return Factory.Unspecified;
    }
        
    /* VectorLike operations */
    /* --------------------- */

    public static SObject op1_vector_like_length(SObject arg) {
        expect1(arg.isVectorLike(), arg, Constants.EX_VLLEN);
        return Factory.makeFixnum(((SVL)arg).length());
    }
    public static SObject op2_vector_like_ref(SObject arg1, SObject arg2) {
        expect2(arg1.isVectorLike(), arg1,
                arg2.isFixnum(), arg2,
                Constants.EX_VLREF);
        rangeCheckVL(arg1, arg2, Constants.EX_VLREF);
        return ((SVL)arg1).elementAt(((SFixnum)arg2).value);
    }

    public static SObject op3_vector_like_set(SObject arg1, SObject arg2,
                                           SObject arg3) {
        expect2(arg1.isVectorLike(), arg1,
                arg2.isFixnum(), arg2,
                Constants.EX_VLSET);
        rangeCheckVL(arg1, arg2, Constants.EX_VLSET);
        ((SVL)arg1).setElementAt(((SFixnum)arg2).value, arg3);
        return Factory.Unspecified;
    }

    /* Vector operations */
    /* ----------------- */

    public static SObject op2_make_vector(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, true, arg2, Constants.EX_MKVL);
        return Factory.makeVector(((SFixnum)arg1).value, arg2);
    }

    public static SObject op1_vector_length(SObject arg) {
        expect1(arg.isVector(), arg, Constants.EX_VECTOR_LENGTH);
        return Factory.makeFixnum(((SVL)arg).length());
    }
    public static SObject op1_vector_length_vec(SObject arg) {
        return Factory.makeFixnum(((SVL)arg).length());
    }

    public static SObject op2_vector_ref(SObject arg1, SObject arg2) {
        expect2(arg1.isVector(), arg1, arg2.isFixnum(), arg2, Constants.EX_VECTOR_REF);
        rangeCheckVL(arg1, arg2, Constants.EX_VLREF);
        return ((SVL)arg1).elementAt(((SFixnum)arg2).value);;
    }
    public static SObject op2_vector_ref_trusted(SObject arg1, SObject arg2) {
        return ((SVL)arg1).elementAt(((SFixnum)arg2).value);;
    }

    public static SObject op3_vector_set(SObject arg1, SObject arg2, SObject arg3) {
        expect3(arg1.isVector(), arg1, arg2.isFixnum(), arg2, true, arg3, Constants.EX_VECTOR_SET);
        rangeCheckVL(arg1, arg2, arg3, Constants.EX_VLSET);
        ((SVL)arg1).setElementAt(((SFixnum)arg2).value, arg3);
        return Factory.Unspecified;
    }
    public static SObject op3_vector_set_trusted(SObject arg1, SObject arg2, SObject arg3) {
        ((SVL)arg1).setElementAt(((SFixnum)arg2).value, arg3);
        return Factory.Unspecified;
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
        expect1(arg.isProcedure(), arg, Constants.EX_PROCEDURE_LENGTH);
        Procedure p = (Procedure) arg;
        // Procedure length is number of rib slots plus 2 (code and constants)
        return Factory.wrap(p.rib.Length + 2);
    }

    public static SObject op1_make_procedure(SObject arg) {
        expect1(arg.isFixnum(), arg, Constants.EX_MKVL);
        int a = ((SFixnum)arg).value - 2;
        SObject[] env = new SObject[a];
        env[0] = Factory.False;
        for (int i = 1; i < a; ++i) {
            env[i] = Factory.Null; // So says Petit Larceny
        }
        return new Procedure(CodeVector.NoCode,
                             Factory.makeVector(0, Factory.False),
                             env);
    }

    public static SObject op2_procedure_ref(SObject arg1, SObject arg2) {
        expect2(arg1.isProcedure(), arg1, arg2.isFixnum(), arg2, Constants.EX_PROCEDURE_REF);
        Procedure p = (Procedure) arg1;
        int b = ((SFixnum)arg2).value;
        
        if (b == 0) {
            // Code vector
            return p.getCode();
        } else if (b == 1) {
            // Constant vector
            return p.constantvector;
        } else if (b > 1) {
            b = b - 2;
            if (b < p.rib.Length) {
                return p.rib[b];
            }
        }
        Exn.fault(Constants.EX_PROCEDURE_REF, "procedure-ref: bad index", arg1, arg2);
        return null;
    }

    public static SObject op3_procedure_set(SObject arg1, SObject arg2, SObject arg3) {
        expect3(arg1.isProcedure(), arg1, arg2.isFixnum(), arg2, true, arg3,
                Constants.EX_PROCEDURE_SET);
        Procedure p = (Procedure) arg1;
        int b = ((SFixnum)arg2).value;

        // System.Console.WriteLine("** (procedure-set! {0} {1} {2})", arg1, arg2, arg3);

        if (b == 0) {
            // "code vector"
            p.setCode(arg3);
            return Factory.Unspecified;
        } else if (b == 1) {
            if (arg3.isVector()) {
                p.setConstants((SVL)arg3);
                return Factory.Unspecified;
            } else {
                Exn.fault(Constants.EX_PROCEDURE_SET, "not a vector", arg1, arg2, arg3);
                return null;
            }
        } else if (b > 1) {
            int bb = b - 2;
            if (bb < p.rib.Length) {
                p.rib[bb] = arg3;
                return Factory.Unspecified;
            }
        }
        Exn.fault(Constants.EX_PROCEDURE_SET, "procedure-set!: bad index " + b, arg1, arg2, arg3);
        return null;
    }

    /* String operations */
    /* ----------------- */

    public static SObject op1_make_string(SObject arg) {
        expect1(arg.isFixnum(), arg, Constants.EX_MKBVL);
        return Factory.makeString(((SFixnum)arg).value, (char)0);
    }
    public static SObject op2_make_string(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isChar(), arg2, Constants.EX_MKBVL);
        return Factory.makeString(((SFixnum)arg1).value, ((SChar)arg2).val);
    }

    public static SObject op1_string_length(SObject arg) {
        expect1(arg.isString(), arg, Constants.EX_STRING_LENGTH);
        return Factory.makeFixnum(((SByteVL)arg).length());
    }
    public static SObject op1_string_length_str(SObject arg) {
        return Factory.makeFixnum(((SByteVL)arg).length());
    }

    public static SObject op2_string_ref(SObject arg1, SObject arg2) {
        expect2(arg1.isString(), arg1, arg2.isFixnum(), arg2, Constants.EX_STRING_REF);
        rangeCheckBVL(arg1, arg2, Constants.EX_STRING_REF);
        return Factory.makeChar(((SByteVL)arg1).elements[((SFixnum)arg2).value]);
    }
    public static SObject op2_string_ref_trusted(SObject arg1, SObject arg2) {
        return Factory.makeChar(((SByteVL)arg1).elements[((SFixnum)arg2).value]);
    }

    public static SObject op3_string_set(SObject arg1, SObject arg2, SObject arg3) {
        expect3(arg1.isString(), arg1, arg2.isFixnum(), arg2, arg3.isChar(), arg3,
                Constants.EX_STRING_SET);
        rangeCheckBVL(arg1, arg2, arg3, Constants.EX_STRING_SET);
        SByteVL a = (SByteVL) arg1;
        a.elements[((SFixnum)arg2).value] = (byte)((SChar)arg3).val;
        return Factory.Unspecified;
    }
    public static SObject op3_string_set_trusted(SObject arg1, SObject arg2, SObject arg3) {
        SByteVL a = (SByteVL) arg1;
        a.elements[((SFixnum)arg2).value] = (byte)((SChar)arg3).val;
        return Factory.Unspecified;
    }

    /* Bytevector operations */
    /* --------------------- */

    public static SObject op1_make_bytevector(SObject arg) {
        expect1(arg.isFixnum(), arg, Constants.EX_MKBVL);
        return Factory.makeByteVector(((SFixnum)arg).intValue(), (byte)0);
    }
    public static SObject op1_bytevector_length(SObject arg) {
        expect1(arg.isByteVector(), arg, Constants.EX_BYTEVECTOR_LENGTH);
        return Factory.makeFixnum(((SByteVL)arg).length());
    }
    public static SObject op2_bytevector_ref(SObject arg1, SObject arg2) {
        expect2(arg1.isByteVector(), arg1, arg2.isFixnum(), arg2, Constants.EX_BYTEVECTOR_REF);
        rangeCheckBVL(arg1, arg2, Constants.EX_BYTEVECTOR_REF);
        return ((SByteVL)arg1).elementAt(((SFixnum)arg2).value);
    }
    public static SObject op3_bytevector_set(SObject arg1, SObject arg2,
                                          SObject arg3) {
        expect3(arg1.isByteVector(), arg1, arg2.isFixnum(), arg2, arg3.isFixnum(), arg3, 
                Constants.EX_BYTEVECTOR_SET);
        rangeCheckBVL(arg1, arg2, arg3, Constants.EX_BYTEVECTOR_SET);
        ((SByteVL)arg1).setElementAt
            (((SFixnum)arg2).value,
             ((SFixnum)arg3).value);
        return Factory.Unspecified;
    }
    public static SObject op2_bytevector_equal(SObject arg1, SObject arg2) {
        expect2(arg1.isByteVector(), arg1, arg2.isByteVector(), arg2, -1);

        byte[] bytes1 = ((SByteVL)arg1).elements;
        byte[] bytes2 = ((SByteVL)arg2).elements;
        if (bytes1.Length != bytes2.Length) return Factory.False;
        for (int i = 0; i < bytes1.Length; ++i) {
            if (bytes1[i] != bytes2[i]) return Factory.False;
        }
        return Factory.True;
    }

    public static SObject op2_bytevector_fill(SObject arg1, SObject arg2) {
        expect2(arg1.isByteVector(), arg1, arg2.isFixnum(), arg2, Constants.EX_BVFILL);
        ((SByteVL)arg1).fill((byte)((SFixnum)arg2).value);
        return Factory.Unspecified;
    }

    /* Bytevector-likoe operations */
    /* -------------------------- */

    public static SObject op1_bytevector_like_length(SObject arg) {
        expect1(arg.isByteVectorLike(), arg, Constants.EX_BVLLEN);
        return Factory.makeFixnum(((SByteVL)arg).length());
    }
    public static SObject op2_bytevector_like_ref(SObject arg1, SObject arg2) {
        expect2(arg1.isByteVectorLike(), arg1, arg2.isFixnum(), arg2, Constants.EX_BVLREF);
        rangeCheckBVL(arg1, arg2, Constants.EX_BVLREF);
        return ((SByteVL)arg1).elementAt(((SFixnum)arg2).value);
    }
    public static SObject op3_bytevector_like_set(SObject arg1, SObject arg2, SObject arg3) {
        expect3(arg1.isByteVectorLike(), arg1, arg2.isFixnum(), arg2, arg3.isFixnum(), arg3,
                Constants.EX_BVLSET);
        rangeCheckBVL(arg1, arg2, arg3, Constants.EX_BVLSET);
        ((SByteVL)arg1).setElementAt
            (((SFixnum)arg2).value,
             ((SFixnum)arg3).value);
        return Factory.Unspecified;
    }
    public static SObject op2_sys_bvlcmp(SObject arg1, SObject arg2) {
        expect2(arg1.isByteVectorLike(), arg1, arg2.isByteVectorLike(), arg2, -1);
        byte[] a = ((SByteVL) arg1).elements;
        byte[] b = ((SByteVL) arg2).elements;

        int upper = (a.Length < b.Length) ? a.Length : b.Length;
        for (int i = 0; i < upper; ++i) {
            if (a[i] == b[i]) {
                continue;
            } else {
                return Factory.makeFixnum(a[i] - b[i]);
            }
        }
        return Factory.makeFixnum(a.Length - b.Length);
    }
    
    /* Fixnum Ops                */
    /* ------------------------- */

    public static SObject op1_fxzerop(SObject arg) {
        int a = ((SFixnum)arg).value;
        return Factory.makeBoolean(a == 0);
    }
    public static SObject op1_fxpositivep(SObject arg) {
        expect1(arg.isFixnum(), arg, Constants.EX_FXPOSITIVE);
        int a = ((SFixnum)arg).value;
        return Factory.makeBoolean(a > 0);
    }
    public static SObject op1_fxnegativep(SObject arg) {
        expect1(arg.isFixnum(), arg, Constants.EX_FXNEGATIVE);
        int a = ((SFixnum)arg).value;
        return Factory.makeBoolean(a < 0);
    }
    public static SObject op1_fxnegative(SObject arg) {
        expect1(arg.isFixnum(), arg, Constants.EX_FXNEG);
        int a = ((SFixnum)arg).value;
        if (!SFixnum.inFixnumRange(-a)) {
            Exn.fault(Constants.EX_FXNEG, "result not a fixnum", arg);
        }
        return Factory.makeNumber(-a);
    }
    public static SObject op2_fxplus(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_FXADD);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        int c = a + b;
        if (!SFixnum.inFixnumRange(c)) {
            Exn.fault(Constants.EX_FXADD, "result not a fixnum", arg1, arg2);
        }
        return Factory.makeNumber(c);
    }
    public static SObject op2_fxminus(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_FXSUB);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        int c = a - b;
        if (!SFixnum.inFixnumRange(c)) {
            Exn.fault(Constants.EX_FXSUB, "result not a fixnum", arg1, arg2);
        }
        return Factory.makeNumber(c);
    }
	public static SObject op2_fxmul(SObject arg1, SObject arg2) {
		expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_FXMUL);
		int a = ((SFixnum)arg1).value;
		int b = ((SFixnum)arg2).value;
		int c = a * b;
		if (!SFixnum.inFixnumRange(c)) {
			Exn.fault(Constants.EX_FXMUL, "result not a fixnum", arg1, arg2);
		}
		return Factory.makeNumber(c);
	}
    public static SObject op2_fxless(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_FXLT);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        return Factory.makeBoolean(a < b);
    }
    public static SObject op2_fxgreater(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_FXGT);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        return Factory.makeBoolean(a > b);
    }
    public static SObject op2_fxless_equal(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_FXLE);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        return Factory.makeBoolean(a <= b);
    }
    public static SObject op2_fxgreater_equal(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_FXGE);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        return Factory.makeBoolean(a >= b);
    }
    public static SObject op2_fxequal(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_FXEQ);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        return Factory.makeBoolean(a == b);
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
        expect1(arg.isFixnum(), arg, Constants.EX_LOGNOT);
        int a = ((SFixnum)arg).value;
        return Factory.makeFixnum(~a);
    }
    public static SObject op2_logand(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_LOGAND);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        return Factory.makeFixnum(a & b);
    }
    public static SObject op2_logior(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_LOGIOR);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        return Factory.makeFixnum(a | b);
    }
    public static SObject op2_logxor(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_LOGXOR);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        return Factory.makeFixnum(a ^ b);
    }
    public static SObject op2_lsh(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_LSH);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        return Factory.makeFixnum(a << b);
    }
    public static SObject op2_rsh(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_RSHA);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        return Factory.makeFixnum(a >> b);
    }
    public static SObject op2_rsha(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_RSHA);
        int a = ((SFixnum)arg1).value;
        int b = ((SFixnum)arg2).value;
        return Factory.makeFixnum(a >> b);
    }
    public static SObject op2_rshl(SObject arg1, SObject arg2) {
        expect2(arg1.isFixnum(), arg1, arg2.isFixnum(), arg2, Constants.EX_RSHL);
        uint a = (uint)((SFixnum)arg1).value << 2;
        int b = ((SFixnum)arg2).value;
        // return Factory.makeFixnum((int)(a >> b));
        return Factory.makeFixnum((int)(a >> b) >> 2);
    }
    // rot

    /* Arithmetic operations */
    /* --------------------- */

    public static SObject op1_real_part(SObject arg) {
        if (arg is SFixnum) {
            return arg;
        } else if (arg is SVL) {
            SVL a = (SVL) arg;
            if (a.tag == Tags.BignumTag 
                | a.tag == Tags.RatnumTag) {
                return a;
            } else if (a.tag == Tags.RectnumTag) {
                return Number.rectRealPart(a);
            }
        } else if (arg is SByteVL) {
            SByteVL a = (SByteVL) arg;
            if (a.tag == Tags.FlonumTag) {
                return a;
            } else if (a.tag == Tags.CompnumTag) {
                return Number.compRealPart(a);
            }
        }
        Exn.fault(Constants.EX_IMAGPART,
                  "not a number");
        return Factory.Unspecified;
    }

    public static SObject op1_imag_part(SObject arg) {
        if (arg is SFixnum) {
            return Factory.makeFixnum(0);
        } else if (arg is SVL) {
            SVL a = (SVL) arg;
            if (a.tag == Tags.BignumTag 
                | a.tag == Tags.RatnumTag) {
                return Factory.makeFixnum(0);
            } else if (a.tag == Tags.RectnumTag) {
                return Number.rectImagPart(a);
            }
        } else if (arg is SByteVL) {
            SByteVL a = (SByteVL) arg;
            if (a.tag == Tags.FlonumTag) {
                return Factory.makeFixnum(0);
            } else if (a.tag == Tags.CompnumTag) {
                return Number.compImagPart(a);
            }
        }
        Exn.fault(Constants.EX_IMAGPART,
                  "not a number");
        return Factory.Unspecified;
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
        return Factory.makeBoolean
            (((SFixnum)arg1).value == ((SFixnum)arg2).value);
    }
    public static SObject op2_less_fix_fix(SObject arg1, SObject arg2) {
        return Factory.makeBoolean
            (((SFixnum)arg1).value < ((SFixnum)arg2).value);
    }
    public static SObject op2_lessequal_fix_fix(SObject arg1, SObject arg2) {
        return Factory.makeBoolean
            (((SFixnum)arg1).value <= ((SFixnum)arg2).value);
    }
    public static SObject op2_greater_fix_fix(SObject arg1, SObject arg2) {
        return Factory.makeBoolean
            (((SFixnum)arg1).value > ((SFixnum)arg2).value);
    }
    public static SObject op2_greaterequal_fix_fix(SObject arg1, SObject arg2) {
        return Factory.makeBoolean
            (((SFixnum)arg1).value >= ((SFixnum)arg2).value);
    }

}
}

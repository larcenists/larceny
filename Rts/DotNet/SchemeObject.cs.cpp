#include "macros.cpp"

using System;
using System.Collections;
using System.IO;
using System.Text;
using Scheme.RT;
using Scheme.Rep;

namespace Scheme.Rep {

    // -------------------------------------------
    // Tags
    // -------------------------------------------
    public sealed class Tags {
        public static readonly int VectorTag = Constants.VEC_SUBTAG >>2;
        public static readonly int RectnumTag = Constants.RECT_SUBTAG >>2;
        public static readonly int RatnumTag = Constants.RAT_SUBTAG >>2;
        public static readonly int StructureTag = Constants.STRUCT_SUBTAG >>2;
        public static readonly int PortTag = 4; // FIXME: From Lib/Common/typetags.sch
        public static readonly int SymbolTag = Constants.SYM_SUBTAG >>2;

        public static readonly int ByteVectorTag = Constants.BVEC_SUBTAG >>2;
        public static readonly int StringTag = Constants.STR_SUBTAG >>2;
        public static readonly int CompnumTag = Constants.COMP_SUBTAG >>2;
        public static readonly int BignumTag = Constants.BIG_SUBTAG >>2;
        public static readonly int FlonumTag = Constants.FLO_SUBTAG >>2;
    }

    // -------------------------------------------
    // SObject
    // -------------------------------------------
    public abstract class SObject {
        // Debugging
        public override string ToString() {
            System.IO.StringWriter sw = new System.IO.StringWriter();
            this.write(sw);
            return sw.ToString();
        }
        public virtual void write(TextWriter w) {
            w.Write("#<object:");
            w.Write(this.GetType());
            w.Write(">");
        }

        // Predicates
        PREDF(isNumber)
        PREDF(isFixnum)
        PREDF(isFlonum)
        PREDF(isBignum)
        PREDF(isRatnum)
        PREDF(isRectnum)
        PREDF(isCompnum)

        PREDF(isComplex)
        PREDF(isReal)
        PREDF(isRational)
        PREDF(isInteger)
        PREDF(isExact)
        PREDF(isInexact)

        PREDF(isImmediate)
        PREDF(isSymbol)
        PREDF(isChar)
        PREDF(isPair)
        PREDF(isProcedure)

        PREDF(isVectorLike)
        PREDF(isVector)
        PREDF(isPort)
        PREDF(isStructure)
            
        PREDF(isByteVectorLike)
        PREDF(isByteVector)
        PREDF(isString)

        // ===================
        //   Operations
        // ===================

        // General Predicates
        PREDSF(op_numberp)
        PREDSF(op_fixnump)
        PREDSF(op_flonump)
        PREDSF(op_bignump)
        PREDSF(op_ratnump)
        PREDSF(op_rectnump)
        PREDSF(op_compnump)

        PREDSF(op_complexp)
        PREDSF(op_realp)
        PREDSF(op_rationalp)
        PREDSF(op_integerp)

        VEX1(op_exactp, EX_EXACTP)
        VEX1(op_inexactp, EX_INEXACTP)

        PREDSF(op_immediatep)
        PREDSF(op_symbolp)
        PREDSF(op_charp)
        PREDSF(op_pairp)
        PREDSF(op_procedurep)

        PREDSF(op_vector_likep)
        PREDSF(op_vectorp)
        PREDSF(op_portp)
        PREDSF(op_structurep)

        PREDSF(op_bytevector_likep)
        PREDSF(op_bytevectorp)
        PREDSF(op_stringp)

        public SObject op_eqp(SObject arg2) { return Factory.wrap(this == arg2); }
        public SObject op_not() { return Factory.wrap(this != Factory.False); }
        public SObject op_nullp() { return Factory.wrap(this == Factory.Null); }
        public SObject op_eof_objectp() { return Factory.wrap(this == Factory.Eof); }
        public SObject op_unspecifiedp() { 
            return Factory.wrap(this == Factory.Unspecified); 
        }
        public SObject op_undefinedp() {
            return Factory.wrap(this == Factory.Undefined);
        }
        
        // Misc
        public SObject op_creg() {
            return Cont.getCC();
        }
        public SObject op_creg_set() {
            Cont.setCC(this);
            return Factory.Unspecified;
        }
        public SObject op_break() {
            Exn.fault(Constants.EX_BREAKPOINT);
            return Factory.Impossible;
        }
        public SObject op_gc_counter() {
            Exn.fault(Constants.EX_UNSUPPORTED);
            return Factory.Impossible;
        }

        // Data
        public SObject op_unspecified() {
            return Factory.Unspecified;
        }
        public SObject op_undefined() {
            return Factory.Undefined;
        }
        public SObject op_eof_object() {
            return Factory.Eof;
        }
        
        // Chars
        VEX2X(op_char_lt, EX_CHARLT, SChar)
        VEX2X(op_char_le, EX_CHARLE, SChar)
        VEX2X(op_char_ge, EX_CHARGE, SChar)
        VEX2X(op_char_gt, EX_CHARGT, SChar)
        VEX2X(op_char_equals, EX_CHAREQ, SChar)

        VEX1(op_char2integer, EX_CHAR2INT)
        VEX1(op_integer2char, EX_INT2CHAR)

        // Cell Operations
        public SObject op_make_cell() { return Factory.makePair(this, Factory.False); }
        VEX1(op_cell_ref, EX_CAR)
        VEX2(op_cell_set, EX_SETCAR)

        // Pair Operations
        public SObject op_cons(SObject arg2) { return Factory.makePair(this, arg2); }
        VEX1(op_car, EX_CAR)
        VEX1(op_car_pair, EX_CAR)
        VEX1(op_cdr, EX_CDR)
        VEX1(op_cdr_pair, EX_CDR)

        VEX2(op_set_car, EX_SETCAR)
        VEX2(op_set_car_pair, EX_SETCAR)
        VEX2(op_set_cdr, EX_SETCDR)
        VEX2(op_set_cdr_pair, EX_SETCDR)
        
        // Typetag Operations
        VEX1(op_typetag, EX_TYPETAG)
        VEX2X(op_typetag_set, EX_TYPETAGSET, STagged)

        // Vectorlike Operations
        VEX1(op_vector_like_length, EX_VLLEN)
        VEX2X(op_vector_like_ref, EX_VLREF, SVL)
        VEX3X(op_vector_like_set, EX_VLSET, SVL)

        // Vector Operations
        VEX2(op_make_vector, EX_MKVL)
        VEX1(op_vector_length, EX_VECTOR_LENGTH)
        VEX2X(op_vector_ref, EX_VECTOR_REF, SVL)
            //        VEX2X(op_vector_ref_trusted, EX_VECTOR_REF, SVL)
        VEX3X(op_vector_set, EX_VECTOR_SET, SVL)
            //        VEX3X(op_vector_set_trusted, EX_VECTOR_SET, SVL)

        // Procedure Operations
        VEX1(op_procedure_length, EX_PROCEDURE_LENGTH)
        VEX1(op_make_procedure, EX_MKVL)
        VEX2X(op_procedure_ref, EX_PROCEDURE_REF, Procedure)
        VEX3X(op_procedure_set, EX_PROCEDURE_SET, Procedure)

        // String Operations
        VEX2X(op_make_string, EX_MKBVL, SFixnum)
        VEX1(op_string_length, EX_STRING_LENGTH)
        VEX2X(op_string_ref, EX_STRING_REF, SByteVL)
            //        VEX2X(op_string_ref_trusted, EX_STRING_REF, SByteVL)
        VEX3X(op_string_set, EX_STRING_SET, SByteVL)
            //        VEX3X(op_string_set_trusted, EX_STRING_SET, SByteVL)

        // Bytevector Operations
        VEX1(op_make_bytevector, EX_MKBVL)
        VEX1(op_bytevector_length, EX_BYTEVECTOR_LENGTH)
        VEX2X(op_bytevector_ref, EX_BYTEVECTOR_REF, SByteVL)
        VEX3X(op_bytevector_set, EX_BYTEVECTOR_SET, SByteVL)
        VEX2X(op_bytevector_equal, EX_UNSUPPORTED, SByteVL)
        VEX2X(op_bytevector_fill, EX_BVFILL, SByteVL)

        // Bytevector-like Operations
        VEX1(op_bytevector_like_length, EX_BVLLEN)
        VEX2X(op_bytevector_like_ref, EX_BVLREF, SByteVL)
        VEX3X(op_bytevector_like_set, EX_BVLSET, SByteVL)
        VEX2X(op_sys_bvlcmp, EX_UNSUPPORTED, SByteVL)

        // Fixnum Operations
        VEX1(op_fxzerop, EX_UNSUPPORTED)
        VEX1(op_fxpositivep, EX_FXPOSITIVE)
        VEX1(op_fxnegativep, EX_FXNEGATIVE)
        VEX1(op_fxnegative, EX_FXNEG)
        VEX2X(op_fxplus, EX_FXADD, SFixnum)
        VEX2X(op_fxminus, EX_FXSUB, SFixnum)
        VEX2X(op_fxmul, EX_FXMUL, SFixnum)
        VEX2X(op_fxless, EX_FXLT, SFixnum)
        VEX2X(op_fxgreater, EX_FXGT, SFixnum)
        VEX2X(op_fxless_equal, EX_FXLE, SFixnum)
        VEX2X(op_fxgreater_equal, EX_FXGE, SFixnum)
        VEX2X(op_fxequal, EX_FXEQ, SFixnum)

        public SObject op_most_positive_fixnum() {
            return Factory.makeFixnum(SFixnum.MAX);
        }
        public SObject op_most_negative_fixnum() {
            return Factory.makeFixnum(SFixnum.MIN);
        }
            
        // Logical Operations
        VEX1(op_lognot, EX_LOGNOT)
        VEX2X(op_logand, EX_LOGAND, SFixnum)
        VEX2X(op_logior, EX_LOGIOR, SFixnum)
        VEX2X(op_logxor, EX_LOGXOR, SFixnum)
        VEX2X(op_lsh, EX_LSH, SFixnum)
        VEX2X(op_rsh, EX_RSHA, SFixnum)
        VEX2X(op_rsha, EX_RSHA, SFixnum)
        VEX2X(op_rshl, EX_RSHL, SFixnum)

        // Arithmetic Operations
        VEX1(op_real_part, EX_REALPART)
        VEX1(op_imag_part, EX_IMAGPART)

        // -------------------
        // Special Operations
        // -------------------

        //SPECIALVEX1(op_enable_interrupts, EX_EINTR)
        //public void op_disable_interrupts() {
        //    if (Reg.interruptsEnabled) {
        //        Reg.interruptsEnabled = false;
        //        Reg.Result = Factory.makeFixnum((int)Reg.timer);
        //    } else {
        //        Reg.Result = Factory.makeBoolean(false);
        //    }
        //    Exn.checkSignals();
        //}

        //SPECIALVEX1(op_syscall)
        //
        //SPECIALVEX1(op_zerop)
        //SPECIALVEX2(op_eqvp)
        //SPECIALVEX2(op_numeric_equals)
        //SPECIALVEX2(op_less_than)
        //SPECIALVEX2(op_greater_than)
        //SPECIALVEX2(op_less_or_equal)
        //SPECIALVEX2(op_greater_or_equal)

        //SPECIALARITH2(op_plus)  // Declares op_plus_2 for SFixum, SVL, SByteVL
        //SPECIALARITH2(op_minus)
        //SPECIALARITH2(op_multiply)
        //SPECIALARITH2(op_divide)
        //SPECIALARITH2(op_quotient)
        //SPECIALARITH2(op_remainder)
        //SPECIALVEX1(op_truncate)
        //SPECIALVEX1(op_round)
        //SPECIALVEX1(op_negative)
        //SPECIALVEX1(op_exact2inexact)
        //SPECIALVEX1(op_inexact2exact)

        // THE STUFF I WAS TOO LAZY TO PORT
        public SObject op_vector_length_vec() {
            return Factory.makeFixnum(((SVL)this).length());
        }
        public SObject op_vector_ref_trusted(SObject arg2) {
            SObject arg1 = this;
            return ((SVL)arg1).elementAt(((SFixnum)arg2).value);;
        }
        public SObject op_vector_set_trusted(SObject arg2, SObject arg3) {
            SObject arg1 = this;
            ((SVL)arg1).setElementAt(((SFixnum)arg2).value, arg3);
            return Factory.Unspecified;
        }
        public SObject op_sys_partial_list__vector(SObject arg2) {
            SObject arg1 = this;
            int n = ((SFixnum)arg2).value;
            SObject[] items = new SObject[n];
            for (int i = 0; i < n; ++i) {
                items[i] = ((SPair)arg1).first;
                arg1 = ((SPair)arg1).rest;
            }
            return Factory.makeVector(items);
        }
        public SObject op_string_length_str() {
            SObject arg = this;
            return Factory.makeFixnum(((SByteVL)arg).length());
        }

        public SObject op_string_ref_trusted(SObject arg2) {
            SObject arg1 = this;
            return Factory.makeChar(((SByteVL)arg1).elements[((SFixnum)arg2).value]);
        }
        public SObject op_string_set_trusted(SObject arg2, SObject arg3) {
            SObject arg1 = this;
            SByteVL a = (SByteVL) arg1;
            a.elements[((SFixnum)arg2).value] = (byte)((SChar)arg3).val;
            return Factory.Unspecified;
        }
        public SObject op_plus_idx_idx(SObject arg2) {
            SObject arg1 = this;
            return Factory.makeNumber
                (((SFixnum)arg1).value + ((SFixnum)arg2).value);
        }
        public SObject op_minus_idx_idx(SObject arg2) {
            SObject arg1 = this;
            return Factory.makeNumber 
                (((SFixnum)arg1).value - ((SFixnum)arg2).value);
        }
        public SObject op_equal_fix_fix(SObject arg2) {
            SObject arg1 = this;
            return Factory.makeBoolean
                (((SFixnum)arg1).value == ((SFixnum)arg2).value);
        }
        public SObject op_less_fix_fix(SObject arg2) {
            SObject arg1 = this;
            return Factory.makeBoolean
                (((SFixnum)arg1).value < ((SFixnum)arg2).value);
        }
        public SObject op_lessequal_fix_fix(SObject arg2) {
            SObject arg1 = this;
            return Factory.makeBoolean
                (((SFixnum)arg1).value <= ((SFixnum)arg2).value);
        }
        public SObject op_greater_fix_fix(SObject arg2) {
            SObject arg1 = this;
            return Factory.makeBoolean
                (((SFixnum)arg1).value > ((SFixnum)arg2).value);
        }
        public SObject op_greaterequal_fix_fix(SObject arg2) {
            SObject arg1 = this;
            return Factory.makeBoolean
                (((SFixnum)arg1).value >= ((SFixnum)arg2).value);
        }

    }

    // -------------------------------------------
    // SImmediate
    // -------------------------------------------
    public sealed class SImmediate : SObject {
        public readonly string rep;
        public SImmediate(String rep) {
            this.rep = rep;
        }
        public override void write(TextWriter w) {
            w.Write(rep);
        }

        // ------------------
        public override bool isImmediate() { return true; }
        public override SObject op_immediatep() { return Factory.True; }
    }

    // -------------------------------------------
    // SChar
    // -------------------------------------------
    public sealed class SChar : SObject {
        public static readonly int CHAR_COUNT = 256;
        public static readonly SChar[] characters = 
            new SChar[CHAR_COUNT];

        public char val;
        private SChar(char c) {
            this.val = c;
        }
        static SChar() {
            for (int i = 0; i < CHAR_COUNT; ++i) {
                characters[i] = new SChar((char)i);
            }
        }
        public static SChar makeChar(int c) {
            if (c >= 0 && c < CHAR_COUNT) {
                return characters[c];
            } else {
                Exn.internalError("not a valid char");
                return new SChar((char)c);
            }
        }
        public override void write(TextWriter w) {
            w.Write("#\\");
            w.Write(val);
        }

        // ------------------
        public override bool isChar() { return true; }
        public override SObject op_charp() { return Factory.True; }

        DISPATCH2(op_char_lt)
        DISPATCH2(op_char_le)
        DISPATCH2(op_char_gt)
        DISPATCH2(op_char_ge)
        DISPATCH2(op_char_equals)
        OVERRIDE2X(op_char_lt, SChar) {
            return Factory.wrap(arg1.val < this.val);
        }
        OVERRIDE2X(op_char_le, SChar) {
            return Factory.wrap(arg1.val <= this.val);
        }
        OVERRIDE2X(op_char_gt, SChar) {
            return Factory.wrap(arg1.val > this.val);
        }
        OVERRIDE2X(op_char_ge, SChar) {
            return Factory.wrap(arg1.val >= this.val);
        }
        OVERRIDE2X(op_char_equals, SChar) {
            return Factory.wrap(arg1.val == this.val);
        }

        IMPL1(op_char2integer) {
            return Factory.wrap(this.val);
        }

        OVERRIDE2X(op_make_string, SFixnum) {
            return Factory.makeString(arg1.value, this.val);
        }
    }

    // -------------------------------------------
    // The numeric representations
    // -------------------------------------------
    /*
     * Numbers are represented in the following ways:
     *     - fixnum  = SFixnum
     *     - bignum  = SByteVL/bignum
     *     - flonum  = SByteVL/flonum
     *     - ratnum  = SVL/ratnum
     *     - rectnum = SVL/rectnum
     *     - compnum = SVL/compnum
     */

    // -------------------------------------------
    // SFixnum
    // -------------------------------------------
    public sealed class SFixnum : SObject {
        public readonly int value;
        public static readonly SFixnum[] pool;
        public static readonly int maxPreAlloc = 16000;
        public static readonly int MAX = (1 << 29) - 1;
        public static readonly int MIN = -(1 << 29);
        public static readonly int BITS = 30;

        // Stores numbers -maxPreAlloc to +maxPreAlloc
        //          0 -> maxPreAlloc
        // -maxPreAlloc -> 0
        static SFixnum() {
            pool = new SFixnum[2 * maxPreAlloc + 1];
            for (int i = -maxPreAlloc; i <= maxPreAlloc; i++)
                pool[i + maxPreAlloc] = new SFixnum(i);                   
        }
        private SFixnum(int value) {
            this.value = value;
        }
        public override void write(TextWriter w) {
            w.Write(value);
        }
        public int intValue() {
            return value;
        }
        public static bool inFixnumRange(int n) {
            int highbits = n >> (BITS-1);
            return (highbits == 0 || highbits == -1);
        }
        public static bool inFixnumRange(long n) {
            return ((n >> 32 == 0) || (n >> 32 == -1))
                && inFixnumRange((int)n);
        }
        public static SFixnum makeFixnum(int val) {
            if (val >= -maxPreAlloc && val <= maxPreAlloc) {
                return pool[val + maxPreAlloc];
            } else
                return new SFixnum(val);
        }
        // ----------------
        
        public override bool isNumber() { return true; }
        public override bool isFixnum() { return true; }
        public override bool isComplex() { return true; }
        public override bool isReal() { return true; }
        public override bool isRational() { return true; }
        public override bool isInteger() { return true; }
        public override bool isExact() { return true; }

        public override SObject op_numberp() { return Factory.True; }
        public override SObject op_fixnump() { return Factory.True; }
        public override SObject op_complexp() { return Factory.True; }
        public override SObject op_realp() { return Factory.True; }
        public override SObject op_rationalp() { return Factory.True; }
        public override SObject op_integerp() { return Factory.True; }
        public override SObject op_exactp() { return Factory.True; }

        IMPL1(op_integer2char) {
            return Factory.makeChar(this.value);
        }
        OVERRIDE2X(op_typetag_set, STagged) {
            arg1.tag = this.value;
            return Factory.Unspecified;
        }
        OVERRIDE2X(op_vector_like_ref, SVL) {
            SObject[] elements = arg1.elements;
            int index = this.value;
            if (0 <= index && index < elements.Length) {
                return elements[index];
            } else {
                Exn.fault(Constants.EX_VLREF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        OVERRIDE3X(op_vector_like_set, SVL) {
            SObject[] elements = arg1.elements;
            int index = this.value;
            if (0 <= index && index < elements.Length) {
                elements[index] = arg3;
                return Factory.Unspecified;
            } else {
                Exn.fault(Constants.EX_VLSET, null, arg1, this, arg3);
                return Factory.Impossible;
            }
        }
        IMPL2(op_make_vector) {
            int length = this.value;
            if (0 <= length) {
                return Factory.makeVector(this.value, arg2);
            } else {
                Exn.fault(Constants.EX_MKVL, null, this, arg2);
                return Factory.Impossible;
            }
        }
        OVERRIDE2X(op_vector_ref, SVL) {
            SObject[] elements = arg1.elements;
            int index = this.value;
            if (0 <= index && index < elements.Length) {
                return elements[index];
            } else {
                Exn.fault(Constants.EX_VECTOR_REF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        OVERRIDE3X(op_vector_set, SVL) {
            SObject[] elements = arg1.elements;
            int index = this.value;
            if (0 <= index && index < elements.Length) {
                elements[index] = arg3;
                return Factory.Unspecified;
            } else {
                Exn.fault(Constants.EX_VECTOR_SET, null, arg1, this, arg3);
                return Factory.Impossible;
            }
        }
        IMPL1(op_make_procedure) {
            int a = this.value - 2;
            if (0 <= a) {
                SObject[] env = new SObject[a];
                env[0] = Factory.False;
                for (int i = 1; i < a; ++i) {
                    env[i] = Factory.Null; // So says Petit Larceny
                }
                return new Procedure(CodeVector.NoCode,
                                     Factory.makeVector(0, Factory.False),
                                     env);
            } else {
                Exn.fault(Constants.EX_MKVL, null, this);
                return Factory.Impossible;
            }
        }
        OVERRIDE2X(op_procedure_ref, Procedure) {
            Procedure p = arg1;
            int b = this.value;
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
            Exn.fault(Constants.EX_PROCEDURE_REF, null, arg1, this);
            return Factory.Impossible;
        }
        OVERRIDE3X(op_procedure_set, Procedure) {
            Procedure p = arg1;
            int b = this.value;
            if (b == 0) {
                // Code vector
                p.setCode(arg3);
                return Factory.Unspecified;
            } else if (b == 1) {
                // Constant vector
                if (arg3.isVector()) {
                    p.setConstants((SVL)arg3);
                    return Factory.Unspecified;
                } else {
                    Exn.fault(Constants.EX_PROCEDURE_SET, null, arg1, this, arg3);
                    return Factory.Impossible;
                }
            } else if (b > 1) {
                b = b - 2;
                if (b < p.rib.Length) {
                    p.rib[b] = arg3;
                    return Factory.Unspecified;
                }
            }
            Exn.fault(Constants.EX_PROCEDURE_SET, null, arg1, this, arg3);
            return Factory.Impossible;
        }

        DISPATCH2(op_make_string)

        IMPL1(op_make_bytevector) {
            return Factory.makeByteVector(this.value, (byte)0);
        }
        OVERRIDE2X(op_bytevector_ref, SByteVL) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            if (0 <= index && index < bytes.Length) {
                return Factory.wrap(bytes[index]);
            } else {
                Exn.fault(Constants.EX_BYTEVECTOR_REF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        OVERRIDE3X(op_bytevector_set, SByteVL) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            if (0 <= index && index < bytes.Length) {
                if (arg3 is SFixnum) {
                    bytes[index] = (byte)((SFixnum)arg3).value;
                    return Factory.Unspecified;
                }
            }
            Exn.fault(Constants.EX_BYTEVECTOR_SET, null, arg1, this, arg3);
            return Factory.Impossible;
        }
        OVERRIDE2X(op_bytevector_fill, SByteVL) {
            byte[] bytes = arg1.elements;
            byte fill = (byte) this.value;
            for (int i = 0; i < bytes.Length; ++i) {
               bytes[i] = fill;
            }
            return Factory.Unspecified;
        }
        
        OVERRIDE2X(op_bytevector_like_ref, SByteVL) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            if (0 <= index && index < bytes.Length) {
                return Factory.wrap(bytes[index]);
            } else {
                Exn.fault(Constants.EX_BVLREF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        OVERRIDE3X(op_bytevector_like_set, SByteVL) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            if (0 <= index && index < bytes.Length) {
                if (arg3 is SFixnum) {
                    bytes[index] = (byte)((SFixnum)arg3).value;
                    return Factory.Unspecified;
                }
            }
            Exn.fault(Constants.EX_BVLREF, null, arg1, this);
            return Factory.Impossible;
        }
        
        IMPL1(op_fxzerop) {
            return Factory.wrap(this.value == 0);
        }
        IMPL1(op_fxpositivep) {
            return Factory.wrap(this.value > 0);
        }
        IMPL1(op_fxnegativep) {
            return Factory.wrap(this.value < 0);
        }
        IMPL1(op_fxnegative) {
            int a = - this.value;
            if (!SFixnum.inFixnumRange(-a)) {
                Exn.fault(Constants.EX_FXNEG, "result not a fixnum", this);
                return Factory.Impossible;
            }
            return Factory.wrap(a);
        }
        DISPATCH2(op_fxplus)
        DISPATCH2(op_fxminus)
        DISPATCH2(op_fxmul)
        DISPATCH2(op_fxless)
        DISPATCH2(op_fxless_equal)
        DISPATCH2(op_fxgreater)
        DISPATCH2(op_fxgreater_equal)
        DISPATCH2(op_fxequal)

        OVERRIDE2X(op_fxplus, SFixnum) {
            int a = arg1.value, b = this.value; 
            int r = a + b; 
            if (!SFixnum.inFixnumRange(r)) {
                Exn.fault(Constants.EX_FXADD, null, arg1, this);
                return Factory.Impossible; 
            }
            return Factory.wrap(r);
        }
        OVERRIDE2X(op_fxminus, SFixnum) {
            int a = arg1.value, b = this.value; 
            int r = a - b; 
            if (!SFixnum.inFixnumRange(r)) {
                Exn.fault(Constants.EX_FXSUB, null, arg1, this); 
                return Factory.Impossible; 
            }
            return Factory.wrap(r);
        }
        OVERRIDE2X(op_fxmul, SFixnum) {
            int a = arg1.value, b = this.value;
            long r = a * b;
            if (!SFixnum.inFixnumRange(r)) {
                Exn.fault(Constants.EX_FXMUL, null, arg1, this);
                return Factory.Impossible;
            }
            return Factory.wrap((int)r);
        }
        OVERRIDE2X(op_fxless, SFixnum) {
            return Factory.wrap(arg1.value < this.value);
        }
        OVERRIDE2X(op_fxless_equal, SFixnum) {
            return Factory.wrap(arg1.value <= this.value);
        }
        OVERRIDE2X(op_fxgreater, SFixnum) {
            return Factory.wrap(arg1.value > this.value);
        }
        OVERRIDE2X(op_fxgreater_equal, SFixnum) {
            return Factory.wrap(arg1.value >= this.value);
        }
        OVERRIDE2X(op_fxequal, SFixnum) {
            return Factory.wrap(arg1.value == this.value);
        }

        IMPL1(op_lognot) {
            return Factory.wrap(~this.value);
        }
        DISPATCH2(op_logand)
        DISPATCH2(op_logior)
        DISPATCH2(op_logxor)
        DISPATCH2(op_lsh)
        DISPATCH2(op_rsh)
        DISPATCH2(op_rsha)
        DISPATCH2(op_rshl)

        OVERRIDE2X(op_logand, SFixnum) {
            return Factory.wrap(arg1.value & this.value);
        }
        OVERRIDE2X(op_logior, SFixnum) {
            return Factory.wrap(arg1.value | this.value);
        }
        OVERRIDE2X(op_logxor, SFixnum) {
            return Factory.wrap(arg1.value ^ this.value);
        }
        OVERRIDE2X(op_lsh, SFixnum) {
            int r = arg1.value << this.value;
            r = (r << 2) >> 2; // mask out top bits (w/ sign extend)
            return Factory.wrap(r);
        }
        OVERRIDE2X(op_rsh, SFixnum) {
            int r = arg1.value >> this.value;
            r = (r << 2) >> 2;
            return Factory.wrap(r);
        }
        OVERRIDE2X(op_rsha, SFixnum) {
            int r = arg1.value >> this.value;
            r = (r << 2) >> 2;
            return Factory.wrap(r);
        }
        OVERRIDE2X(op_rshl, SFixnum) {
            uint a = (uint)arg1.value << 2;
            int b = this.value;
            int r = (int)(a >> b) >> 2;
            return Factory.wrap((int)r);
        }

        IMPL1(op_real_part) {
            return this;
        }
        IMPL1(op_imag_part) {
            return Factory.wrap(0);
        }

        // Special Operations

        //SPECIALIMPL1(op_enable_interrupts) {
        //    int time = ((SFixnum)arg).value;
        //    if (time > 0) {
        //        Reg.interruptsEnabled = true;
        //        Reg.timer = time;
        //    } else {
        //        Exn.fault(Constants.EX_EINTR, null, this);
        //    }
        //    Reg.Result = Factory.Unspecified;
        //    Exn.checkSignals();
        //}
        //SPECIALIMPL1(op_syscall) {
        //    // subtract one 'cuz the first arg is just the value
        //    // to which we want to dispatch.
        //    int num_args = this.value - 1;
        //    Sys num_syscall = (Sys) ((SFixnum)Reg.register1).intValue();
        //    Syscall.dispatch(num_args, num_syscall);
        //}
        //SPECIALIMPL1(op_zerop) {
        //    Reg.Result = Factory.wrap(this.value == 0);
        //}
        //SPECIALDISPATCH2(op_eqvp)
        //SPECIALDISPATCH2(op_numeric_equals)
        //SPECIALDISPATCH2(op_less_then)
        //SPECIALDISPATCH2(op_less_or_equal)
        //SPECIALDISPATCH2(op_greater_than)
        //SPECIALDISPATCH2(op_greater_or_equal)
        //
        //SPECIALDISPATCH2(op_plus)
        //SPECIALDISPATCH2(op_minus)
        //SPECIALDISPATCH2(op_multiply)
        //SPECIALDISPATCH2(op_divide)
        //SPECIALDISPATCH2(op_quotient)
        //SPECIALDISPATCH2(op_remainder)

        //SPECIALIMPL1(op_truncate) {
        //    Reg.Result = this;
        //}
        //SPECIALIMPL1(op_round) {
        //    Reg.Result = this;
        //}
        //SPECIALIMPL1(op_negative) {
        //    Reg.Result = Factory.wrap(-this.value);
        //}
        //SPECIALIMPL1(op_exact2inexact) {
        //    Reg.Result = Factory.makeFlonum((double)this.value);
        //}
        //SPECIALIMPL1(op_inexact2exact) {
        //    Reg.Result = this;
        //}

        //SPECIALOVERRIDE2X(op_eqvp, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value == arg1.value);
        //}
        //SPECIALOVERRIDE2X(op_numeric_equals, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value == arg1.value);
        //}
        //SPECIALOVERRIDE2X(op_less_than, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value < arg1.value);
        //}
        //SPECIALOVERRIDE2X(op_less_or_equal, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value <= arg1.value);
        //}
        //SPECIALOVERRIDE2X(op_greater_than, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value > arg1.value);
        //}
        //SPECIALOVERRIDE2X(op_greater_or_equal, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value >= arg1.value);
        //}

        //SPECIALOVERRIDE2X(op_plus, SFixnum) {
        //    Reg.Result = Factory.wrap(arg1.value + this.value);
        //}
        //SPECIALOVERRIDE2X(op_minus, SFixnum) {
        //    Reg.Result = Factory.wrap(arg1.value - this.value);
        //}
        //SPECIALOVERRIDE2X(op_multiply, SFixnum) {
        //    long r = arg1.value * this.value;
        //    Reg.Result = Factory.wrap(r);
        //}
        //SPECIALOVERRIDE2X(op_divide, SFixnum) {
        //    int a = arg1.value, b = this.value;
        //    if (b == 0) {
        //        Exn.fault(Constants.EX_DIV, null, arg1, this);
        //        return;
        //    } else {
        //        if (a % b == 0) {
        //            Reg.Result = Factory.wrap(a / b);
        //            return;
        //        } else {
        //            Call.callMillicodeSupport2
        //                (Constants.MS_FIXNUM2RATNUM_DIV, arg1, this);
        //            return; // TAIL CALL
        //        }
        //    }
        //}
        //SPECIALOVERRIDE2X(op_quotient, SFixnum) {
        //    int a = arg1.value, b = this.value;
        //    if (b == 0) {
        //        Exn.fault(Constants.EX_QUOTIENT, null, arg1, this);
        //        return;
        //    } else {
        //        Reg.Result = Factory.wrap(a / b);
        //        return;
        //    }
        //}
        //SPECIALOVERRIDE2X(op_remainder, SFixnum) {
        //    int a = arg1.value, b = this.value;
        //    if (b == 0) {
        //        Exn.fault(Constants.EX_REMAINDER, null, arg1, this);
        //        return;
        //    } else {
        //        Reg.Result = Factory.wrap(a % b);
        //        return;
        //    }
        //}
    }

    // -------------------------------------------
    // STagged
    // -------------------------------------------
    public class STagged : SObject {
        public int tag;
        
        public override SObject op_typetag() {
            return Factory.wrap(this.tag);
        }
        public override SObject op_typetag_set(SObject arg2) {
            return arg2.op_typetag_set_2(this);
        }

        public void check_typetag(int tag, int excode) {
            if (this.tag != tag) Exn.fault(excode, null, this);
        }
        public void check_typetag(int tag, SObject arg2, int excode) {
            if (this.tag != tag) Exn.fault(excode, null, this, arg2);
        }
        public void check_typetag(int tag, SObject arg2, 
                                           SObject arg3, int excode) {
            if (this.tag != tag) Exn.fault(excode, null, this, arg2, arg3);
        }
    }

    // -------------------------------------------
    // SVL (vector-like)
    // -------------------------------------------
    public sealed class SVL : STagged {
        public readonly SObject[] elements;
        
        public SVL(int tag, int size, SObject fill) {
            this.tag = tag;
            this.elements = new SObject[size];
            for (int i = 0; i < size; ++i) {elements[i] = fill;};
        }
        public SVL(int tag, SObject[] vec) {
            this.tag = tag;
            this.elements = vec;
        }

        public int length() {
            return elements.Length;
        }
        public SObject elementAt(int index) {
            return elements[index];
        }
        public SObject elementAt(SFixnum index) {
            return elements[index.intValue()];
        }
        public void setElementAt(int index, SObject elt) {
            elements[index] = elt;
        }

        public override void write(TextWriter w) {
            if (this.tag == Tags.VectorTag) {
                w.Write("#<vector>");
            } else if (this.tag == Tags.RectnumTag) {
                w.Write("#<rectnum>");
            } else if (this.tag == Tags.RatnumTag) {
                w.Write("#<ratnum>");
            } else if (this.tag == Tags.PortTag) {
                w.Write("#<port>");
            } else if (this.tag == Tags.StructureTag) {
                w.Write("#<structure>");
            } else if (this.tag == Tags.SymbolTag) {
                SByteVL name = (SByteVL) this.elements[0];
                w.Write(name.asString());
            } else {
                w.Write("#<vectorlike: " + this.tag + ">");
            }
        }

        // -------------------

        public override bool isNumber() {
            return this.isRatnum() || this.isRectnum();
        }
        public override bool isRatnum() {
            return this.tag == Tags.RatnumTag;
        }
        public override bool isRectnum() {
            return this.tag == Tags.RectnumTag;
        }
        public override bool isComplex() {
            return this.isRatnum() || this.isRectnum();
        }
        public override bool isReal() {
            return this.isRatnum();
        }
        public override bool isRational() {
            return this.isRatnum();
        }
        public override bool isExact() {
            return this.isRatnum() || this.isRectnum();
        }
        
        public override bool isVectorLike() {
            return true;
        }
        public override bool isVector() {
            return this.tag == Tags.VectorTag;
        }
        public override bool isPort() {
            return this.tag == Tags.PortTag;
        }
        public override bool isStructure() {
            return this.tag == Tags.StructureTag;
        }
        public override bool isSymbol() {
            return this.tag == Tags.SymbolTag;
        }

        // -------------------
        IMPL1(op_numberp) {
            return Factory.wrap(this.isRatnum() || this.isRectnum());
        }
        IMPL1(op_ratnump) {
            return Factory.wrap(this.tag == Tags.RatnumTag);
        }
        IMPL1(op_rectnump) {
            return Factory.wrap(this.tag == Tags.RectnumTag);
        }
        IMPL1(op_complexp) {
            return Factory.wrap(this.isRatnum() || this.isRectnum());
        }
        IMPL1(op_realp) {
            return this.op_ratnump();
        }
        IMPL1(op_rationalp) {
            return this.op_ratnump();
        }
        IMPL1(op_exactp) {
            if (this.isNumber()) {
                return Factory.wrap(this.isRatnum() || this.isRectnum());
            } else {
                Exn.fault(Constants.EX_EXACTP, null, this);
                return Factory.Impossible;
            }
        }
        
        IMPL1(op_vector_likep) {
            return Factory.True;
        }
        IMPL1(op_vectorp) {
            return Factory.wrap(this.tag == Tags.VectorTag);
        }
        IMPL1(op_portp) {
            return Factory.wrap(this.tag == Tags.PortTag);
        }
        IMPL1(op_structurep) {
            return Factory.wrap(this.tag == Tags.StructureTag);
        }
        IMPL1(op_symbolp) {
            return Factory.wrap(this.tag == Tags.SymbolTag);
        }

        IMPL1(op_vector_like_length) {
            return Factory.wrap(elements.Length);
        }
        DISPATCH2(op_vector_like_ref)
        DISPATCH3(op_vector_like_set)

        IMPL1(op_vector_length) {
            check_typetag(Tags.VectorTag, Constants.EX_VECTOR_LENGTH);
            return Factory.wrap(elements.Length);
        }
        DISPATCH2_W_TAG(op_vector_ref, Tags.VectorTag, EX_VECTOR_REF)
        DISPATCH3_W_TAG(op_vector_set, Tags.VectorTag, EX_VECTOR_SET)

        // Special Operations
        //SPECIALIMPL1(op_zerop) {
        //    if (this.tag == Tags.RectnumTag) {
        //        return Factory.False; // FIXME??
        //    } else if (this.tag == Tags.RatnumTag) {
        //        this.op_numeric_equals_2(Factory.makeFixnum(0));
        //    } else {
        //        super.op_zerop();
        //    }
        //}
    }
    
    // -------------------------------------------
    // SByteVL (bytevector-like)
    // -------------------------------------------
    public sealed class SByteVL : STagged {
        public readonly byte[] elements;
        public static System.Text.Encoding stringEncoding 
            = new System.Text.ASCIIEncoding();

        public SByteVL(int tag, int size, byte fill) {
            this.tag = tag;
            this.elements = new byte[size];
            for (int i = 0; i < size; ++i) {elements[i] = fill;}
        }
        public SByteVL(int tag, byte[] vec) {
            this.tag = tag;
            this.elements = vec;
        }
                
        public int length() {
            return elements.Length;
        }

        public SFixnum elementAt(int index) {
            return SFixnum.makeFixnum(elements[index]);
        }

        public void setElementAt(int index, int element) {
            elements[index] = (byte)element;
        }

        public byte getByte(int index) {
            return elements[index];
        }
        public void setByte(int index, int b) {
            elements[index] = (byte)b;
        }

        public void fill(byte b) {
            for (int i = 0; i < elements.Length; ++i) {
                elements[i] = b;
            }
        }
        
        public short getInt16(int index) {
            return System.BitConverter.ToInt16(elements, index*2);
        }
        public void setInt16(int index, int s) {
            byte[] bytes = System.BitConverter.GetBytes((short)s);
            elements[index*2] = bytes[0];
            elements[index*2 + 1] = bytes[1];
        }
        
        public ushort getUInt16(int index) {
            return System.BitConverter.ToUInt16(elements, index*2);
        }
        public void setUInt16(int index, int s) {
            byte[] bytes = System.BitConverter.GetBytes((ushort)s);
            elements[index*2] = bytes[0];
            elements[index*2 + 1] = bytes[1];
        }

        public int getInt32(int index) {
            return System.BitConverter.ToInt32(elements, index*4);
        }
        public void setInt32(int index, int n) {
            byte[] bytes = System.BitConverter.GetBytes(n);
            int i = index*4;
            elements[i] = bytes[0];
            elements[i+1] = bytes[1];
            elements[i+2] = bytes[2];
            elements[i+3] = bytes[3];
        }

        public uint getUInt32(int index) {
            return System.BitConverter.ToUInt32(elements, index*4);
        }
        public void setUInt32(int index, uint n) {
            byte[] bytes = System.BitConverter.GetBytes(n);
            int i = index*4;
            elements[i] = bytes[0];
            elements[i+1] = bytes[1];
            elements[i+2] = bytes[2];
            elements[i+3] = bytes[3];
        }

        // unsafeAsDouble: interprets bytes as the bit representation 
        //     of a double value
        public double unsafeAsDouble(int steps) {
            return System.BitConverter.ToDouble(elements, 4 + steps * 8); // steps * sizeof(double)) + offset
        }
        public void unsafeSetDouble(double d) {
            byte[] b = System.BitConverter.GetBytes(d);
            for (int i = 0; i < 8; ++i) {
                elements[i+4] = b[i];
            }
        }


        // -----
        private bool isIntegralFlonum() {
            double v = this.unsafeAsDouble(0);
            // Exn.debug.Write("Testing for integral double " + v);
            bool b = (Math.Ceiling(v) == Math.Floor(v));
            // Exn.debug.WriteLine(b ? " yes" : " no");
            return b;
        }

        public override void write(TextWriter w) {
            if (this.tag == Tags.ByteVectorTag) {
                w.Write("#<bytevector*");
                w.Write(elements.Length);
                w.Write(">");
            } else if (this.tag == Tags.StringTag) {
                w.Write("\"");
                w.Write(this.asString());
                w.Write("\"");
            } else if (this.tag == Tags.CompnumTag) {
                w.Write("#<compnum>");
            } else if (this.tag == Tags.BignumTag) {
                w.Write("#<bignum[{0}]>", Number.getBignumLength(this));
            } else if (this.tag == Tags.FlonumTag) {
                w.Write(this.unsafeAsDouble(0));
            } else {
                w.Write("#<bytevector-like ");
                w.Write(this.tag);
                w.Write(">");
            }
        }
        
        // asString returns a CLR string with the same characters as the Scheme string
        // It does not add ""
        public string asString() {
            return stringEncoding.GetString(elements);
        }

        // ----------------------

        public override bool isNumber() {
            return this.isBignum() || this.isFlonum() || this.isCompnum();
        } 
        public override bool isExact() {
            return this.isBignum();
        }
        public override bool isInexact() {
            return this.isFlonum() || this.isCompnum();
        }
        public override bool isBignum() {
            return this.tag == Tags.BignumTag;
        }
        public override bool isFlonum() {
            return this.tag == Tags.FlonumTag;
        }
        public override bool isCompnum() {
            return this.tag == Tags.CompnumTag;
        }
        public override bool isComplex() {
            return this.isBignum() || this.isFlonum() || this.isCompnum();
        }
        public override bool isReal() {
            return this.isBignum() || this.isFlonum();
        }
        public override bool isRational() {
            return this.isBignum() || this.isFlonum();
        }
        public override bool isInteger() {
            return this.isBignum() || (this.isFlonum() && this.isIntegralFlonum());
        }
        public override bool isByteVectorLike() {
            return true;
        }
        public override bool isByteVector() {
            return this.tag == Tags.ByteVectorTag;
        }
        public override bool isString() {
            return this.tag == Tags.StringTag;
        }

        // ----------------------

        public override SObject op_numberp() {
            return Factory.wrap(this.isBignum() || this.isFlonum() || this.isCompnum());
        } 
        public override SObject op_exactp() {
            return this.op_bignump();
        }
        public override SObject op_inexactp() {
            return Factory.wrap(this.isFlonum() || this.isCompnum());
        }
        public override SObject op_bignump() {
            return Factory.wrap(this.tag == Tags.BignumTag);
        }
        public override SObject op_flonump() {
            return Factory.wrap(this.tag == Tags.FlonumTag);
        }
        public override SObject op_compnump() {
            return Factory.wrap(this.tag == Tags.CompnumTag);
        }
        public override SObject op_complexp() {
            return Factory.wrap(this.isBignum() || this.isFlonum() || this.isCompnum());
        }
        public override SObject op_realp() {
            return Factory.wrap(this.isBignum() || this.isFlonum());
        }
        public override SObject op_rationalp() {
            return Factory.wrap(this.isBignum() || this.isFlonum());
        }
        public override SObject op_integerp() {
            return Factory.wrap(this.isBignum() ||
                                (this.isFlonum() && this.isIntegralFlonum()));
        }
        public override SObject op_bytevector_likep() {
            return Factory.True;
        }
        public override SObject op_bytevectorp() {
            return Factory.wrap(this.tag == Tags.ByteVectorTag);
        }
        public override SObject op_stringp() {
            return Factory.wrap(this.tag == Tags.StringTag);
        }

        DISPATCH2_W_TAG(op_string_ref, Tags.StringTag, EX_STRING_REF)
        DISPATCH3_W_TAG(op_string_set, Tags.StringTag, EX_STRING_SET)

        IMPL1_W_TAG(op_bytevector_length, Tags.ByteVectorTag, EX_BYTEVECTOR_LENGTH) {
            return Factory.wrap(this.elements.Length);
        }
        DISPATCH2_W_TAG(op_bytevector_ref, Tags.ByteVectorTag, EX_BYTEVECTOR_REF)
        DISPATCH3_W_TAG(op_bytevector_set, Tags.ByteVectorTag, EX_BYTEVECTOR_SET)
        DISPATCH2_W_TAG(op_bytevector_equal, Tags.ByteVectorTag, EX_UNSUPPORTED)
        DISPATCH2_W_TAG(op_bytevector_fill, Tags.ByteVectorTag, EX_BVFILL)

        IMPL1(op_bytevector_like_length) {
            return Factory.wrap(this.elements.Length);
        }
        DISPATCH2(op_bytevector_like_ref)
        DISPATCH3(op_bytevector_like_set)
        DISPATCH2(op_sys_bvlcmp)

        OVERRIDE2X(op_sys_bvlcmp, SByteVL) {
            byte[] a = arg1.elements;
            byte[] b = this.elements;
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
    }

    // -------------------------------------------
    // SPair
    // -------------------------------------------
    public sealed class SPair : SObject {
        public SObject first;
        public SObject rest;

        public override void write(TextWriter w) {
            w.Write("(");
            writeInList(this, w);
            w.Write(")");
        }

        public static void writeInList(SPair p, TextWriter w) {
            while (true) {
                w.Write(p.first);
                if (p.rest.isPair()) {
                    w.Write(" ");
                    p = (SPair)p.rest;
                } else if (p.rest == Factory.Null) {
                    return;
                } else {
                    w.Write(" . ");
                    p.rest.write(w);
                    return;
                }
            }
        }

        public SPair(SObject elem, SObject rest) {
            this.first = elem;
            this.rest = rest;
        }

        public SObject getFirst() {
            return first;
        }

        public SObject getRest() {
            return rest;
        }

        public void setFirst(SObject first) {
            this.first = first;
        }

        public void setRest(SObject rest) {
            this.rest = rest;
        }

        // ------------------
        public override bool isPair() { return true; }

        public override SObject op_pairp() { return Factory.True; }
        public override SObject op_cell_ref() { return this.first; }
        public override SObject op_cell_set(SObject arg2) {
            this.first = arg2;
            return Factory.Unspecified;
        }
        IMPL1(op_car) { return this.first; }
        IMPL1(op_car_pair) { return this.first; }
        IMPL1(op_cdr) { return this.rest; }
        IMPL1(op_cdr_pair) { return this.rest; }

        IMPL2(op_set_car) { this.first = arg2; return Factory.Unspecified; }
        IMPL2(op_set_car_pair) { this.first = arg2; return Factory.Unspecified; }
        IMPL2(op_set_cdr) { this.rest = arg2; return Factory.Unspecified; }
        IMPL2(op_set_cdr_pair) { this.rest = arg2; return Factory.Unspecified; }

    }
    
    // -------------------------------------------
    // Procedure
    // -------------------------------------------
    public sealed class Procedure : STagged {
        public CodeVector entrypoint;
        public SObject[] rib;
        public SVL constantvector;
        public SObject[] constants;

        public Procedure(CodeVector entrypoint, 
                         SObject constantvector,
                         SObject[] rib) {
            this.tag = Constants.PROC_TAG;
            this.entrypoint = entrypoint;
            this.constantvector = (SVL) constantvector;
            this.constants = this.constantvector.elements;
            this.rib = rib;
        }

        public Procedure(CodeVector entrypoint, SObject constantvector)
            : this(entrypoint, constantvector, new SObject[]{}) {}
        public Procedure(CodeVector entrypoint)
            : this(entrypoint, Factory.makeVector(1, Factory.False), new SObject[0]) {}

        public void setCode(SObject code) {
            if (code is CodeVector) {
                this.entrypoint = (CodeVector) code;
            } else if (code == Factory.False) {
                this.entrypoint = CodeVector.NoCode;
            } else {
                Exn.internalError("procedure-set! 0 called, not a codevector: " + code);
            }
        }
        public SObject getCode() {
            if (this.entrypoint is DataCodeVector) {
                return ((DataCodeVector)this.entrypoint).datum;
            } else { 
                return this.entrypoint;
            }
        }
        
        public void setConstants(SVL constantvector) {
            this.constantvector = constantvector;
            this.constants = constantvector.elements;
        }
        
        /** lookup
         * Look up (rib, slot) in lexical environment
         */    
        public SObject lookup(int ri, int slot) {
            SObject[] rib = this.rib;
            while (ri > 0) {
                rib = ((Procedure)rib[0]).rib;
                ri --;
            }
            return rib[slot];
        }
        
        /** update
         * Mutate a lexically bound variable at (rib, slot) to new_value
         */
        public void update(int ri, int slot, SObject newValue) {
            SObject[] rib = this.rib;
            while (ri > 0) {
                rib = ((Procedure)rib[0]).rib;
                ri --;
            }
            rib[slot] = newValue;
        }

        public override void write(TextWriter w) {
            w.Write("#<PROCEDURE: ");
            if (this.constants.Length >= 1) {
                SObject d = this.constants[0];
                if (d is SVL) {
                    SVL dd = (SVL) d;
                    if (dd.elements != null && dd.elements.Length >= 1) {
                        w.Write(((SVL)d).elements[0]);
                        w.Write("=");
                    }
                }
            }
            w.Write(entrypoint.name());
            w.Write(">");
        }

        public override bool isProcedure() { return true; }
        public override SObject op_procedurep() {
            return Factory.True;
        }

        IMPL1(op_procedure_length) {
            return Factory.wrap(this.rib.Length + 2);
        }
        DISPATCH2(op_procedure_ref)
        DISPATCH3(op_procedure_set)
    }

    // -------------------------------------------
    // CodeVectors and ConstantVectors
    // -------------------------------------------
    public abstract class CodeVector : SObject {
        public static readonly CodeVector NoCode = new DataCodeVector(Factory.False);

        /** call
         * Given a jump index (0 for entry point, NOT the same as label number),
         * start executing at the label corresponding to that code.
         */
        public abstract void call(int jump_index);
        
        public virtual int id() { return 0; }
        public string name() {
            Type t = this.GetType();
            string ns = (string) Exn.namespaces[t.Namespace];
            if (ns == null) {
                ns = t.ToString();
            }
            int idn = id();
            return ns + " " + (idn >> 16) + ":" + (idn & 0xFFFF);
        }

        public override void write(TextWriter w) {
            w.Write("#<CodeVector ");
            w.Write(name());
            w.Write(">");
        }
    }

    public class DataCodeVector : CodeVector {
        public SObject datum;

        public DataCodeVector(SObject datum) {
            this.datum = datum;
        }

        public override void call(int ignored) {
            throw new Exception("not a real codevector");
        }
    }

    /* Foreign
     * Holds foreign values; cooperates with ffi.
     */
    public class Foreign : SObject {
        public object value;
        public Foreign(object value) {
            this.value = value;
        }
    }
}

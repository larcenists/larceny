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
        PREDICATE_VIRTUAL_FALSE(isNumber, numberp)
        PREDICATE_VIRTUAL_FALSE(isFixnum, fixnump)
        PREDICATE_VIRTUAL_FALSE(isFlonum, flonump)
        PREDICATE_VIRTUAL_FALSE(isBignum, bignump)
        PREDICATE_VIRTUAL_FALSE(isRatnum, ratnump)
        PREDICATE_VIRTUAL_FALSE(isRectnum, rectnump)
        PREDICATE_VIRTUAL_FALSE(isCompnum, compnump)

        PREDICATE_VIRTUAL_FALSE(isComplex, complexp)
        PREDICATE_VIRTUAL_FALSE(isReal, realp)
        PREDICATE_VIRTUAL_FALSE(isRational, rationalp)
        PREDICATE_VIRTUAL_FALSE(isInteger, integerp)
        CLR_PREDICATE_VIRTUAL_FALSE(isExact)
        CLR_PREDICATE_VIRTUAL_FALSE(isInexact)
        OP1_VIRTUAL_EXN(exactp, EX_EXACTP)
        OP1_VIRTUAL_EXN(inexactp, EX_INEXACTP)

        PREDICATE_VIRTUAL_FALSE(isImmediate, immediatep)
        PREDICATE_VIRTUAL_FALSE(isSymbol, symbolp)
        PREDICATE_VIRTUAL_FALSE(isChar, charp)
        PREDICATE_VIRTUAL_FALSE(isPair, pairp)
        PREDICATE_VIRTUAL_FALSE(isProcedure, procedurep)

        PREDICATE_VIRTUAL_FALSE(isVectorLike, vector_likep)
        PREDICATE_VIRTUAL_FALSE(isVector, vectorp)
        PREDICATE_VIRTUAL_FALSE(isPort, portp)
        PREDICATE_VIRTUAL_FALSE(isStructure, structurep)

        PREDICATE_VIRTUAL_FALSE(isByteVectorLike, bytevector_likep)
        PREDICATE_VIRTUAL_FALSE(isByteVector, bytevectorp)
        PREDICATE_VIRTUAL_FALSE(isString, stringp)

        // ===================
        //   Operations
        // ===================

        OP2(eqp) { return Factory.wrap(this == arg2); }
        OP1(not) { return Factory.wrap(this != Factory.False); }
        OP1(nullp) { return Factory.wrap(this == Factory.Null); }
        OP1(eof_objectp) { return Factory.wrap(this == Factory.Eof); }
        OP1(unspecifiedp) { return Factory.wrap(this == Factory.Unspecified); }
        OP1(undefinedp) { return Factory.wrap(this == Factory.Undefined); }
        
        // Misc
        OP1(creg) { return Cont.getCC(); }
        OP1(creg_set) {
            Cont.setCC(this);
            return Factory.Unspecified;
        }
        OP1_VIRTUAL_EXN(break, EX_BREAKPOINT)
        OP1_VIRTUAL_EXN(gc_counter, EX_UNSUPPORTED)

        // Data
        OP1(unspecified) { return Factory.Unspecified; }
        OP1(undefined) { return Factory.Undefined; }
        OP1(eof_object) { return Factory.Eof; }
        
        // Chars
        OP2_VIRTUAL_EXN_PAIR(char_lt, EX_CHARLT, SChar)
        OP2_VIRTUAL_EXN_PAIR(char_le, EX_CHARLE, SChar)
        OP2_VIRTUAL_EXN_PAIR(char_ge, EX_CHARGE, SChar)
        OP2_VIRTUAL_EXN_PAIR(char_gt, EX_CHARGT, SChar)
        OP2_VIRTUAL_EXN_PAIR(char_equals, EX_CHAREQ, SChar)

        OP1_VIRTUAL_EXN(char2integer, EX_CHAR2INT)
        OP1_VIRTUAL_EXN(integer2char, EX_INT2CHAR)

        // Cell Operations
        OP1(make_cell) { return Factory.makePair(this, Factory.False); }
        OP1_VIRTUAL_EXN(cell_ref, EX_CAR)
        OP2_VIRTUAL_EXN(cell_set, EX_SETCAR)

        // Pair Operations
        OP2(cons) { return Factory.makePair(this, arg2); }
        OP1_VIRTUAL_EXN(car, EX_CAR)
        OP1_VIRTUAL_EXN(car_pair, EX_CAR)
        OP1_VIRTUAL_EXN(cdr, EX_CDR)
        OP1_VIRTUAL_EXN(cdr_pair, EX_CDR)

        OP2_VIRTUAL_EXN(set_car, EX_SETCAR)
        OP2_VIRTUAL_EXN(set_car_pair, EX_SETCAR)
        OP2_VIRTUAL_EXN(set_cdr, EX_SETCDR)
        OP2_VIRTUAL_EXN(set_cdr_pair, EX_SETCDR)
        
        // Typetag Operations
        OP1_VIRTUAL_EXN(typetag, EX_TYPETAG)
        OP2_VIRTUAL_EXN_PAIR(typetag_set, EX_TYPETAGSET, STagged)

        // Vectorlike Operations
        OP1_VIRTUAL_EXN(vector_like_length, EX_VLLEN)
        OP2_VIRTUAL_EXN_PAIR(vector_like_ref, EX_VLREF, SVL)
        OP3_VIRTUAL_EXN_PAIR(vector_like_set, EX_VLSET, SVL)

        // Vector Operations
        OP2_VIRTUAL_EXN(make_vector, EX_MKVL)
        OP1_VIRTUAL_EXN(vector_length, EX_VECTOR_LENGTH)
        OP2_VIRTUAL_EXN_PAIR(vector_ref, EX_VECTOR_REF, SVL)
            //        OP2_VIRTUAL_EXN_PAIR(vector_ref_trusted, EX_VECTOR_REF, SVL)
        OP3_VIRTUAL_EXN_PAIR(vector_set, EX_VECTOR_SET, SVL)
            //        OP3_VIRTUAL_EXN_PAIR(vector_set_trusted, EX_VECTOR_SET, SVL)

        // Procedure Operations
        OP1_VIRTUAL_EXN(procedure_length, EX_PROCEDURE_LENGTH)
        OP1_VIRTUAL_EXN(make_procedure, EX_MKVL)
        OP2_VIRTUAL_EXN_PAIR(procedure_ref, EX_PROCEDURE_REF, Procedure)
        OP3_VIRTUAL_EXN_PAIR(procedure_set, EX_PROCEDURE_SET, Procedure)

        // String Operations
        OP2_VIRTUAL_EXN_PAIR(make_string, EX_MKBVL, SFixnum)
        OP1_VIRTUAL_EXN(string_length, EX_STRING_LENGTH)
        OP2_VIRTUAL_EXN_PAIR(string_ref, EX_STRING_REF, SByteVL)
            //        OP2_VIRTUAL_EXN_PAIR(string_ref_trusted, EX_STRING_REF, SByteVL)
        OP3_VIRTUAL_EXN_PAIR(string_set, EX_STRING_SET, SByteVL)
            //        OP3_VIRTUAL_EXN_PAIR(string_set_trusted, EX_STRING_SET, SByteVL)

        // Bytevector Operations
        OP1_VIRTUAL_EXN(make_bytevector, EX_MKBVL)
        OP1_VIRTUAL_EXN(bytevector_length, EX_BYTEVECTOR_LENGTH)
        OP2_VIRTUAL_EXN_PAIR(bytevector_ref, EX_BYTEVECTOR_REF, SByteVL)
        OP3_VIRTUAL_EXN_PAIR(bytevector_set, EX_BYTEVECTOR_SET, SByteVL)
        OP2_VIRTUAL_EXN_PAIR(bytevector_equal, EX_UNSUPPORTED, SByteVL)
        OP2_VIRTUAL_EXN_PAIR(bytevector_fill, EX_BVFILL, SByteVL)

        // Bytevector-like Operations
        OP1_VIRTUAL_EXN(bytevector_like_length, EX_BVLLEN)
        OP2_VIRTUAL_EXN_PAIR(bytevector_like_ref, EX_BVLREF, SByteVL)
        OP3_VIRTUAL_EXN_PAIR(bytevector_like_set, EX_BVLSET, SByteVL)
        OP2_VIRTUAL_EXN_PAIR(sys_bvlcmp, EX_UNSUPPORTED, SByteVL)

        // Fixnum Operations
        OP1_VIRTUAL_EXN(fxzerop, EX_UNSUPPORTED)
        OP1_VIRTUAL_EXN(fxpositivep, EX_FXPOSITIVE)
        OP1_VIRTUAL_EXN(fxnegativep, EX_FXNEGATIVE)
        OP1_VIRTUAL_EXN(fxnegative, EX_FXNEG)
        OP2_VIRTUAL_EXN_PAIR(fxplus, EX_FXADD, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxminus, EX_FXSUB, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxmul, EX_FXMUL, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxless, EX_FXLT, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxgreater, EX_FXGT, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxless_equal, EX_FXLE, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxgreater_equal, EX_FXGE, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxequal, EX_FXEQ, SFixnum)

        OP1(most_positive_fixnum) { return Factory.makeFixnum(SFixnum.MAX); }
        OP1(most_negative_fixnum) { return Factory.makeFixnum(SFixnum.MIN); }

        // Logical Operations
        OP1_VIRTUAL_EXN(lognot, EX_LOGNOT)
        OP2_VIRTUAL_EXN_PAIR(logand, EX_LOGAND, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(logior, EX_LOGIOR, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(logxor, EX_LOGXOR, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(lsh, EX_LSH, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(rsh, EX_RSHA, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(rsha, EX_RSHA, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(rshl, EX_RSHL, SFixnum)

        // Arithmetic Operations
        OP1_VIRTUAL_EXN(real_part, EX_REALPART)
        OP1_VIRTUAL_EXN(imag_part, EX_IMAGPART)

        // -------------------
        // Special Operations
        // -------------------

        //SPECIALOP1_VIRTUAL_EXN(enable_interrupts, EX_EINTR)
        //public void disable_interrupts() {
        //    if (Reg.interruptsEnabled) {
        //        Reg.interruptsEnabled = false;
        //        Reg.Result = Factory.makeFixnum((int)Reg.timer);
        //    } else {
        //        Reg.Result = Factory.makeBoolean(false);
        //    }
        //    Exn.checkSignals();
        //}

        //SPECIALOP1_VIRTUAL_EXN(syscall)
        //
        //SPECIALOP1_VIRTUAL_EXN(zerop)
        //SPECIALOP2_VIRTUAL_EXN(eqvp)
        //SPECIALOP2_VIRTUAL_EXN(numeric_equals)
        //SPECIALOP2_VIRTUAL_EXN(less_than)
        //SPECIALOP2_VIRTUAL_EXN(greater_than)
        //SPECIALOP2_VIRTUAL_EXN(less_or_equal)
        //SPECIALOP2_VIRTUAL_EXN(greater_or_equal)

        //SPECIALARITH2(plus)  // Declares plus_2 for SFixum, SVL, SByteVL
        //SPECIALARITH2(minus)
        //SPECIALARITH2(multiply)
        //SPECIALARITH2(divide)
        //SPECIALARITH2(quotient)
        //SPECIALARITH2(remainder)
        //SPECIALOP1_VIRTUAL_EXN(truncate)
        //SPECIALOP1_VIRTUAL_EXN(round)
        //SPECIALOP1_VIRTUAL_EXN(negative)
        //SPECIALOP1_VIRTUAL_EXN(exact2inexact)
        //SPECIALOP1_VIRTUAL_EXN(inexact2exact)

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
        PREDICATE_OVERRIDE_TRUE(isImmediate, immediatep)
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
        PREDICATE_OVERRIDE_TRUE(isChar, charp)

        OP2_CHAIN(char_lt)
        OP2_CHAIN(char_le)
        OP2_CHAIN(char_gt)
        OP2_CHAIN(char_ge)
        OP2_CHAIN(char_equals)
        OP2_OVERRIDE_REVERSED(char_lt, SChar) {
            return Factory.wrap(arg1.val < this.val);
        }
        OP2_OVERRIDE_REVERSED(char_le, SChar) {
            return Factory.wrap(arg1.val <= this.val);
        }
        OP2_OVERRIDE_REVERSED(char_gt, SChar) {
            return Factory.wrap(arg1.val > this.val);
        }
        OP2_OVERRIDE_REVERSED(char_ge, SChar) {
            return Factory.wrap(arg1.val >= this.val);
        }
        OP2_OVERRIDE_REVERSED(char_equals, SChar) {
            return Factory.wrap(arg1.val == this.val);
        }

        OP1_OVERRIDE(char2integer) {
            return Factory.wrap(this.val);
        }

        OP2_OVERRIDE_REVERSED(make_string, SFixnum) {
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

        PREDICATE_OVERRIDE_TRUE(isNumber, numberp)
        PREDICATE_OVERRIDE_TRUE(isFixnum, fixnump)
        PREDICATE_OVERRIDE_TRUE(isComplex, complexp)
        PREDICATE_OVERRIDE_TRUE(isReal, realp)
        PREDICATE_OVERRIDE_TRUE(isRational, rationalp)
        PREDICATE_OVERRIDE_TRUE(isInteger, integerp)
        PREDICATE_OVERRIDE_TRUE(isExact, exactp)
        PREDICATE_OVERRIDE_FALSE(isInexact, inexactp)

        OP1_OVERRIDE(integer2char) {
            return Factory.makeChar(this.value);
        }
        OP2_OVERRIDE_REVERSED(typetag_set, STagged) {
            arg1.tag = this.value;
            return Factory.Unspecified;
        }
        OP2_OVERRIDE_REVERSED(vector_like_ref, SVL) {
            SObject[] elements = arg1.elements;
            int index = this.value;
            if (0 <= index && index < elements.Length) {
                return elements[index];
            } else {
                Exn.fault(Constants.EX_VLREF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        OP3_OVERRIDE_REVERSED(vector_like_set, SVL) {
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
        OP2_OVERRIDE(make_vector) {
            int length = this.value;
            if (0 <= length) {
                return Factory.makeVector(this.value, arg2);
            } else {
                Exn.fault(Constants.EX_MKVL, null, this, arg2);
                return Factory.Impossible;
            }
        }
        OP2_OVERRIDE_REVERSED(vector_ref, SVL) {
            SObject[] elements = arg1.elements;
            int index = this.value;
            if (0 <= index && index < elements.Length) {
                return elements[index];
            } else {
                Exn.fault(Constants.EX_VECTOR_REF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        OP3_OVERRIDE_REVERSED(vector_set, SVL) {
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
        OP1_OVERRIDE(make_procedure) {
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
        OP2_OVERRIDE_REVERSED(procedure_ref, Procedure) {
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
        OP3_OVERRIDE_REVERSED(procedure_set, Procedure) {
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

        OP2_CHAIN(make_string)

        OP1_OVERRIDE(make_bytevector) {
            return Factory.makeByteVector(this.value, (byte)0);
        }
        OP2_OVERRIDE_REVERSED(bytevector_ref, SByteVL) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            if (0 <= index && index < bytes.Length) {
                return Factory.wrap(bytes[index]);
            } else {
                Exn.fault(Constants.EX_BYTEVECTOR_REF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        OP3_OVERRIDE_REVERSED(bytevector_set, SByteVL) {
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
        OP2_OVERRIDE_REVERSED(bytevector_fill, SByteVL) {
            byte[] bytes = arg1.elements;
            byte fill = (byte) this.value;
            for (int i = 0; i < bytes.Length; ++i) {
               bytes[i] = fill;
            }
            return Factory.Unspecified;
        }
        
        OP2_OVERRIDE_REVERSED(bytevector_like_ref, SByteVL) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            if (0 <= index && index < bytes.Length) {
                return Factory.wrap(bytes[index]);
            } else {
                Exn.fault(Constants.EX_BVLREF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        OP3_OVERRIDE_REVERSED(bytevector_like_set, SByteVL) {
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
        
        OP1_OVERRIDE(fxzerop) {
            return Factory.wrap(this.value == 0);
        }
        OP1_OVERRIDE(fxpositivep) {
            return Factory.wrap(this.value > 0);
        }
        OP1_OVERRIDE(fxnegativep) {
            return Factory.wrap(this.value < 0);
        }
        OP1_OVERRIDE(fxnegative) {
            int a = - this.value;
            if (!SFixnum.inFixnumRange(-a)) {
                Exn.fault(Constants.EX_FXNEG, "result not a fixnum", this);
                return Factory.Impossible;
            }
            return Factory.wrap(a);
        }
        OP2_CHAIN(fxplus)
        OP2_CHAIN(fxminus)
        OP2_CHAIN(fxmul)
        OP2_CHAIN(fxless)
        OP2_CHAIN(fxless_equal)
        OP2_CHAIN(fxgreater)
        OP2_CHAIN(fxgreater_equal)
        OP2_CHAIN(fxequal)

        OP2_OVERRIDE_REVERSED(fxplus, SFixnum) {
            int a = arg1.value, b = this.value; 
            int r = a + b; 
            if (!SFixnum.inFixnumRange(r)) {
                Exn.fault(Constants.EX_FXADD, null, arg1, this);
                return Factory.Impossible; 
            }
            return Factory.wrap(r);
        }
        OP2_OVERRIDE_REVERSED(fxminus, SFixnum) {
            int a = arg1.value, b = this.value; 
            int r = a - b; 
            if (!SFixnum.inFixnumRange(r)) {
                Exn.fault(Constants.EX_FXSUB, null, arg1, this); 
                return Factory.Impossible; 
            }
            return Factory.wrap(r);
        }
        OP2_OVERRIDE_REVERSED(fxmul, SFixnum) {
            int a = arg1.value, b = this.value;
            long r = a * b;
            if (!SFixnum.inFixnumRange(r)) {
                Exn.fault(Constants.EX_FXMUL, null, arg1, this);
                return Factory.Impossible;
            }
            return Factory.wrap((int)r);
        }
        OP2_OVERRIDE_REVERSED(fxless, SFixnum) {
            return Factory.wrap(arg1.value < this.value);
        }
        OP2_OVERRIDE_REVERSED(fxless_equal, SFixnum) {
            return Factory.wrap(arg1.value <= this.value);
        }
        OP2_OVERRIDE_REVERSED(fxgreater, SFixnum) {
            return Factory.wrap(arg1.value > this.value);
        }
        OP2_OVERRIDE_REVERSED(fxgreater_equal, SFixnum) {
            return Factory.wrap(arg1.value >= this.value);
        }
        OP2_OVERRIDE_REVERSED(fxequal, SFixnum) {
            return Factory.wrap(arg1.value == this.value);
        }

        OP1_OVERRIDE(lognot) {
            return Factory.wrap(~this.value);
        }
        OP2_CHAIN(logand)
        OP2_CHAIN(logior)
        OP2_CHAIN(logxor)
        OP2_CHAIN(lsh)
        OP2_CHAIN(rsh)
        OP2_CHAIN(rsha)
        OP2_CHAIN(rshl)

        OP2_OVERRIDE_REVERSED(logand, SFixnum) {
            return Factory.wrap(arg1.value & this.value);
        }
        OP2_OVERRIDE_REVERSED(logior, SFixnum) {
            return Factory.wrap(arg1.value | this.value);
        }
        OP2_OVERRIDE_REVERSED(logxor, SFixnum) {
            return Factory.wrap(arg1.value ^ this.value);
        }
        OP2_OVERRIDE_REVERSED(lsh, SFixnum) {
            int r = arg1.value << this.value;
            r = (r << 2) >> 2; // mask out top bits (w/ sign extend)
            return Factory.wrap(r);
        }
        OP2_OVERRIDE_REVERSED(rsh, SFixnum) {
            int r = arg1.value >> this.value;
            r = (r << 2) >> 2;
            return Factory.wrap(r);
        }
        OP2_OVERRIDE_REVERSED(rsha, SFixnum) {
            int r = arg1.value >> this.value;
            r = (r << 2) >> 2;
            return Factory.wrap(r);
        }
        OP2_OVERRIDE_REVERSED(rshl, SFixnum) {
            uint a = (uint)arg1.value << 2;
            int b = this.value;
            int r = (int)(a >> b) >> 2;
            return Factory.wrap((int)r);
        }

        OP1_OVERRIDE(real_part) {
            return this;
        }
        OP1_OVERRIDE(imag_part) {
            return Factory.wrap(0);
        }

        // Special Operations

        //SPECIALOP1_OVERRIDE(enable_interrupts) {
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
        //SPECIALOP1_OVERRIDE(syscall) {
        //    // subtract one 'cuz the first arg is just the value
        //    // to which we want to dispatch.
        //    int num_args = this.value - 1;
        //    Sys num_syscall = (Sys) ((SFixnum)Reg.register1).intValue();
        //    Syscall.dispatch(num_args, num_syscall);
        //}
        //SPECIALOP1_OVERRIDE(zerop) {
        //    Reg.Result = Factory.wrap(this.value == 0);
        //}
        //SPECIALOP2_CHAIN(eqvp)
        //SPECIALOP2_CHAIN(numeric_equals)
        //SPECIALOP2_CHAIN(less_then)
        //SPECIALOP2_CHAIN(less_or_equal)
        //SPECIALOP2_CHAIN(greater_than)
        //SPECIALOP2_CHAIN(greater_or_equal)
        //
        //SPECIALOP2_CHAIN(plus)
        //SPECIALOP2_CHAIN(minus)
        //SPECIALOP2_CHAIN(multiply)
        //SPECIALOP2_CHAIN(divide)
        //SPECIALOP2_CHAIN(quotient)
        //SPECIALOP2_CHAIN(remainder)

        //SPECIALOP1_OVERRIDE(truncate) {
        //    Reg.Result = this;
        //}
        //SPECIALOP1_OVERRIDE(round) {
        //    Reg.Result = this;
        //}
        //SPECIALOP1_OVERRIDE(negative) {
        //    Reg.Result = Factory.wrap(-this.value);
        //}
        //SPECIALOP1_OVERRIDE(exact2inexact) {
        //    Reg.Result = Factory.makeFlonum((double)this.value);
        //}
        //SPECIALOP1_OVERRIDE(inexact2exact) {
        //    Reg.Result = this;
        //}

        //SPECIALOP2_OVERRIDE_REVERSED(eqvp, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value == arg1.value);
        //}
        //SPECIALOP2_OVERRIDE_REVERSED(numeric_equals, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value == arg1.value);
        //}
        //SPECIALOP2_OVERRIDE_REVERSED(less_than, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value < arg1.value);
        //}
        //SPECIALOP2_OVERRIDE_REVERSED(less_or_equal, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value <= arg1.value);
        //}
        //SPECIALOP2_OVERRIDE_REVERSED(greater_than, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value > arg1.value);
        //}
        //SPECIALOP2_OVERRIDE_REVERSED(greater_or_equal, SFixnum) {
        //    Reg.Result = Factory.wrap(this.value >= arg1.value);
        //}

        //SPECIALOP2_OVERRIDE_REVERSED(plus, SFixnum) {
        //    Reg.Result = Factory.wrap(arg1.value + this.value);
        //}
        //SPECIALOP2_OVERRIDE_REVERSED(minus, SFixnum) {
        //    Reg.Result = Factory.wrap(arg1.value - this.value);
        //}
        //SPECIALOP2_OVERRIDE_REVERSED(multiply, SFixnum) {
        //    long r = arg1.value * this.value;
        //    Reg.Result = Factory.wrap(r);
        //}
        //SPECIALOP2_OVERRIDE_REVERSED(divide, SFixnum) {
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
        //SPECIALOP2_OVERRIDE_REVERSED(quotient, SFixnum) {
        //    int a = arg1.value, b = this.value;
        //    if (b == 0) {
        //        Exn.fault(Constants.EX_QUOTIENT, null, arg1, this);
        //        return;
        //    } else {
        //        Reg.Result = Factory.wrap(a / b);
        //        return;
        //    }
        //}
        //SPECIALOP2_OVERRIDE_REVERSED(remainder, SFixnum) {
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

        public void check_typetag(int tag, int excode) {
            if (this.tag != tag) Exn.fault(excode, null, this);
        }
        public void check_typetag(int tag, SObject arg2, int excode) {
            if (this.tag != tag) Exn.fault(excode, null, this, arg2);
        }
        public void check_typetag(int tag, SObject arg2, SObject arg3, int excode) {
            if (this.tag != tag) Exn.fault(excode, null, this, arg2, arg3);
        }
        // ----
        OP1_OVERRIDE(typetag) { return Factory.wrap(this.tag); }
        OP2_CHAIN(typetag_set)

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

        PREDICATE_OVERRIDE_EXPR(isNumber, numberp, this.isRatnum() || this.isRectnum())
        PREDICATE_OVERRIDE_EXPR(isRatnum, ratnump, this.tag == Tags.RatnumTag)
        PREDICATE_OVERRIDE_EXPR(isRectnum, rectnump, this.tag == Tags.RectnumTag)
        PREDICATE_OVERRIDE_EXPR(isComplex, complexp, this.isRatnum() || this.isRectnum())
        PREDICATE_OVERRIDE_EXPR(isReal, realp, this.isRatnum())

        // FIXME!!!! exact?, inexact? should throw errors on non-numbers
        CLR_PREDICATE_OVERRIDE_EXPR(isExact, this.isRatnum() || this.isRectnum())
        CLR_PREDICATE_OVERRIDE_FALSE(isInexact)
        OP1_OVERRIDE(exactp) {
            if (this.isNumber()) {
                return Factory.wrap(this.isRatnum() || this.isRectnum());
            } else {
                Exn.fault(Constants.EX_EXACTP, null, this);
                return Factory.Impossible;
            }
        }
        OP1_OVERRIDE(inexactp) {
            if (this.isNumber()) {
                return Factory.False;
            } else {
                Exn.fault(Constants.EX_INEXACTP, null, this);
                return Factory.Impossible;
            }
        }

        PREDICATE_OVERRIDE_TRUE(isVectorLike, vector_likep)
        PREDICATE_OVERRIDE_EXPR(isVector, vectorp, this.tag == Tags.VectorTag)
        PREDICATE_OVERRIDE_EXPR(isPort, portp, this.tag == Tags.PortTag)
        PREDICATE_OVERRIDE_EXPR(isStructure, structurep, this.tag == Tags.StructureTag)
        PREDICATE_OVERRIDE_EXPR(isSymbol, symbolp, this.tag == Tags.SymbolTag)

        // -------------------
        OP1_OVERRIDE(vector_like_length) {
            return Factory.wrap(elements.Length);
        }
        OP2_CHAIN(vector_like_ref)
        OP3_CHAIN(vector_like_set)

        OP1_OVERRIDE(vector_length) {
            check_typetag(Tags.VectorTag, Constants.EX_VECTOR_LENGTH);
            return Factory.wrap(elements.Length);
        }
        OP2_CHAIN_CHECK_TAG(vector_ref, Tags.VectorTag, EX_VECTOR_REF)
        OP3_CHAIN_CHECK_TAG(vector_set, Tags.VectorTag, EX_VECTOR_SET)

        // Special Operations
        //SPECIALOP1_OVERRIDE(zerop) {
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
            bool b = (Math.Ceiling(v) == Math.Floor(v));
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

        PREDICATE_OVERRIDE_EXPR(isNumber, numberp, this.isBignum() || this.isFlonum() || this.isCompnum())
        CLR_PREDICATE_OVERRIDE_EXPR(isExact, this.isBignum())
        CLR_PREDICATE_OVERRIDE_EXPR(isInexact, this.isFlonum() || this.isCompnum())
        OP1_OVERRIDE(exactp) {
            if (this.isNumber()) {
                return Factory.wrap(this.isBignum());
            } else {
                Exn.fault(Constants.EX_EXACTP, null, this);
                return Factory.Impossible;
            }
        }
        OP1_OVERRIDE(inexactp) {
            if (this.isNumber()) {
                return Factory.wrap(this.isFlonum() || this.isCompnum());
            } else {
                Exn.fault(Constants.EX_INEXACTP, null, this);
                return Factory.Impossible;
            }
        }
        PREDICATE_OVERRIDE_EXPR(isBignum, bignump, this.tag == Tags.BignumTag)
        PREDICATE_OVERRIDE_EXPR(isFlonum, flonump, this.tag == Tags.FlonumTag)
        PREDICATE_OVERRIDE_EXPR(isCompnum, compnump, this.tag == Tags.CompnumTag)
        PREDICATE_OVERRIDE_EXPR(isComplex, complexp, this.isBignum() || this.isFlonum() || this.isCompnum())
        PREDICATE_OVERRIDE_EXPR(isReal, realp, this.isBignum() || this.isFlonum())
        PREDICATE_OVERRIDE_EXPR(isRational, rationalp, this.isBignum() || this.isFlonum())
        PREDICATE_OVERRIDE_EXPR(isInteger, integerp, this.isBignum() || (this.isFlonum() && this.isIntegralFlonum()))
        PREDICATE_OVERRIDE_TRUE(isByteVectorLike, bytevector_likep)
        PREDICATE_OVERRIDE_EXPR(isByteVector, bytevectorp, this.tag == Tags.ByteVectorTag)
        PREDICATE_OVERRIDE_EXPR(isString, stringp, this.tag == Tags.StringTag)
        // ----------------------

        OP2_CHAIN_CHECK_TAG(string_ref, Tags.StringTag, EX_STRING_REF)
        OP3_CHAIN_CHECK_TAG(string_set, Tags.StringTag, EX_STRING_SET)

        OP1_OVERRIDE_CHECK_TAG(bytevector_length, Tags.ByteVectorTag, EX_BYTEVECTOR_LENGTH) {
            return Factory.wrap(this.elements.Length);
        }
        OP2_CHAIN_CHECK_TAG(bytevector_ref, Tags.ByteVectorTag, EX_BYTEVECTOR_REF)
        OP3_CHAIN_CHECK_TAG(bytevector_set, Tags.ByteVectorTag, EX_BYTEVECTOR_SET)
        OP2_CHAIN_CHECK_TAG(bytevector_equal, Tags.ByteVectorTag, EX_UNSUPPORTED)
        OP2_CHAIN_CHECK_TAG(bytevector_fill, Tags.ByteVectorTag, EX_BVFILL)

        OP1_OVERRIDE(bytevector_like_length) {
            return Factory.wrap(this.elements.Length);
        }
        OP2_CHAIN(bytevector_like_ref)
        OP3_CHAIN(bytevector_like_set)
        OP2_CHAIN(sys_bvlcmp)

        OP2_OVERRIDE_REVERSED(sys_bvlcmp, SByteVL) {
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
        PREDICATE_OVERRIDE_TRUE(isPair, pairp)
        OP1_OVERRIDE(cell_ref) { return this.first; }
        OP2_OVERRIDE(cell_set) {
            this.first = arg2;
            return Factory.Unspecified;
        }
        OP1_OVERRIDE(car) { return this.first; }
        OP1_OVERRIDE(car_pair) { return this.first; }
        OP1_OVERRIDE(cdr) { return this.rest; }
        OP1_OVERRIDE(cdr_pair) { return this.rest; }

        OP2_OVERRIDE(set_car) { this.first = arg2; return Factory.Unspecified; }
        OP2_OVERRIDE(set_car_pair) { this.first = arg2; return Factory.Unspecified; }
        OP2_OVERRIDE(set_cdr) { this.rest = arg2; return Factory.Unspecified; }
        OP2_OVERRIDE(set_cdr_pair) { this.rest = arg2; return Factory.Unspecified; }

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

        // ----
        PREDICATE_OVERRIDE_TRUE(isProcedure, procedurep)
        OP1_OVERRIDE(procedure_length) {
            return Factory.wrap(this.rib.Length + 2);
        }
        OP2_CHAIN(procedure_ref)
        OP3_CHAIN(procedure_set)
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

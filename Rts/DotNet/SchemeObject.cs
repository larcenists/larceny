/* Predicates
 * PREDICATE_VIRTUAL_FALSE defines a virtual predicate pair returning false/#f
 * PREDICATE_OVERRIDE_TRUE overrides a virtual predicate to return true/#t
 * PREDICATE_OVERRIDE_EXPR overrides a virtual predicate to return result of body.
 */
/* OP1_VIRTUAL_EXN defines an operation of one argument which
 *   by default throws the given exception
 */






/* OP1(method) declares an operation of one argument
 */
/* OP2_VIRTUAL_EXN defines an operation of two arguments
 *   that by default throws the given exception
 */
/* OP2_CHAIN overrides the operation to chain to 
 *   the reversed form of itself on arg2
 */
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
        public virtual SObject op_numberp() { return Factory.False; } public virtual bool isNumber() { return false; }
        public virtual SObject op_fixnump() { return Factory.False; } public virtual bool isFixnum() { return false; }
        public virtual SObject op_flonump() { return Factory.False; } public virtual bool isFlonum() { return false; }
        public virtual SObject op_bignump() { return Factory.False; } public virtual bool isBignum() { return false; }
        public virtual SObject op_ratnump() { return Factory.False; } public virtual bool isRatnum() { return false; }
        public virtual SObject op_rectnump() { return Factory.False; } public virtual bool isRectnum() { return false; }
        public virtual SObject op_compnump() { return Factory.False; } public virtual bool isCompnum() { return false; }

        public virtual SObject op_complexp() { return Factory.False; } public virtual bool isComplex() { return false; }
        public virtual SObject op_realp() { return Factory.False; } public virtual bool isReal() { return false; }
        public virtual SObject op_rationalp() { return Factory.False; } public virtual bool isRational() { return false; }
        public virtual SObject op_integerp() { return Factory.False; } public virtual bool isInteger() { return false; }
        public virtual bool isExact() { return false; }
        public virtual bool isInexact() { return false; }
        public virtual SObject op_exactp() { Exn.fault(Constants.EX_EXACTP, null, this); return Factory.Impossible; }
        public virtual SObject op_inexactp() { Exn.fault(Constants.EX_INEXACTP, null, this); return Factory.Impossible; }

        public virtual SObject op_immediatep() { return Factory.False; } public virtual bool isImmediate() { return false; }
        public virtual SObject op_symbolp() { return Factory.False; } public virtual bool isSymbol() { return false; }
        public virtual SObject op_charp() { return Factory.False; } public virtual bool isChar() { return false; }
        public virtual SObject op_pairp() { return Factory.False; } public virtual bool isPair() { return false; }
        public virtual SObject op_procedurep() { return Factory.False; } public virtual bool isProcedure() { return false; }

        public virtual SObject op_vector_likep() { return Factory.False; } public virtual bool isVectorLike() { return false; }
        public virtual SObject op_vectorp() { return Factory.False; } public virtual bool isVector() { return false; }
        public virtual SObject op_portp() { return Factory.False; } public virtual bool isPort() { return false; }
        public virtual SObject op_structurep() { return Factory.False; } public virtual bool isStructure() { return false; }

        public virtual SObject op_bytevector_likep() { return Factory.False; } public virtual bool isByteVectorLike() { return false; }
        public virtual SObject op_bytevectorp() { return Factory.False; } public virtual bool isByteVector() { return false; }
        public virtual SObject op_stringp() { return Factory.False; } public virtual bool isString() { return false; }

        // ===================
        //   Operations
        // ===================

        public SObject op_eqp(SObject arg2) { return Factory.wrap(this == arg2); }
        public SObject op_not() { return Factory.wrap(this != Factory.False); }
        public SObject op_nullp() { return Factory.wrap(this == Factory.Null); }
        public SObject op_eof_objectp() { return Factory.wrap(this == Factory.Eof); }
        public SObject op_unspecifiedp() { return Factory.wrap(this == Factory.Unspecified); }
        public SObject op_undefinedp() { return Factory.wrap(this == Factory.Undefined); }

        // Misc
        public SObject op_creg() { return Cont.getCC(); }
        public SObject op_creg_set() {
            Cont.setCC(this);
            return Factory.Unspecified;
        }
        public virtual SObject op_break() { Exn.fault(Constants.EX_BREAKPOINT, null, this); return Factory.Impossible; }
        public virtual SObject op_gc_counter() { Exn.fault(Constants.EX_UNSUPPORTED, null, this); return Factory.Impossible; }

        // Data
        public SObject op_unspecified() { return Factory.Unspecified; }
        public SObject op_undefined() { return Factory.Undefined; }
        public SObject op_eof_object() { return Factory.Eof; }

        // Chars
        public virtual SObject op_char_lt(SObject arg2) { Exn.fault(Constants.EX_CHARLT, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_char_lt(SChar arg1) { Exn.fault(Constants.EX_CHARLT, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_char_le(SObject arg2) { Exn.fault(Constants.EX_CHARLE, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_char_le(SChar arg1) { Exn.fault(Constants.EX_CHARLE, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_char_ge(SObject arg2) { Exn.fault(Constants.EX_CHARGE, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_char_ge(SChar arg1) { Exn.fault(Constants.EX_CHARGE, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_char_gt(SObject arg2) { Exn.fault(Constants.EX_CHARGT, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_char_gt(SChar arg1) { Exn.fault(Constants.EX_CHARGT, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_char_equals(SObject arg2) { Exn.fault(Constants.EX_CHAREQ, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_char_equals(SChar arg1) { Exn.fault(Constants.EX_CHAREQ, null, arg1, this); return Factory.Impossible; }

        public virtual SObject op_char2integer() { Exn.fault(Constants.EX_CHAR2INT, null, this); return Factory.Impossible; }
        public virtual SObject op_integer2char() { Exn.fault(Constants.EX_INT2CHAR, null, this); return Factory.Impossible; }

        // Cell Operations
        public SObject op_make_cell() { return Factory.makePair(this, Factory.False); }
        public virtual SObject op_cell_ref() { Exn.fault(Constants.EX_CAR, null, this); return Factory.Impossible; }
        public virtual SObject op_cell_set(SObject arg2) { Exn.fault(Constants.EX_SETCAR, null, this, arg2); return Factory.Impossible; }

        // Pair Operations
        public SObject op_cons(SObject arg2) { return Factory.makePair(this, arg2); }
        public virtual SObject op_car() { Exn.fault(Constants.EX_CAR, null, this); return Factory.Impossible; }
        public virtual SObject op_car_pair() { Exn.fault(Constants.EX_CAR, null, this); return Factory.Impossible; }
        public virtual SObject op_cdr() { Exn.fault(Constants.EX_CDR, null, this); return Factory.Impossible; }
        public virtual SObject op_cdr_pair() { Exn.fault(Constants.EX_CDR, null, this); return Factory.Impossible; }

        public virtual SObject op_set_car(SObject arg2) { Exn.fault(Constants.EX_SETCAR, null, this, arg2); return Factory.Impossible; }
        public virtual SObject op_set_car_pair(SObject arg2) { Exn.fault(Constants.EX_SETCAR, null, this, arg2); return Factory.Impossible; }
        public virtual SObject op_set_cdr(SObject arg2) { Exn.fault(Constants.EX_SETCDR, null, this, arg2); return Factory.Impossible; }
        public virtual SObject op_set_cdr_pair(SObject arg2) { Exn.fault(Constants.EX_SETCDR, null, this, arg2); return Factory.Impossible; }

        // Typetag Operations
        public virtual SObject op_typetag() { Exn.fault(Constants.EX_TYPETAG, null, this); return Factory.Impossible; }
        public virtual SObject op_typetag_set(SObject arg2) { Exn.fault(Constants.EX_TYPETAGSET, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_typetag_set(STagged arg1) { Exn.fault(Constants.EX_TYPETAGSET, null, arg1, this); return Factory.Impossible; }

        // Vectorlike Operations
        public virtual SObject op_vector_like_length() { Exn.fault(Constants.EX_VLLEN, null, this); return Factory.Impossible; }
        public virtual SObject op_vector_like_ref(SObject arg2) { Exn.fault(Constants.EX_VLREF, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_vector_like_ref(SVL arg1) { Exn.fault(Constants.EX_VLREF, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_vector_like_set(SObject arg2, SObject arg3) { Exn.fault(Constants.EX_VLSET, null, this, arg2, arg3); return Factory.Impossible; } public virtual SObject op_reversed_vector_like_set(SVL arg1, SObject arg3) { Exn.fault(Constants.EX_VLSET, null, arg1, this, arg3); return Factory.Impossible; }

        // Vector Operations
        public virtual SObject op_make_vector(SObject arg2) { Exn.fault(Constants.EX_MKVL, null, this, arg2); return Factory.Impossible; }
        public virtual SObject op_vector_length() { Exn.fault(Constants.EX_VECTOR_LENGTH, null, this); return Factory.Impossible; }
        public virtual SObject op_vector_ref(SObject arg2) { Exn.fault(Constants.EX_VECTOR_REF, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_vector_ref(SVL arg1) { Exn.fault(Constants.EX_VECTOR_REF, null, arg1, this); return Factory.Impossible; }
            //        OP2_VIRTUAL_EXN_PAIR(vector_ref_trusted, EX_VECTOR_REF, SVL)
        public virtual SObject op_vector_set(SObject arg2, SObject arg3) { Exn.fault(Constants.EX_VECTOR_SET, null, this, arg2, arg3); return Factory.Impossible; } public virtual SObject op_reversed_vector_set(SVL arg1, SObject arg3) { Exn.fault(Constants.EX_VECTOR_SET, null, arg1, this, arg3); return Factory.Impossible; }
            //        OP3_VIRTUAL_EXN_PAIR(vector_set_trusted, EX_VECTOR_SET, SVL)

        // Procedure Operations
        public virtual SObject op_procedure_length() { Exn.fault(Constants.EX_PROCEDURE_LENGTH, null, this); return Factory.Impossible; }
        public virtual SObject op_make_procedure() { Exn.fault(Constants.EX_MKVL, null, this); return Factory.Impossible; }
        public virtual SObject op_procedure_ref(SObject arg2) { Exn.fault(Constants.EX_PROCEDURE_REF, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_procedure_ref(Procedure arg1) { Exn.fault(Constants.EX_PROCEDURE_REF, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_procedure_set(SObject arg2, SObject arg3) { Exn.fault(Constants.EX_PROCEDURE_SET, null, this, arg2, arg3); return Factory.Impossible; } public virtual SObject op_reversed_procedure_set(Procedure arg1, SObject arg3) { Exn.fault(Constants.EX_PROCEDURE_SET, null, arg1, this, arg3); return Factory.Impossible; }

        // String Operations
        public virtual SObject op_make_string(SObject arg2) { Exn.fault(Constants.EX_MKBVL, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_make_string(SFixnum arg1) { Exn.fault(Constants.EX_MKBVL, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_string_length() { Exn.fault(Constants.EX_STRING_LENGTH, null, this); return Factory.Impossible; }
        public virtual SObject op_string_ref(SObject arg2) { Exn.fault(Constants.EX_STRING_REF, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_string_ref(SByteVL arg1) { Exn.fault(Constants.EX_STRING_REF, null, arg1, this); return Factory.Impossible; }
            //        OP2_VIRTUAL_EXN_PAIR(string_ref_trusted, EX_STRING_REF, SByteVL)
        public virtual SObject op_string_set(SObject arg2, SObject arg3) { Exn.fault(Constants.EX_STRING_SET, null, this, arg2, arg3); return Factory.Impossible; } public virtual SObject op_reversed_string_set(SByteVL arg1, SObject arg3) { Exn.fault(Constants.EX_STRING_SET, null, arg1, this, arg3); return Factory.Impossible; }
            //        OP3_VIRTUAL_EXN_PAIR(string_set_trusted, EX_STRING_SET, SByteVL)

        // Bytevector Operations
        public virtual SObject op_make_bytevector() { Exn.fault(Constants.EX_MKBVL, null, this); return Factory.Impossible; }
        public virtual SObject op_bytevector_length() { Exn.fault(Constants.EX_BYTEVECTOR_LENGTH, null, this); return Factory.Impossible; }
        public virtual SObject op_bytevector_ref(SObject arg2) { Exn.fault(Constants.EX_BYTEVECTOR_REF, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_bytevector_ref(SByteVL arg1) { Exn.fault(Constants.EX_BYTEVECTOR_REF, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_bytevector_set(SObject arg2, SObject arg3) { Exn.fault(Constants.EX_BYTEVECTOR_SET, null, this, arg2, arg3); return Factory.Impossible; } public virtual SObject op_reversed_bytevector_set(SByteVL arg1, SObject arg3) { Exn.fault(Constants.EX_BYTEVECTOR_SET, null, arg1, this, arg3); return Factory.Impossible; }
        public virtual SObject op_bytevector_equal(SObject arg2) { Exn.fault(Constants.EX_UNSUPPORTED, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_bytevector_equal(SByteVL arg1) { Exn.fault(Constants.EX_UNSUPPORTED, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_bytevector_fill(SObject arg2) { Exn.fault(Constants.EX_BVFILL, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_bytevector_fill(SByteVL arg1) { Exn.fault(Constants.EX_BVFILL, null, arg1, this); return Factory.Impossible; }

        // Bytevector-like Operations
        public virtual SObject op_bytevector_like_length() { Exn.fault(Constants.EX_BVLLEN, null, this); return Factory.Impossible; }
        public virtual SObject op_bytevector_like_ref(SObject arg2) { Exn.fault(Constants.EX_BVLREF, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_bytevector_like_ref(SByteVL arg1) { Exn.fault(Constants.EX_BVLREF, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_bytevector_like_set(SObject arg2, SObject arg3) { Exn.fault(Constants.EX_BVLSET, null, this, arg2, arg3); return Factory.Impossible; } public virtual SObject op_reversed_bytevector_like_set(SByteVL arg1, SObject arg3) { Exn.fault(Constants.EX_BVLSET, null, arg1, this, arg3); return Factory.Impossible; }
        public virtual SObject op_sys_bvlcmp(SObject arg2) { Exn.fault(Constants.EX_UNSUPPORTED, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_sys_bvlcmp(SByteVL arg1) { Exn.fault(Constants.EX_UNSUPPORTED, null, arg1, this); return Factory.Impossible; }

        // Fixnum Operations
        public virtual SObject op_fxzerop() { Exn.fault(Constants.EX_UNSUPPORTED, null, this); return Factory.Impossible; }
        public virtual SObject op_fxpositivep() { Exn.fault(Constants.EX_FXPOSITIVE, null, this); return Factory.Impossible; }
        public virtual SObject op_fxnegativep() { Exn.fault(Constants.EX_FXNEGATIVE, null, this); return Factory.Impossible; }
        public virtual SObject op_fxnegative() { Exn.fault(Constants.EX_FXNEG, null, this); return Factory.Impossible; }
        public virtual SObject op_fxplus(SObject arg2) { Exn.fault(Constants.EX_FXADD, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_fxplus(SFixnum arg1) { Exn.fault(Constants.EX_FXADD, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_fxminus(SObject arg2) { Exn.fault(Constants.EX_FXSUB, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_fxminus(SFixnum arg1) { Exn.fault(Constants.EX_FXSUB, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_fxmul(SObject arg2) { Exn.fault(Constants.EX_FXMUL, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_fxmul(SFixnum arg1) { Exn.fault(Constants.EX_FXMUL, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_fxless(SObject arg2) { Exn.fault(Constants.EX_FXLT, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_fxless(SFixnum arg1) { Exn.fault(Constants.EX_FXLT, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_fxgreater(SObject arg2) { Exn.fault(Constants.EX_FXGT, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_fxgreater(SFixnum arg1) { Exn.fault(Constants.EX_FXGT, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_fxless_equal(SObject arg2) { Exn.fault(Constants.EX_FXLE, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_fxless_equal(SFixnum arg1) { Exn.fault(Constants.EX_FXLE, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_fxgreater_equal(SObject arg2) { Exn.fault(Constants.EX_FXGE, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_fxgreater_equal(SFixnum arg1) { Exn.fault(Constants.EX_FXGE, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_fxequal(SObject arg2) { Exn.fault(Constants.EX_FXEQ, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_fxequal(SFixnum arg1) { Exn.fault(Constants.EX_FXEQ, null, arg1, this); return Factory.Impossible; }

        public SObject op_most_positive_fixnum() { return Factory.makeFixnum(SFixnum.MAX); }
        public SObject op_most_negative_fixnum() { return Factory.makeFixnum(SFixnum.MIN); }

        // Logical Operations
        public virtual SObject op_lognot() { Exn.fault(Constants.EX_LOGNOT, null, this); return Factory.Impossible; }
        public virtual SObject op_logand(SObject arg2) { Exn.fault(Constants.EX_LOGAND, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_logand(SFixnum arg1) { Exn.fault(Constants.EX_LOGAND, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_logior(SObject arg2) { Exn.fault(Constants.EX_LOGIOR, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_logior(SFixnum arg1) { Exn.fault(Constants.EX_LOGIOR, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_logxor(SObject arg2) { Exn.fault(Constants.EX_LOGXOR, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_logxor(SFixnum arg1) { Exn.fault(Constants.EX_LOGXOR, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_lsh(SObject arg2) { Exn.fault(Constants.EX_LSH, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_lsh(SFixnum arg1) { Exn.fault(Constants.EX_LSH, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_rsh(SObject arg2) { Exn.fault(Constants.EX_RSHA, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_rsh(SFixnum arg1) { Exn.fault(Constants.EX_RSHA, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_rsha(SObject arg2) { Exn.fault(Constants.EX_RSHA, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_rsha(SFixnum arg1) { Exn.fault(Constants.EX_RSHA, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_rshl(SObject arg2) { Exn.fault(Constants.EX_RSHL, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_rshl(SFixnum arg1) { Exn.fault(Constants.EX_RSHL, null, arg1, this); return Factory.Impossible; }

        // Arithmetic Operations
        public virtual SObject op_real_part() { Exn.fault(Constants.EX_REALPART, null, this); return Factory.Impossible; }
        public virtual SObject op_imag_part() { Exn.fault(Constants.EX_IMAGPART, null, this); return Factory.Impossible; }

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
        public override SObject op_immediatep() { return Factory.True; } public override bool isImmediate() { return true; }
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
        public override SObject op_charp() { return Factory.True; } public override bool isChar() { return true; }

        public override SObject op_char_lt(SObject arg2) { return arg2.op_reversed_char_lt(this); }
        public override SObject op_char_le(SObject arg2) { return arg2.op_reversed_char_le(this); }
        public override SObject op_char_gt(SObject arg2) { return arg2.op_reversed_char_gt(this); }
        public override SObject op_char_ge(SObject arg2) { return arg2.op_reversed_char_ge(this); }
        public override SObject op_char_equals(SObject arg2) { return arg2.op_reversed_char_equals(this); }
        public override SObject op_reversed_char_lt(SChar arg1) {
            return Factory.wrap(arg1.val < this.val);
        }
        public override SObject op_reversed_char_le(SChar arg1) {
            return Factory.wrap(arg1.val <= this.val);
        }
        public override SObject op_reversed_char_gt(SChar arg1) {
            return Factory.wrap(arg1.val > this.val);
        }
        public override SObject op_reversed_char_ge(SChar arg1) {
            return Factory.wrap(arg1.val >= this.val);
        }
        public override SObject op_reversed_char_equals(SChar arg1) {
            return Factory.wrap(arg1.val == this.val);
        }

        public override SObject op_char2integer() {
            return Factory.wrap(this.val);
        }

        public override SObject op_reversed_make_string(SFixnum arg1) {
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

        public override SObject op_numberp() { return Factory.True; } public override bool isNumber() { return true; }
        public override SObject op_fixnump() { return Factory.True; } public override bool isFixnum() { return true; }
        public override SObject op_complexp() { return Factory.True; } public override bool isComplex() { return true; }
        public override SObject op_realp() { return Factory.True; } public override bool isReal() { return true; }
        public override SObject op_rationalp() { return Factory.True; } public override bool isRational() { return true; }
        public override SObject op_integerp() { return Factory.True; } public override bool isInteger() { return true; }
        public override SObject op_exactp() { return Factory.True; } public override bool isExact() { return true; }
        public override SObject op_inexactp() { return Factory.False; } public override bool isInexact() { return false; }

        public override SObject op_integer2char() {
            return Factory.makeChar(this.value);
        }
        public override SObject op_reversed_typetag_set(STagged arg1) {
            arg1.tag = this.value;
            return Factory.Unspecified;
        }
        public override SObject op_reversed_vector_like_ref(SVL arg1) {
            SObject[] elements = arg1.elements;
            int index = this.value;
            if (0 <= index && index < elements.Length) {
                return elements[index];
            } else {
                Exn.fault(Constants.EX_VLREF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        public override SObject op_reversed_vector_like_set(SVL arg1, SObject arg3) {
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
        public override SObject op_make_vector(SObject arg2) {
            int length = this.value;
            if (0 <= length) {
                return Factory.makeVector(this.value, arg2);
            } else {
                Exn.fault(Constants.EX_MKVL, null, this, arg2);
                return Factory.Impossible;
            }
        }
        public override SObject op_reversed_vector_ref(SVL arg1) {
            SObject[] elements = arg1.elements;
            int index = this.value;
            if (0 <= index && index < elements.Length) {
                return elements[index];
            } else {
                Exn.fault(Constants.EX_VECTOR_REF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        public override SObject op_reversed_vector_set(SVL arg1, SObject arg3) {
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
        public override SObject op_make_procedure() {
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
        public override SObject op_reversed_procedure_ref(Procedure arg1) {
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
        public override SObject op_reversed_procedure_set(Procedure arg1, SObject arg3) {
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

        public override SObject op_make_string(SObject arg2) { return arg2.op_reversed_make_string(this); }

        public override SObject op_make_bytevector() {
            return Factory.makeByteVector(this.value, (byte)0);
        }
        public override SObject op_reversed_bytevector_ref(SByteVL arg1) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            if (0 <= index && index < bytes.Length) {
                return Factory.wrap(bytes[index]);
            } else {
                Exn.fault(Constants.EX_BYTEVECTOR_REF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        public override SObject op_reversed_bytevector_set(SByteVL arg1, SObject arg3) {
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
        public override SObject op_reversed_bytevector_fill(SByteVL arg1) {
            byte[] bytes = arg1.elements;
            byte fill = (byte) this.value;
            for (int i = 0; i < bytes.Length; ++i) {
               bytes[i] = fill;
            }
            return Factory.Unspecified;
        }

        public override SObject op_reversed_bytevector_like_ref(SByteVL arg1) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            if (0 <= index && index < bytes.Length) {
                return Factory.wrap(bytes[index]);
            } else {
                Exn.fault(Constants.EX_BVLREF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        public override SObject op_reversed_bytevector_like_set(SByteVL arg1, SObject arg3) {
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

        public override SObject op_fxzerop() {
            return Factory.wrap(this.value == 0);
        }
        public override SObject op_fxpositivep() {
            return Factory.wrap(this.value > 0);
        }
        public override SObject op_fxnegativep() {
            return Factory.wrap(this.value < 0);
        }
        public override SObject op_fxnegative() {
            int a = - this.value;
            if (!SFixnum.inFixnumRange(-a)) {
                Exn.fault(Constants.EX_FXNEG, "result not a fixnum", this);
                return Factory.Impossible;
            }
            return Factory.wrap(a);
        }
        public override SObject op_fxplus(SObject arg2) { return arg2.op_reversed_fxplus(this); }
        public override SObject op_fxminus(SObject arg2) { return arg2.op_reversed_fxminus(this); }
        public override SObject op_fxmul(SObject arg2) { return arg2.op_reversed_fxmul(this); }
        public override SObject op_fxless(SObject arg2) { return arg2.op_reversed_fxless(this); }
        public override SObject op_fxless_equal(SObject arg2) { return arg2.op_reversed_fxless_equal(this); }
        public override SObject op_fxgreater(SObject arg2) { return arg2.op_reversed_fxgreater(this); }
        public override SObject op_fxgreater_equal(SObject arg2) { return arg2.op_reversed_fxgreater_equal(this); }
        public override SObject op_fxequal(SObject arg2) { return arg2.op_reversed_fxequal(this); }

        public override SObject op_reversed_fxplus(SFixnum arg1) {
            int a = arg1.value, b = this.value;
            int r = a + b;
            if (!SFixnum.inFixnumRange(r)) {
                Exn.fault(Constants.EX_FXADD, null, arg1, this);
                return Factory.Impossible;
            }
            return Factory.wrap(r);
        }
        public override SObject op_reversed_fxminus(SFixnum arg1) {
            int a = arg1.value, b = this.value;
            int r = a - b;
            if (!SFixnum.inFixnumRange(r)) {
                Exn.fault(Constants.EX_FXSUB, null, arg1, this);
                return Factory.Impossible;
            }
            return Factory.wrap(r);
        }
        public override SObject op_reversed_fxmul(SFixnum arg1) {
            int a = arg1.value, b = this.value;
            long r = a * b;
            if (!SFixnum.inFixnumRange(r)) {
                Exn.fault(Constants.EX_FXMUL, null, arg1, this);
                return Factory.Impossible;
            }
            return Factory.wrap((int)r);
        }
        public override SObject op_reversed_fxless(SFixnum arg1) {
            return Factory.wrap(arg1.value < this.value);
        }
        public override SObject op_reversed_fxless_equal(SFixnum arg1) {
            return Factory.wrap(arg1.value <= this.value);
        }
        public override SObject op_reversed_fxgreater(SFixnum arg1) {
            return Factory.wrap(arg1.value > this.value);
        }
        public override SObject op_reversed_fxgreater_equal(SFixnum arg1) {
            return Factory.wrap(arg1.value >= this.value);
        }
        public override SObject op_reversed_fxequal(SFixnum arg1) {
            return Factory.wrap(arg1.value == this.value);
        }

        public override SObject op_lognot() {
            return Factory.wrap(~this.value);
        }
        public override SObject op_logand(SObject arg2) { return arg2.op_reversed_logand(this); }
        public override SObject op_logior(SObject arg2) { return arg2.op_reversed_logior(this); }
        public override SObject op_logxor(SObject arg2) { return arg2.op_reversed_logxor(this); }
        public override SObject op_lsh(SObject arg2) { return arg2.op_reversed_lsh(this); }
        public override SObject op_rsh(SObject arg2) { return arg2.op_reversed_rsh(this); }
        public override SObject op_rsha(SObject arg2) { return arg2.op_reversed_rsha(this); }
        public override SObject op_rshl(SObject arg2) { return arg2.op_reversed_rshl(this); }

        public override SObject op_reversed_logand(SFixnum arg1) {
            return Factory.wrap(arg1.value & this.value);
        }
        public override SObject op_reversed_logior(SFixnum arg1) {
            return Factory.wrap(arg1.value | this.value);
        }
        public override SObject op_reversed_logxor(SFixnum arg1) {
            return Factory.wrap(arg1.value ^ this.value);
        }
        public override SObject op_reversed_lsh(SFixnum arg1) {
            int r = arg1.value << this.value;
            r = (r << 2) >> 2; // mask out top bits (w/ sign extend)
            return Factory.wrap(r);
        }
        public override SObject op_reversed_rsh(SFixnum arg1) {
            int r = arg1.value >> this.value;
            r = (r << 2) >> 2;
            return Factory.wrap(r);
        }
        public override SObject op_reversed_rsha(SFixnum arg1) {
            int r = arg1.value >> this.value;
            r = (r << 2) >> 2;
            return Factory.wrap(r);
        }
        public override SObject op_reversed_rshl(SFixnum arg1) {
            uint a = (uint)arg1.value << 2;
            int b = this.value;
            int r = (int)(a >> b) >> 2;
            return Factory.wrap((int)r);
        }

        public override SObject op_real_part() {
            return this;
        }
        public override SObject op_imag_part() {
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
        public override SObject op_typetag() { return Factory.wrap(this.tag); }
        public override SObject op_typetag_set(SObject arg2) { return arg2.op_reversed_typetag_set(this); }

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

        public override SObject op_numberp() { return Factory.makeBoolean(this.isRatnum() || this.isRectnum()); } public override bool isNumber() { return (this.isRatnum() || this.isRectnum()); }
        public override SObject op_ratnump() { return Factory.makeBoolean(this.tag == Tags.RatnumTag); } public override bool isRatnum() { return (this.tag == Tags.RatnumTag); }
        public override SObject op_rectnump() { return Factory.makeBoolean(this.tag == Tags.RectnumTag); } public override bool isRectnum() { return (this.tag == Tags.RectnumTag); }
        public override SObject op_complexp() { return Factory.makeBoolean(this.isRatnum() || this.isRectnum()); } public override bool isComplex() { return (this.isRatnum() || this.isRectnum()); }
        public override SObject op_realp() { return Factory.makeBoolean(this.isRatnum()); } public override bool isReal() { return (this.isRatnum()); }

        // FIXME!!!! exact?, inexact? should throw errors on non-numbers
        public override bool isExact() { return (this.isRatnum() || this.isRectnum()); }
        public override bool isInexact() { return false; }
        public override SObject op_exactp() {
            if (this.isNumber()) {
                return Factory.wrap(this.isRatnum() || this.isRectnum());
            } else {
                Exn.fault(Constants.EX_EXACTP, null, this);
                return Factory.Impossible;
            }
        }
        public override SObject op_inexactp() {
            if (this.isNumber()) {
                return Factory.False;
            } else {
                Exn.fault(Constants.EX_INEXACTP, null, this);
                return Factory.Impossible;
            }
        }

        public override SObject op_vector_likep() { return Factory.True; } public override bool isVectorLike() { return true; }
        public override SObject op_vectorp() { return Factory.makeBoolean(this.tag == Tags.VectorTag); } public override bool isVector() { return (this.tag == Tags.VectorTag); }
        public override SObject op_portp() { return Factory.makeBoolean(this.tag == Tags.PortTag); } public override bool isPort() { return (this.tag == Tags.PortTag); }
        public override SObject op_structurep() { return Factory.makeBoolean(this.tag == Tags.StructureTag); } public override bool isStructure() { return (this.tag == Tags.StructureTag); }
        public override SObject op_symbolp() { return Factory.makeBoolean(this.tag == Tags.SymbolTag); } public override bool isSymbol() { return (this.tag == Tags.SymbolTag); }

        // -------------------
        public override SObject op_vector_like_length() {
            return Factory.wrap(elements.Length);
        }
        public override SObject op_vector_like_ref(SObject arg2) { return arg2.op_reversed_vector_like_ref(this); }
        public override SObject op_vector_like_set(SObject arg2, SObject arg3) { return arg2.op_reversed_vector_like_set(this, arg3); }

        public override SObject op_vector_length() {
            check_typetag(Tags.VectorTag, Constants.EX_VECTOR_LENGTH);
            return Factory.wrap(elements.Length);
        }
        public override SObject op_vector_ref(SObject arg2) { check_typetag(Tags.VectorTag, arg2, Constants.EX_VECTOR_REF); return arg2.op_reversed_vector_ref(this); }
        public override SObject op_vector_set(SObject arg2, SObject arg3) { check_typetag(Tags.VectorTag, arg2, arg3, Constants.EX_VECTOR_SET); return arg2.op_reversed_vector_set(this, arg3); }

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

        public override SObject op_numberp() { return Factory.makeBoolean(this.isBignum() || this.isFlonum() || this.isCompnum()); } public override bool isNumber() { return (this.isBignum() || this.isFlonum() || this.isCompnum()); }
        public override bool isExact() { return (this.isBignum()); }
        public override bool isInexact() { return (this.isFlonum() || this.isCompnum()); }
        public override SObject op_exactp() {
            if (this.isNumber()) {
                return Factory.wrap(this.isBignum());
            } else {
                Exn.fault(Constants.EX_EXACTP, null, this);
                return Factory.Impossible;
            }
        }
        public override SObject op_inexactp() {
            if (this.isNumber()) {
                return Factory.wrap(this.isFlonum() || this.isCompnum());
            } else {
                Exn.fault(Constants.EX_INEXACTP, null, this);
                return Factory.Impossible;
            }
        }
        public override SObject op_bignump() { return Factory.makeBoolean(this.tag == Tags.BignumTag); } public override bool isBignum() { return (this.tag == Tags.BignumTag); }
        public override SObject op_flonump() { return Factory.makeBoolean(this.tag == Tags.FlonumTag); } public override bool isFlonum() { return (this.tag == Tags.FlonumTag); }
        public override SObject op_compnump() { return Factory.makeBoolean(this.tag == Tags.CompnumTag); } public override bool isCompnum() { return (this.tag == Tags.CompnumTag); }
        public override SObject op_complexp() { return Factory.makeBoolean(this.isBignum() || this.isFlonum() || this.isCompnum()); } public override bool isComplex() { return (this.isBignum() || this.isFlonum() || this.isCompnum()); }
        public override SObject op_realp() { return Factory.makeBoolean(this.isBignum() || this.isFlonum()); } public override bool isReal() { return (this.isBignum() || this.isFlonum()); }
        public override SObject op_rationalp() { return Factory.makeBoolean(this.isBignum() || this.isFlonum()); } public override bool isRational() { return (this.isBignum() || this.isFlonum()); }
        public override SObject op_integerp() { return Factory.makeBoolean(this.isBignum() || (this.isFlonum() && this.isIntegralFlonum())); } public override bool isInteger() { return (this.isBignum() || (this.isFlonum() && this.isIntegralFlonum())); }
        public override SObject op_bytevector_likep() { return Factory.True; } public override bool isByteVectorLike() { return true; }
        public override SObject op_bytevectorp() { return Factory.makeBoolean(this.tag == Tags.ByteVectorTag); } public override bool isByteVector() { return (this.tag == Tags.ByteVectorTag); }
        public override SObject op_stringp() { return Factory.makeBoolean(this.tag == Tags.StringTag); } public override bool isString() { return (this.tag == Tags.StringTag); }
        // ----------------------

        public override SObject op_string_ref(SObject arg2) { check_typetag(Tags.StringTag, arg2, Constants.EX_STRING_REF); return arg2.op_reversed_string_ref(this); }
        public override SObject op_string_set(SObject arg2, SObject arg3) { check_typetag(Tags.StringTag, arg2, arg3, Constants.EX_STRING_SET); return arg2.op_reversed_string_set(this, arg3); }

        public override SObject op_bytevector_length() { check_typetag(Tags.ByteVectorTag, Constants.EX_BYTEVECTOR_LENGTH); return implementation_bytevector_length(); } private SObject implementation_bytevector_length() {
            return Factory.wrap(this.elements.Length);
        }
        public override SObject op_bytevector_ref(SObject arg2) { check_typetag(Tags.ByteVectorTag, arg2, Constants.EX_BYTEVECTOR_REF); return arg2.op_reversed_bytevector_ref(this); }
        public override SObject op_bytevector_set(SObject arg2, SObject arg3) { check_typetag(Tags.ByteVectorTag, arg2, arg3, Constants.EX_BYTEVECTOR_SET); return arg2.op_reversed_bytevector_set(this, arg3); }
        public override SObject op_bytevector_equal(SObject arg2) { check_typetag(Tags.ByteVectorTag, arg2, Constants.EX_UNSUPPORTED); return arg2.op_reversed_bytevector_equal(this); }
        public override SObject op_bytevector_fill(SObject arg2) { check_typetag(Tags.ByteVectorTag, arg2, Constants.EX_BVFILL); return arg2.op_reversed_bytevector_fill(this); }

        public override SObject op_bytevector_like_length() {
            return Factory.wrap(this.elements.Length);
        }
        public override SObject op_bytevector_like_ref(SObject arg2) { return arg2.op_reversed_bytevector_like_ref(this); }
        public override SObject op_bytevector_like_set(SObject arg2, SObject arg3) { return arg2.op_reversed_bytevector_like_set(this, arg3); }
        public override SObject op_sys_bvlcmp(SObject arg2) { return arg2.op_reversed_sys_bvlcmp(this); }

        public override SObject op_reversed_sys_bvlcmp(SByteVL arg1) {
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
        public override SObject op_pairp() { return Factory.True; } public override bool isPair() { return true; }
        public override SObject op_cell_ref() { return this.first; }
        public override SObject op_cell_set(SObject arg2) {
            this.first = arg2;
            return Factory.Unspecified;
        }
        public override SObject op_car() { return this.first; }
        public override SObject op_car_pair() { return this.first; }
        public override SObject op_cdr() { return this.rest; }
        public override SObject op_cdr_pair() { return this.rest; }

        public override SObject op_set_car(SObject arg2) { this.first = arg2; return Factory.Unspecified; }
        public override SObject op_set_car_pair(SObject arg2) { this.first = arg2; return Factory.Unspecified; }
        public override SObject op_set_cdr(SObject arg2) { this.rest = arg2; return Factory.Unspecified; }
        public override SObject op_set_cdr_pair(SObject arg2) { this.rest = arg2; return Factory.Unspecified; }

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
        public override SObject op_procedurep() { return Factory.True; } public override bool isProcedure() { return true; }
        public override SObject op_procedure_length() {
            return Factory.wrap(this.rib.Length + 2);
        }
        public override SObject op_procedure_ref(SObject arg2) { return arg2.op_reversed_procedure_ref(this); }
        public override SObject op_procedure_set(SObject arg2, SObject arg3) { return arg2.op_reversed_procedure_set(this, arg3); }
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

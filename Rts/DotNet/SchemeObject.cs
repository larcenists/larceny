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
/* Special Operations */
/* ---- */
// NC = No Contagion
using System;
using System.Collections;
using System.IO;
using System.Text;
using Scheme.RT;
using Scheme.Rep;

namespace Scheme.Rep {

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


// Operations for SObject
// Mostly, just declares virtual methods that fault with the right excode.

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
        public virtual SObject op_vector_length_vec() { Exn.fault(Constants.EX_VECTOR_LENGTH, null, this); return Factory.Impossible; }
        public virtual SObject op_vector_ref(SObject arg2) { Exn.fault(Constants.EX_VECTOR_REF, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_vector_ref(SVL arg1) { Exn.fault(Constants.EX_VECTOR_REF, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_vector_ref_trusted(SObject arg2) { Exn.fault(Constants.EX_VECTOR_REF, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_vector_ref_trusted(SVL arg1) { Exn.fault(Constants.EX_VECTOR_REF, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_vector_set(SObject arg2, SObject arg3) { Exn.fault(Constants.EX_VECTOR_SET, null, this, arg2, arg3); return Factory.Impossible; } public virtual SObject op_reversed_vector_set(SVL arg1, SObject arg3) { Exn.fault(Constants.EX_VECTOR_SET, null, arg1, this, arg3); return Factory.Impossible; }
        public virtual SObject op_vector_set_trusted(SObject arg2, SObject arg3) { Exn.fault(Constants.EX_VECTOR_SET, null, this, arg2, arg3); return Factory.Impossible; } public virtual SObject op_reversed_vector_set_trusted(SVL arg1, SObject arg3) { Exn.fault(Constants.EX_VECTOR_SET, null, arg1, this, arg3); return Factory.Impossible; }

        // Procedure Operations
        public virtual SObject op_procedure_length() { Exn.fault(Constants.EX_PROCEDURE_LENGTH, null, this); return Factory.Impossible; }
        public virtual SObject op_make_procedure() { Exn.fault(Constants.EX_MKVL, null, this); return Factory.Impossible; }
        public virtual SObject op_procedure_ref(SObject arg2) { Exn.fault(Constants.EX_PROCEDURE_REF, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_procedure_ref(Procedure arg1) { Exn.fault(Constants.EX_PROCEDURE_REF, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_procedure_set(SObject arg2, SObject arg3) { Exn.fault(Constants.EX_PROCEDURE_SET, null, this, arg2, arg3); return Factory.Impossible; } public virtual SObject op_reversed_procedure_set(Procedure arg1, SObject arg3) { Exn.fault(Constants.EX_PROCEDURE_SET, null, arg1, this, arg3); return Factory.Impossible; }

        // String Operations
        public virtual SObject op_make_string(SObject arg2) { Exn.fault(Constants.EX_MKBVL, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_make_string(SFixnum arg1) { Exn.fault(Constants.EX_MKBVL, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_string_length() { Exn.fault(Constants.EX_STRING_LENGTH, null, this); return Factory.Impossible; }
        public virtual SObject op_string_length_str() { Exn.fault(Constants.EX_STRING_LENGTH, null, this); return Factory.Impossible; }
        public virtual SObject op_string_ref(SObject arg2) { Exn.fault(Constants.EX_STRING_REF, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_string_ref(SByteVL arg1) { Exn.fault(Constants.EX_STRING_REF, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_string_ref_trusted(SObject arg2) { Exn.fault(Constants.EX_STRING_REF, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_string_ref_trusted(SByteVL arg1) { Exn.fault(Constants.EX_STRING_REF, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_string_set(SObject arg2, SObject arg3) { Exn.fault(Constants.EX_STRING_SET, null, this, arg2, arg3); return Factory.Impossible; } public virtual SObject op_reversed_string_set(SByteVL arg1, SObject arg3) { Exn.fault(Constants.EX_STRING_SET, null, arg1, this, arg3); return Factory.Impossible; }
        public virtual SObject op_string_set_trusted(SObject arg2, SObject arg3) { Exn.fault(Constants.EX_STRING_SET, null, this, arg2, arg3); return Factory.Impossible; } public virtual SObject op_reversed_string_set_trusted(SByteVL arg1, SObject arg3) { Exn.fault(Constants.EX_STRING_SET, null, arg1, this, arg3); return Factory.Impossible; }

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

        public virtual SObject op_plus_idx_idx(SObject arg2) { Exn.fault(Constants.EX_ADD, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_plus_idx_idx(SFixnum arg1) { Exn.fault(Constants.EX_ADD, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_minus_idx_idx(SObject arg2) { Exn.fault(Constants.EX_SUB, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_minus_idx_idx(SFixnum arg1) { Exn.fault(Constants.EX_SUB, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_equal_fix_fix(SObject arg2) { Exn.fault(Constants.EX_EQUALP, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_equal_fix_fix(SFixnum arg1) { Exn.fault(Constants.EX_EQUALP, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_less_fix_fix(SObject arg2) { Exn.fault(Constants.EX_LESSP, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_less_fix_fix(SFixnum arg1) { Exn.fault(Constants.EX_LESSP, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_lessequal_fix_fix(SObject arg2) { Exn.fault(Constants.EX_LESSEQP, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_lessequal_fix_fix(SFixnum arg1) { Exn.fault(Constants.EX_LESSEQP, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_greater_fix_fix(SObject arg2) { Exn.fault(Constants.EX_GREATERP, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_greater_fix_fix(SFixnum arg1) { Exn.fault(Constants.EX_GREATERP, null, arg1, this); return Factory.Impossible; }
        public virtual SObject op_greaterequal_fix_fix(SObject arg2) { Exn.fault(Constants.EX_GREATEREQP, null, this, arg2); return Factory.Impossible; } public virtual SObject op_reversed_greaterequal_fix_fix(SFixnum arg1) { Exn.fault(Constants.EX_GREATEREQP, null, arg1, this); return Factory.Impossible; }

        // Misc Operations
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

        // -------------------
        // Special Operations
        // -------------------

        public virtual void op_enable_interrupts() { Exn.fault(Constants.EX_EINTR, null, this); }
        public void op_disable_interrupts() {
            if (Reg.interruptsEnabled) {
                        Reg.interruptsEnabled = false;
                        Reg.Result = Factory.makeFixnum((int)Reg.timer);
            } else {
                Reg.Result = Factory.makeBoolean(false);
            }
            Exn.checkSignals();
        }
        public virtual void op_syscall() { Exn.fault(Constants.EX_UNSUPPORTED, null, this); }

        public virtual void op_zerop() { Exn.fault(Constants.EX_ZEROP, null, this); }
        public void op_eqvp(SObject arg2) {
            if (this == arg2) {
                Reg.Result = Factory.True;
            } else {
                this.op_eqvp_not_eq(arg2);
            }
        }
        public virtual void op_eqvp_not_eq(SObject arg2) {
            Reg.Result = Factory.False;
        }
        public virtual void op_reversed_generic_eqvp_not_eq(SObject arg1) {
            Reg.Result = Factory.False;
        }
        public virtual void op_reversed_fixnum_eqvp_not_eq(SFixnum arg1) { this.op_reversed_generic_eqvp_not_eq(arg1); } public virtual void op_reversed_bignum_eqvp_not_eq(SByteVL arg1) { this.op_reversed_generic_eqvp_not_eq(arg1); } public virtual void op_reversed_flonum_eqvp_not_eq(SByteVL arg1) { this.op_reversed_generic_eqvp_not_eq(arg1); } public virtual void op_reversed_compnum_eqvp_not_eq(SByteVL arg1) { this.op_reversed_generic_eqvp_not_eq(arg1); } public virtual void op_reversed_ratnum_eqvp_not_eq(SVL arg1) { this.op_reversed_generic_eqvp_not_eq(arg1); } public virtual void op_reversed_rectnum_eqvp_not_eq(SVL arg1) { this.op_reversed_generic_eqvp_not_eq(arg1); }
        public virtual void op_reversed_char_eqvp_not_eq(SChar arg1) { this.op_reversed_generic_eqvp_not_eq(arg1); }

        public virtual void op_numeric_equals(SObject arg2) { Procedure generic = Call.getSupportProcedure(Constants.MS_GENERIC_EQUAL); Call.econtagion(this, arg2, generic); } public void op_reversed_generic_numeric_equals(SObject arg1) { Procedure MS_GENERIC_EQUAL = Call.getSupportProcedure(Constants.MS_GENERIC_EQUAL); Call.econtagion(arg1, this, MS_GENERIC_EQUAL); } public virtual void op_reversed_fixnum_numeric_equals(SFixnum arg1) { this.op_reversed_generic_numeric_equals(arg1); } public virtual void op_reversed_bignum_numeric_equals(SByteVL arg1) { this.op_reversed_generic_numeric_equals(arg1); } public virtual void op_reversed_flonum_numeric_equals(SByteVL arg1) { this.op_reversed_generic_numeric_equals(arg1); } public virtual void op_reversed_compnum_numeric_equals(SByteVL arg1) { this.op_reversed_generic_numeric_equals(arg1); } public virtual void op_reversed_ratnum_numeric_equals(SVL arg1) { this.op_reversed_generic_numeric_equals(arg1); } public virtual void op_reversed_rectnum_numeric_equals(SVL arg1) { this.op_reversed_generic_numeric_equals(arg1); }
        public virtual void op_less_than(SObject arg2) { Procedure generic = Call.getSupportProcedure(Constants.MS_GENERIC_LESS); Call.pcontagion(this, arg2, generic); } public void op_reversed_generic_less_than(SObject arg1) { Procedure MS_GENERIC_LESS = Call.getSupportProcedure(Constants.MS_GENERIC_LESS); Call.pcontagion(arg1, this, MS_GENERIC_LESS); } public virtual void op_reversed_fixnum_less_than(SFixnum arg1) { this.op_reversed_generic_less_than(arg1); } public virtual void op_reversed_bignum_less_than(SByteVL arg1) { this.op_reversed_generic_less_than(arg1); } public virtual void op_reversed_flonum_less_than(SByteVL arg1) { this.op_reversed_generic_less_than(arg1); } public virtual void op_reversed_compnum_less_than(SByteVL arg1) { this.op_reversed_generic_less_than(arg1); } public virtual void op_reversed_ratnum_less_than(SVL arg1) { this.op_reversed_generic_less_than(arg1); } public virtual void op_reversed_rectnum_less_than(SVL arg1) { this.op_reversed_generic_less_than(arg1); }
        public virtual void op_less_or_equal(SObject arg2) { Procedure generic = Call.getSupportProcedure(Constants.MS_GENERIC_LESSEQ); Call.pcontagion(this, arg2, generic); } public void op_reversed_generic_less_or_equal(SObject arg1) { Procedure MS_GENERIC_LESSEQ = Call.getSupportProcedure(Constants.MS_GENERIC_LESSEQ); Call.pcontagion(arg1, this, MS_GENERIC_LESSEQ); } public virtual void op_reversed_fixnum_less_or_equal(SFixnum arg1) { this.op_reversed_generic_less_or_equal(arg1); } public virtual void op_reversed_bignum_less_or_equal(SByteVL arg1) { this.op_reversed_generic_less_or_equal(arg1); } public virtual void op_reversed_flonum_less_or_equal(SByteVL arg1) { this.op_reversed_generic_less_or_equal(arg1); } public virtual void op_reversed_compnum_less_or_equal(SByteVL arg1) { this.op_reversed_generic_less_or_equal(arg1); } public virtual void op_reversed_ratnum_less_or_equal(SVL arg1) { this.op_reversed_generic_less_or_equal(arg1); } public virtual void op_reversed_rectnum_less_or_equal(SVL arg1) { this.op_reversed_generic_less_or_equal(arg1); }
        public virtual void op_greater_than(SObject arg2) { Procedure generic = Call.getSupportProcedure(Constants.MS_GENERIC_GREATER); Call.pcontagion(this, arg2, generic); } public void op_reversed_generic_greater_than(SObject arg1) { Procedure MS_GENERIC_GREATER = Call.getSupportProcedure(Constants.MS_GENERIC_GREATER); Call.pcontagion(arg1, this, MS_GENERIC_GREATER); } public virtual void op_reversed_fixnum_greater_than(SFixnum arg1) { this.op_reversed_generic_greater_than(arg1); } public virtual void op_reversed_bignum_greater_than(SByteVL arg1) { this.op_reversed_generic_greater_than(arg1); } public virtual void op_reversed_flonum_greater_than(SByteVL arg1) { this.op_reversed_generic_greater_than(arg1); } public virtual void op_reversed_compnum_greater_than(SByteVL arg1) { this.op_reversed_generic_greater_than(arg1); } public virtual void op_reversed_ratnum_greater_than(SVL arg1) { this.op_reversed_generic_greater_than(arg1); } public virtual void op_reversed_rectnum_greater_than(SVL arg1) { this.op_reversed_generic_greater_than(arg1); }
        public virtual void op_greater_or_equal(SObject arg2) { Procedure generic = Call.getSupportProcedure(Constants.MS_GENERIC_GREATEREQ); Call.pcontagion(this, arg2, generic); } public void op_reversed_generic_greater_or_equal(SObject arg1) { Procedure MS_GENERIC_GREATEREQ = Call.getSupportProcedure(Constants.MS_GENERIC_GREATEREQ); Call.pcontagion(arg1, this, MS_GENERIC_GREATEREQ); } public virtual void op_reversed_fixnum_greater_or_equal(SFixnum arg1) { this.op_reversed_generic_greater_or_equal(arg1); } public virtual void op_reversed_bignum_greater_or_equal(SByteVL arg1) { this.op_reversed_generic_greater_or_equal(arg1); } public virtual void op_reversed_flonum_greater_or_equal(SByteVL arg1) { this.op_reversed_generic_greater_or_equal(arg1); } public virtual void op_reversed_compnum_greater_or_equal(SByteVL arg1) { this.op_reversed_generic_greater_or_equal(arg1); } public virtual void op_reversed_ratnum_greater_or_equal(SVL arg1) { this.op_reversed_generic_greater_or_equal(arg1); } public virtual void op_reversed_rectnum_greater_or_equal(SVL arg1) { this.op_reversed_generic_greater_or_equal(arg1); }

        public virtual void op_plus(SObject arg2) { Procedure generic = Call.getSupportProcedure(Constants.MS_GENERIC_ADD); Call.contagion(this, arg2, generic); } public void op_reversed_generic_plus(SObject arg1) { Procedure MS_GENERIC_ADD = Call.getSupportProcedure(Constants.MS_GENERIC_ADD); Call.contagion(arg1, this, MS_GENERIC_ADD); } public virtual void op_reversed_fixnum_plus(SFixnum arg1) { this.op_reversed_generic_plus(arg1); } public virtual void op_reversed_bignum_plus(SByteVL arg1) { this.op_reversed_generic_plus(arg1); } public virtual void op_reversed_flonum_plus(SByteVL arg1) { this.op_reversed_generic_plus(arg1); } public virtual void op_reversed_compnum_plus(SByteVL arg1) { this.op_reversed_generic_plus(arg1); } public virtual void op_reversed_ratnum_plus(SVL arg1) { this.op_reversed_generic_plus(arg1); } public virtual void op_reversed_rectnum_plus(SVL arg1) { this.op_reversed_generic_plus(arg1); }
        public virtual void op_minus(SObject arg2) { Procedure generic = Call.getSupportProcedure(Constants.MS_GENERIC_SUB); Call.contagion(this, arg2, generic); } public void op_reversed_generic_minus(SObject arg1) { Procedure MS_GENERIC_SUB = Call.getSupportProcedure(Constants.MS_GENERIC_SUB); Call.contagion(arg1, this, MS_GENERIC_SUB); } public virtual void op_reversed_fixnum_minus(SFixnum arg1) { this.op_reversed_generic_minus(arg1); } public virtual void op_reversed_bignum_minus(SByteVL arg1) { this.op_reversed_generic_minus(arg1); } public virtual void op_reversed_flonum_minus(SByteVL arg1) { this.op_reversed_generic_minus(arg1); } public virtual void op_reversed_compnum_minus(SByteVL arg1) { this.op_reversed_generic_minus(arg1); } public virtual void op_reversed_ratnum_minus(SVL arg1) { this.op_reversed_generic_minus(arg1); } public virtual void op_reversed_rectnum_minus(SVL arg1) { this.op_reversed_generic_minus(arg1); }
        public virtual void op_multiply(SObject arg2) { Procedure generic = Call.getSupportProcedure(Constants.MS_GENERIC_MUL); Call.contagion(this, arg2, generic); } public void op_reversed_generic_multiply(SObject arg1) { Procedure MS_GENERIC_MUL = Call.getSupportProcedure(Constants.MS_GENERIC_MUL); Call.contagion(arg1, this, MS_GENERIC_MUL); } public virtual void op_reversed_fixnum_multiply(SFixnum arg1) { this.op_reversed_generic_multiply(arg1); } public virtual void op_reversed_bignum_multiply(SByteVL arg1) { this.op_reversed_generic_multiply(arg1); } public virtual void op_reversed_flonum_multiply(SByteVL arg1) { this.op_reversed_generic_multiply(arg1); } public virtual void op_reversed_compnum_multiply(SByteVL arg1) { this.op_reversed_generic_multiply(arg1); } public virtual void op_reversed_ratnum_multiply(SVL arg1) { this.op_reversed_generic_multiply(arg1); } public virtual void op_reversed_rectnum_multiply(SVL arg1) { this.op_reversed_generic_multiply(arg1); }
        public virtual void op_divide(SObject arg2) { Procedure generic = Call.getSupportProcedure(Constants.MS_GENERIC_DIV); Call.contagion(this, arg2, generic); } public void op_reversed_generic_divide(SObject arg1) { Procedure MS_GENERIC_DIV = Call.getSupportProcedure(Constants.MS_GENERIC_DIV); Call.contagion(arg1, this, MS_GENERIC_DIV); } public virtual void op_reversed_fixnum_divide(SFixnum arg1) { this.op_reversed_generic_divide(arg1); } public virtual void op_reversed_bignum_divide(SByteVL arg1) { this.op_reversed_generic_divide(arg1); } public virtual void op_reversed_flonum_divide(SByteVL arg1) { this.op_reversed_generic_divide(arg1); } public virtual void op_reversed_compnum_divide(SByteVL arg1) { this.op_reversed_generic_divide(arg1); } public virtual void op_reversed_ratnum_divide(SVL arg1) { this.op_reversed_generic_divide(arg1); } public virtual void op_reversed_rectnum_divide(SVL arg1) { this.op_reversed_generic_divide(arg1); }

        public virtual void op_quotient(SObject arg2) { Call.callMillicodeSupport2(Constants.MS_HEAVY_QUOTIENT, this, arg2); } public void op_reversed_generic_quotient(SObject arg1) { Call.callMillicodeSupport2(Constants.MS_HEAVY_QUOTIENT, arg1, this); } public virtual void op_reversed_fixnum_quotient(SFixnum arg1) { this.op_reversed_generic_quotient(arg1); } public virtual void op_reversed_bignum_quotient(SByteVL arg1) { this.op_reversed_generic_quotient(arg1); } public virtual void op_reversed_flonum_quotient(SByteVL arg1) { this.op_reversed_generic_quotient(arg1); } public virtual void op_reversed_compnum_quotient(SByteVL arg1) { this.op_reversed_generic_quotient(arg1); } public virtual void op_reversed_ratnum_quotient(SVL arg1) { this.op_reversed_generic_quotient(arg1); } public virtual void op_reversed_rectnum_quotient(SVL arg1) { this.op_reversed_generic_quotient(arg1); }
        public virtual void op_remainder(SObject arg2) { Call.callMillicodeSupport2(Constants.MS_HEAVY_REMAINDER, this, arg2); } public void op_reversed_generic_remainder(SObject arg1) { Call.callMillicodeSupport2(Constants.MS_HEAVY_REMAINDER, arg1, this); } public virtual void op_reversed_fixnum_remainder(SFixnum arg1) { this.op_reversed_generic_remainder(arg1); } public virtual void op_reversed_bignum_remainder(SByteVL arg1) { this.op_reversed_generic_remainder(arg1); } public virtual void op_reversed_flonum_remainder(SByteVL arg1) { this.op_reversed_generic_remainder(arg1); } public virtual void op_reversed_compnum_remainder(SByteVL arg1) { this.op_reversed_generic_remainder(arg1); } public virtual void op_reversed_ratnum_remainder(SVL arg1) { this.op_reversed_generic_remainder(arg1); } public virtual void op_reversed_rectnum_remainder(SVL arg1) { this.op_reversed_generic_remainder(arg1); }

        public virtual void op_truncate() { Exn.fault(Constants.EX_TRUNC, null, this); }
        public virtual void op_round() { Exn.fault(Constants.EX_ROUND, null, this); }
        public virtual void op_negative() { Exn.fault(Constants.EX_NEG, null, this); }
        public virtual void op_exact2inexact() { Call.callMillicodeSupport1(Constants.MS_GENERIC_EXACT2INEXACT, this); }
        public virtual void op_inexact2exact() { Call.callMillicodeSupport1(Constants.MS_GENERIC_INEXACT2EXACT, this); }
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


// Ops for SImmediate

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


// Ops for SChar

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

        // Special Operations
        public override void op_reversed_char_eqvp_not_eq(SChar arg1) {
            Reg.Result = Factory.wrap(arg1.val == this.val);
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


// Ops for SFixnum

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
        public override SObject op_reversed_vector_ref_trusted(SVL arg1) {
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
        public override SObject op_reversed_vector_set_trusted(SVL arg1, SObject arg3) {
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
        public override SObject op_reversed_string_ref(SByteVL arg1) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            if (0 <= index && index < bytes.Length) {
                return Factory.makeChar(bytes[index]);
            } else {
                Exn.fault(Constants.EX_STRING_REF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        public override SObject op_reversed_string_ref_trusted(SByteVL arg1) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            return Factory.makeChar(bytes[index]);
        }
        public override SObject op_reversed_string_set(SByteVL arg1, SObject arg3) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            if (0 <= index && index < bytes.Length) {
                if (arg3 is SChar) {
                    bytes[index] = (byte) ((SChar)arg3).val;
                    return Factory.Unspecified;
                }
            }
            Exn.fault(Constants.EX_STRING_REF, null, arg1, this);
            return Factory.Impossible;
        }
        public override SObject op_reversed_string_set_trusted(SByteVL arg1, SObject arg3) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            bytes[index] = (byte) ((SChar)arg3).val;
            return Factory.Unspecified;
        }

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

        public override SObject op_plus_idx_idx(SObject arg2) { return arg2.op_reversed_plus_idx_idx(this); }
        public override SObject op_minus_idx_idx(SObject arg2) { return arg2.op_reversed_minus_idx_idx(this); }
        public override SObject op_equal_fix_fix(SObject arg2) { return arg2.op_reversed_equal_fix_fix(this); }
        public override SObject op_less_fix_fix(SObject arg2) { return arg2.op_reversed_less_fix_fix(this); }
        public override SObject op_lessequal_fix_fix(SObject arg2) { return arg2.op_reversed_lessequal_fix_fix(this); }
        public override SObject op_greater_fix_fix(SObject arg2) { return arg2.op_reversed_greater_fix_fix(this); }
        public override SObject op_greaterequal_fix_fix(SObject arg2) { return arg2.op_reversed_greaterequal_fix_fix(this); }

        public override SObject op_reversed_plus_idx_idx(SFixnum arg1) {
            return Factory.wrap(arg1.value + this.value);
        }
        public override SObject op_reversed_minus_idx_idx(SFixnum arg1) {
            return Factory.wrap(arg1.value - this.value);
        }
        public override SObject op_reversed_equal_fix_fix(SFixnum arg1) {
            return Factory.wrap(arg1.value == this.value);
        }
        public override SObject op_reversed_less_fix_fix(SFixnum arg1) {
            return Factory.wrap(arg1.value < this.value);
        }
        public override SObject op_reversed_lessequal_fix_fix(SFixnum arg1) {
            return Factory.wrap(arg1.value <= this.value);
        }
        public override SObject op_reversed_greater_fix_fix(SFixnum arg1) {
            return Factory.wrap(arg1.value > this.value);
        }
        public override SObject op_reversed_greaterequal_fix_fix(SFixnum arg1) {
            return Factory.wrap(arg1.value >= this.value);
        }

        // Special Operations

        public override void op_enable_interrupts() {
            int time = this.value;
            if (time > 0) {
                Reg.interruptsEnabled = true;
                Reg.timer = time;
            } else {
                Exn.fault(Constants.EX_EINTR, null, this);
            }
            Reg.Result = Factory.Unspecified;
            Exn.checkSignals();
        }
        public override void op_syscall() {
            // subtract one 'cuz the first arg is just the value
            // to which we want to dispatch.
            int num_args = this.value - 1;
            Sys num_syscall = (Sys) ((SFixnum)Reg.register1).intValue();
            Syscall.dispatch(num_args, num_syscall);
        }
        public override void op_zerop() {
            Reg.Result = Factory.wrap(this.value == 0);
        }

        public override void op_eqvp_not_eq(SObject arg2) { arg2.op_reversed_fixnum_eqvp_not_eq(this); }
        public override void op_numeric_equals(SObject arg2) { arg2.op_reversed_fixnum_numeric_equals(this); }
        public override void op_less_than(SObject arg2) { arg2.op_reversed_fixnum_less_than(this); }
        public override void op_less_or_equal(SObject arg2) { arg2.op_reversed_fixnum_less_or_equal(this); }
        public override void op_greater_than(SObject arg2) { arg2.op_reversed_fixnum_greater_than(this); }
        public override void op_greater_or_equal(SObject arg2) { arg2.op_reversed_fixnum_greater_or_equal(this); }

        public override void op_plus(SObject arg2) { arg2.op_reversed_fixnum_plus(this); }
        public override void op_minus(SObject arg2) { arg2.op_reversed_fixnum_minus(this); }
        public override void op_multiply(SObject arg2) { arg2.op_reversed_fixnum_multiply(this); }
        public override void op_divide(SObject arg2) { arg2.op_reversed_fixnum_divide(this); }
        public override void op_quotient(SObject arg2) { arg2.op_reversed_fixnum_quotient(this); }
        public override void op_remainder(SObject arg2) { arg2.op_reversed_fixnum_remainder(this); }

        public override void op_reversed_fixnum_eqvp_not_eq(SFixnum arg1) {
            Reg.Result = Factory.wrap(arg1.value == this.value);
        }
        public override void op_reversed_fixnum_numeric_equals(SFixnum arg1) {
            Reg.Result = Factory.wrap(arg1.value == this.value);
        }
        public override void op_reversed_fixnum_less_than(SFixnum arg1) {
            Reg.Result = Factory.wrap(arg1.value < this.value);
        }
        public override void op_reversed_fixnum_less_or_equal(SFixnum arg1) {
            Reg.Result = Factory.wrap(arg1.value <= this.value);
        }
        public override void op_reversed_fixnum_greater_than(SFixnum arg1) {
            Reg.Result = Factory.wrap(arg1.value > this.value);
        }
        public override void op_reversed_fixnum_greater_or_equal(SFixnum arg1) {
            Reg.Result = Factory.wrap(arg1.value >= this.value);
        }

        public override void op_reversed_fixnum_plus(SFixnum arg1) {
            Reg.Result = Factory.wrap(arg1.value + this.value);
        }
        public override void op_reversed_fixnum_minus(SFixnum arg1) {
            Reg.Result = Factory.wrap(arg1.value - this.value);
        }
        public override void op_reversed_fixnum_multiply(SFixnum arg1) {
            long a = arg1.value;
            long b = this.value;
            Reg.Result = Factory.wrap(a * b);
        }
        public override void op_reversed_fixnum_divide(SFixnum arg1) {
            int a = arg1.value, b = this.value;
            if (b == 0) {
                Exn.fault(Constants.EX_DIV, null, arg1, this);
            }
            if (a % b == 0) {
                Reg.Result = Factory.wrap(a / b);
            } else {
                Call.callMillicodeSupport2(Constants.MS_FIXNUM2RATNUM_DIV, arg1, this);
            }
        }
        public override void op_reversed_fixnum_quotient(SFixnum arg1) {
            int a = arg1.value, b = this.value;
            if (b == 0) {
                Exn.fault(Constants.EX_QUOTIENT, null, arg1, this);
            }
            Reg.Result = Factory.wrap(a / b);
        }
        public override void op_reversed_bignum_quotient(SByteVL arg1) {
            // Must handle 1-word bignums too.
            // Exn.debug.WriteLine("trying bignum/fixnum case: {0}; {1}", arg1, this);
            int bv = this.value;
            if (bv == 0) {
                Exn.fault(Constants.EX_QUOTIENT, null, arg1, this);
                return;
            } else if (bv > 0 &&
                       Number.getBignumLength(arg1) == 1 &&
                       Number.getBignumSign(arg1) == Number.BIGNUM_POSITIVE) {
                uint av = arg1.getUInt32(1);
                uint result = av / (uint)bv;
                // Exn.debug.WriteLine("  / {0}; {1}", av, bv);
                Reg.Result = Factory.makeNumber(result);
                return;
            }
            base.op_reversed_generic_quotient(arg1);
        }

        public override void op_reversed_fixnum_remainder(SFixnum arg1) {
            int a = arg1.value, b = this.value;
            if (b == 0) {
                Exn.fault(Constants.EX_REMAINDER, null, arg1, this);
            }
            Reg.Result = Factory.wrap(a % b);
        }
        public override void op_reversed_bignum_remainder(SByteVL arg1) {
            // Must handle 1-word bignums too.
            int bv = this.value;
            if (bv > 0 &&
                Number.getBignumLength(arg1) == 1 &&
                Number.getBignumSign(arg1) == Number.BIGNUM_POSITIVE) {
                uint av = arg1.getUInt32(1);
                uint result = av % (uint)bv;
                Reg.Result = Factory.makeNumber(result);
                return;
            }
            base.op_reversed_generic_remainder(arg1);
        }

        public override void op_truncate() {
            Reg.Result = this;
        }
        public override void op_round() {
            Reg.Result = this;
        }
        public override void op_negative() {
            Reg.Result = Factory.wrap(-this.value);
        }
        public override void op_exact2inexact() {
            Reg.Result = Factory.makeFlonum((double)this.value);
        }
        public override void op_inexact2exact() {
            Reg.Result = this;
        }
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


// Ops for STagged (ops common to SVL, SByteVL)

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


// Ops for SVL

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
        public override SObject op_vector_length_vec() {
            return Factory.wrap(elements.Length);
        }

        public override SObject op_vector_ref(SObject arg2) { check_typetag(Tags.VectorTag, arg2, Constants.EX_VECTOR_REF); return arg2.op_reversed_vector_ref(this); }
        public override SObject op_vector_ref_trusted(SObject arg2) { return arg2.op_reversed_vector_ref_trusted(this); }
        public override SObject op_vector_set(SObject arg2, SObject arg3) { check_typetag(Tags.VectorTag, arg2, arg3, Constants.EX_VECTOR_SET); return arg2.op_reversed_vector_set(this, arg3); }
        public override SObject op_vector_set_trusted(SObject arg2, SObject arg3) { return arg2.op_reversed_vector_set_trusted(this, arg3); }

        // Special Operations

        public override void op_zerop() {
            if (this.tag == Tags.RectnumTag) {
                Reg.Result = Factory.False;
            } else if (this.tag == Tags.RatnumTag) {
                Reg.Result = Factory.False;
            } else {
                base.op_zerop();
            }
        }

        public override void op_eqvp_not_eq(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_eqvp_not_eq(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_eqvp_not_eq(this); } else { base.op_eqvp_not_eq(this); } }
        public override void op_numeric_equals(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_numeric_equals(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_numeric_equals(this); } else { base.op_numeric_equals(this); } }
        public override void op_less_than(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_less_than(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_less_than(this); } else { base.op_less_than(this); } }
        public override void op_less_or_equal(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_less_or_equal(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_less_or_equal(this); } else { base.op_less_or_equal(this); } }
        public override void op_greater_than(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_greater_than(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_greater_than(this); } else { base.op_greater_than(this); } }
        public override void op_greater_or_equal(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_greater_or_equal(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_greater_or_equal(this); } else { base.op_greater_or_equal(this); } }

        public override void op_plus(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_plus(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_plus(this); } else { base.op_plus(this); } }
        public override void op_minus(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_minus(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_minus(this); } else { base.op_minus(this); } }
        public override void op_multiply(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_multiply(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_multiply(this); } else { base.op_multiply(this); } }
        public override void op_divide(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_divide(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_divide(this); } else { base.op_divide(this); } }
        public override void op_quotient(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_quotient(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_quotient(this); } else { base.op_quotient(this); } }
        public override void op_remainder(SObject arg2) { if (this.tag == Tags.RatnumTag) { arg2.op_reversed_ratnum_remainder(this); } else if (this.tag == Tags.RectnumTag) { arg2.op_reversed_rectnum_remainder(this); } else { base.op_remainder(this); } }

        // Ratnums
        public override void op_reversed_ratnum_eqvp_not_eq(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RATNUM_EQUAL, arg1, this); } else { base.op_reversed_ratnum_eqvp_not_eq(arg1); } }
        public override void op_reversed_ratnum_numeric_equals(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RATNUM_EQUAL, arg1, this); } else { base.op_reversed_ratnum_numeric_equals(arg1); } }
        public override void op_reversed_ratnum_less_than(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RATNUM_LESS, arg1, this); } else { base.op_reversed_ratnum_less_than(arg1); } }
        public override void op_reversed_ratnum_less_or_equal(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RATNUM_LESSEQ, arg1, this); } else { base.op_reversed_ratnum_less_or_equal(arg1); } }
        public override void op_reversed_ratnum_greater_than(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RATNUM_GREATER, arg1, this); } else { base.op_reversed_ratnum_greater_than(arg1); } }
        public override void op_reversed_ratnum_greater_or_equal(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RATNUM_GREATEREQ, arg1, this); } else { base.op_reversed_ratnum_greater_or_equal(arg1); } }


        public override void op_reversed_ratnum_plus(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RATNUM_ADD, arg1, this); } else { base.op_reversed_ratnum_plus(arg1); } }
        public override void op_reversed_ratnum_minus(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RATNUM_SUB, arg1, this); } else { base.op_reversed_ratnum_minus(arg1); } }
        public override void op_reversed_ratnum_multiply(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RATNUM_MUL, arg1, this); } else { base.op_reversed_ratnum_multiply(arg1); } }
        public override void op_reversed_ratnum_divide(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RATNUM_DIV, arg1, this); } else { base.op_reversed_ratnum_divide(arg1); } }

        // Rectnums
        public override void op_reversed_rectnum_eqvp_not_eq(SVL arg1) { if (this.tag == Tags.RectnumTag) { Call.callMillicodeSupport2(Constants.MS_RECTNUM_EQUAL, arg1, this); } else { base.op_reversed_rectnum_eqvp_not_eq(arg1); } }
        public override void op_reversed_rectnum_numeric_equals(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RECTNUM_EQUAL, arg1, this); } else { base.op_reversed_rectnum_numeric_equals(arg1); } }

        public override void op_reversed_rectnum_plus(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RECTNUM_ADD, arg1, this); } else { base.op_reversed_rectnum_plus(arg1); } }
        public override void op_reversed_rectnum_minus(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RECTNUM_SUB, arg1, this); } else { base.op_reversed_rectnum_minus(arg1); } }
        public override void op_reversed_rectnum_multiply(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RECTNUM_MUL, arg1, this); } else { base.op_reversed_rectnum_multiply(arg1); } }
        public override void op_reversed_rectnum_divide(SVL arg1) { if (this.tag == Tags.RatnumTag) { Call.callMillicodeSupport2(Constants.MS_RECTNUM_DIV, arg1, this); } else { base.op_reversed_rectnum_divide(arg1); } }

        public override void op_truncate() {
            if (this.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RATNUM_TRUNCATE, this);
            } else {
                base.op_truncate();
            }
        }
        public override void op_round() {
            if (this.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RATNUM_ROUND, this);
            } else {
                base.op_truncate();
            }
        }
        public override void op_negative() {
            if (this.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RATNUM_NEGATE, this);
            } else if (this.tag == Tags.RectnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RECTNUM_NEGATE, this);
            } else {
                base.op_truncate();
            }
        }
        public override void op_inexact2exact() {
            if (this.tag == Tags.RatnumTag) {
                Reg.Result = this;
            } else if (this.tag == Tags.RectnumTag) {
                Reg.Result = this;
            } else {
                base.op_truncate();
            }
        }
    }

    // -------------------------------------------
    // SByteVL (bytevector-like)
    // -------------------------------------------
    public sealed class SByteVL : STagged {
        public readonly byte[] elements;
        public static System.Text.Encoding stringEncoding
            = new System.Text.ASCIIEncoding();

        public SByteVL(int tag, byte[] vec) {
            this.tag = tag;
            this.elements = vec;
        }
        public SByteVL(int tag, int size, byte fill) {
            this.tag = tag;
            this.elements = new byte[size];
            for (int i = 0; i < size; ++i) {elements[i] = fill;}
        }

        public int length() {
            return elements.Length;
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
            return System.BitConverter.ToDouble(elements, 4 + steps * 8);
              // steps * sizeof(double)) + offset
        }
        public void unsafeSetDouble(int steps, double d) {
            byte[] b = System.BitConverter.GetBytes(d);
            for (int i = 0; i < 8; ++i) {
                elements[i+4 + steps * 8] = b[i];
            }
        }

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


// Ops for SByteVL

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

        public override SObject op_string_length() {
            check_typetag(Tags.StringTag, this, Constants.EX_STRING_LENGTH);
            return Factory.wrap(this.elements.Length);
        }
        public override SObject op_string_length_str() {
            check_typetag(Tags.StringTag, this, Constants.EX_STRING_LENGTH);
            return Factory.wrap(this.elements.Length);
        }

        public override SObject op_string_ref(SObject arg2) { check_typetag(Tags.StringTag, arg2, Constants.EX_STRING_REF); return arg2.op_reversed_string_ref(this); }
        public override SObject op_string_set(SObject arg2, SObject arg3) { check_typetag(Tags.StringTag, arg2, arg3, Constants.EX_STRING_SET); return arg2.op_reversed_string_set(this, arg3); }
        public override SObject op_string_ref_trusted(SObject arg2) { return arg2.op_reversed_string_ref_trusted(this); }
        public override SObject op_string_set_trusted(SObject arg2, SObject arg3) { return arg2.op_reversed_string_set_trusted(this, arg3); }

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

        // Special Operation

        public override void op_zerop() {
            if (this.tag == Tags.BignumTag) {
                Reg.Result = Factory.wrap(Number.getBignumLength(this) == 0);
            } else if (this.tag == Tags.FlonumTag) {
                Reg.Result = Factory.wrap(this.unsafeAsDouble(0) == 0.0);
            } else if (this.tag == Tags.CompnumTag) {
                Reg.Result = Factory.wrap(this.unsafeAsDouble(0) == 0.0 &&
                                           this.unsafeAsDouble(1) == 0.0);
            } else {
                base.op_zerop();
            }
        }

        public override void op_eqvp_not_eq(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_eqvp_not_eq(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_eqvp_not_eq(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_eqvp_not_eq(this); } else { base.op_eqvp_not_eq(this); } }
        public override void op_numeric_equals(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_numeric_equals(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_numeric_equals(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_numeric_equals(this); } else { base.op_numeric_equals(this); } }
        public override void op_less_than(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_less_than(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_less_than(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_less_than(this); } else { base.op_less_than(this); } }
        public override void op_less_or_equal(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_less_or_equal(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_less_or_equal(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_less_or_equal(this); } else { base.op_less_or_equal(this); } }
        public override void op_greater_than(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_greater_than(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_greater_than(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_greater_than(this); } else { base.op_greater_than(this); } }
        public override void op_greater_or_equal(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_greater_or_equal(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_greater_or_equal(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_greater_or_equal(this); } else { base.op_greater_or_equal(this); } }

        public override void op_plus(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_plus(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_plus(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_plus(this); } else { base.op_plus(this); } }
        public override void op_minus(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_minus(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_minus(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_minus(this); } else { base.op_minus(this); } }
        public override void op_multiply(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_multiply(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_multiply(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_multiply(this); } else { base.op_multiply(this); } }
        public override void op_divide(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_divide(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_divide(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_divide(this); } else { base.op_divide(this); } }
        public override void op_quotient(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_quotient(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_quotient(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_quotient(this); } else { base.op_quotient(this); } }
        public override void op_remainder(SObject arg2) { if (this.tag == Tags.BignumTag) { arg2.op_reversed_bignum_remainder(this); } else if (this.tag == Tags.FlonumTag) { arg2.op_reversed_flonum_remainder(this); } else if (this.tag == Tags.CompnumTag) { arg2.op_reversed_compnum_remainder(this); } else { base.op_remainder(this); } }

        // Bignums
        public override void op_reversed_bignum_eqvp_not_eq(SByteVL arg1) { if (this.tag == Tags.BignumTag) { Call.callMillicodeSupport2(Constants.MS_BIGNUM_EQUAL, arg1, this); } else { base.op_reversed_bignum_eqvp_not_eq(arg1); } }

        public override void op_reversed_bignum_numeric_equals(SByteVL arg1) { if (this.tag == Tags.BignumTag) { Call.callMillicodeSupport2(Constants.MS_BIGNUM_EQUAL, arg1, this); } else { base.op_reversed_bignum_numeric_equals(arg1); } }

        public override void op_reversed_bignum_less_than(SByteVL arg1) { if (this.tag == Tags.BignumTag) { Call.callMillicodeSupport2(Constants.MS_BIGNUM_LESS, arg1, this); } else { base.op_reversed_bignum_less_than(arg1); } }

        public override void op_reversed_bignum_less_or_equal(SByteVL arg1) { if (this.tag == Tags.BignumTag) { Call.callMillicodeSupport2(Constants.MS_BIGNUM_LESSEQ, arg1, this); } else { base.op_reversed_bignum_less_or_equal(arg1); } }

        public override void op_reversed_bignum_greater_than(SByteVL arg1) { if (this.tag == Tags.BignumTag) { Call.callMillicodeSupport2(Constants.MS_BIGNUM_GREATER, arg1, this); } else { base.op_reversed_bignum_greater_than(arg1); } }

        public override void op_reversed_bignum_greater_or_equal(SByteVL arg1) { if (this.tag == Tags.BignumTag) { Call.callMillicodeSupport2(Constants.MS_BIGNUM_GREATEREQ, arg1, this); } else { base.op_reversed_bignum_greater_or_equal(arg1); } }


        public override void op_reversed_bignum_plus(SByteVL arg1) { if (this.tag == Tags.BignumTag) { Call.callMillicodeSupport2(Constants.MS_BIGNUM_ADD, arg1, this); } else { base.op_reversed_bignum_plus(arg1); } }
        public override void op_reversed_bignum_minus(SByteVL arg1) { if (this.tag == Tags.BignumTag) { Call.callMillicodeSupport2(Constants.MS_BIGNUM_SUB, arg1, this); } else { base.op_reversed_bignum_minus(arg1); } }
        public override void op_reversed_bignum_multiply(SByteVL arg1) { if (this.tag == Tags.BignumTag) { Call.callMillicodeSupport2(Constants.MS_BIGNUM_MUL, arg1, this); } else { base.op_reversed_bignum_multiply(arg1); } }
        public override void op_reversed_bignum_divide(SByteVL arg1) { if (this.tag == Tags.BignumTag) { Call.callMillicodeSupport2(Constants.MS_BIGNUM_DIV, arg1, this); } else { base.op_reversed_bignum_divide(arg1); } }

        // Flonums
        public override void op_reversed_flonum_eqvp_not_eq(SByteVL arg1) { if (this.tag == Tags.FlonumTag) { this.op_reversed_flonum_FlonumTag_eqvp_not_eq(arg1); } else { base.op_reversed_flonum_eqvp_not_eq(arg1); } } private void op_reversed_flonum_FlonumTag_eqvp_not_eq(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) == this.unsafeAsDouble(0));
        }
        public override void op_reversed_flonum_numeric_equals(SByteVL arg1) { if (this.tag == Tags.FlonumTag) { this.op_reversed_flonum_FlonumTag_numeric_equals(arg1); } else { base.op_reversed_flonum_numeric_equals(arg1); } } private void op_reversed_flonum_FlonumTag_numeric_equals(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) == this.unsafeAsDouble(0));
        }
        public override void op_reversed_flonum_less_than(SByteVL arg1) { if (this.tag == Tags.FlonumTag) { this.op_reversed_flonum_FlonumTag_less_than(arg1); } else { base.op_reversed_flonum_less_than(arg1); } } private void op_reversed_flonum_FlonumTag_less_than(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) < this.unsafeAsDouble(0));
        }
        public override void op_reversed_flonum_less_or_equal(SByteVL arg1) { if (this.tag == Tags.FlonumTag) { this.op_reversed_flonum_FlonumTag_less_or_equal(arg1); } else { base.op_reversed_flonum_less_or_equal(arg1); } } private void op_reversed_flonum_FlonumTag_less_or_equal(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) <= this.unsafeAsDouble(0));
        }
        public override void op_reversed_flonum_greater_than(SByteVL arg1) { if (this.tag == Tags.FlonumTag) { this.op_reversed_flonum_FlonumTag_greater_than(arg1); } else { base.op_reversed_flonum_greater_than(arg1); } } private void op_reversed_flonum_FlonumTag_greater_than(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) > this.unsafeAsDouble(0));
        }
        public override void op_reversed_flonum_greater_or_equal(SByteVL arg1) { if (this.tag == Tags.FlonumTag) { this.op_reversed_flonum_FlonumTag_greater_or_equal(arg1); } else { base.op_reversed_flonum_greater_or_equal(arg1); } } private void op_reversed_flonum_FlonumTag_greater_or_equal(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) >= this.unsafeAsDouble(0));
        }

        public override void op_reversed_flonum_plus(SByteVL arg1) { if (this.tag == Tags.FlonumTag) { this.op_reversed_flonum_FlonumTag_plus(arg1); } else { base.op_reversed_flonum_plus(arg1); } } private void op_reversed_flonum_FlonumTag_plus(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) + this.unsafeAsDouble(0));
        }
        public override void op_reversed_flonum_minus(SByteVL arg1) { if (this.tag == Tags.FlonumTag) { this.op_reversed_flonum_FlonumTag_minus(arg1); } else { base.op_reversed_flonum_minus(arg1); } } private void op_reversed_flonum_FlonumTag_minus(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) - this.unsafeAsDouble(0));
        }
        public override void op_reversed_flonum_multiply(SByteVL arg1) { if (this.tag == Tags.FlonumTag) { this.op_reversed_flonum_FlonumTag_multiply(arg1); } else { base.op_reversed_flonum_multiply(arg1); } } private void op_reversed_flonum_FlonumTag_multiply(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) * this.unsafeAsDouble(0));
        }
        public override void op_reversed_flonum_divide(SByteVL arg1) { if (this.tag == Tags.FlonumTag) { this.op_reversed_flonum_FlonumTag_divide(arg1); } else { base.op_reversed_flonum_divide(arg1); } } private void op_reversed_flonum_FlonumTag_divide(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) / this.unsafeAsDouble(0));
        }

        // Compnums
        public override void op_reversed_compnum_eqvp_not_eq(SByteVL arg1) { if (this.tag == Tags.CompnumTag) { this.op_reversed_compnum_CompnumTag_eqvp_not_eq(arg1); } else { base.op_reversed_compnum_eqvp_not_eq(arg1); } } private void op_reversed_compnum_CompnumTag_eqvp_not_eq(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) == this.unsafeAsDouble(0)
                                      &&
                                      arg1.unsafeAsDouble(1) == this.unsafeAsDouble(1));
        }
        public override void op_reversed_compnum_numeric_equals(SByteVL arg1) { if (this.tag == Tags.CompnumTag) { this.op_reversed_compnum_CompnumTag_numeric_equals(arg1); } else { base.op_reversed_compnum_numeric_equals(arg1); } } private void op_reversed_compnum_CompnumTag_numeric_equals(SByteVL arg1) {
            Reg.Result = Factory.wrap(arg1.unsafeAsDouble(0) == this.unsafeAsDouble(0)
                                      &&
                                      arg1.unsafeAsDouble(1) == this.unsafeAsDouble(1));
        }

        public override void op_reversed_compnum_plus(SByteVL arg1) { if (this.tag == Tags.CompnumTag) { this.op_reversed_compnum_CompnumTag_plus(arg1); } else { base.op_reversed_compnum_plus(arg1); } } private void op_reversed_compnum_CompnumTag_plus(SByteVL arg1) {
            Reg.Result = Factory.makeCompnum
                (arg1.unsafeAsDouble(0) + this.unsafeAsDouble(0),
                 arg1.unsafeAsDouble(1) + this.unsafeAsDouble(1));
        }
        public override void op_reversed_compnum_minus(SByteVL arg1) { if (this.tag == Tags.CompnumTag) { this.op_reversed_compnum_CompnumTag_minus(arg1); } else { base.op_reversed_compnum_minus(arg1); } } private void op_reversed_compnum_CompnumTag_minus(SByteVL arg1) {
            Reg.Result = Factory.makeCompnum
                (arg1.unsafeAsDouble(0) - this.unsafeAsDouble(0),
                 arg1.unsafeAsDouble(1) - this.unsafeAsDouble(1));
        }
        public override void op_reversed_compnum_multiply(SByteVL arg1) { if (this.tag == Tags.CompnumTag) { this.op_reversed_compnum_CompnumTag_multiply(arg1); } else { base.op_reversed_compnum_multiply(arg1); } } private void op_reversed_compnum_CompnumTag_multiply(SByteVL arg1) {
            double ar = arg1.unsafeAsDouble(0), ai = arg1.unsafeAsDouble(1);
            double br = this.unsafeAsDouble(0), bi = this.unsafeAsDouble(1);
            if (ai == 0) {
                Reg.Result = Factory.makeCompnum(ar * br, ar * bi);
            } else if (bi == 0) {
                Reg.Result = Factory.makeCompnum(ar * br, ai * br);
            } else {
                Reg.Result = Factory.makeCompnum
                    (ar * br - ai * bi,
                     ar * bi + ai * br);
            }
        }
        public override void op_reversed_compnum_divide(SByteVL arg1) { if (this.tag == Tags.CompnumTag) { this.op_reversed_compnum_CompnumTag_divide(arg1); } else { base.op_reversed_compnum_divide(arg1); } } private void op_reversed_compnum_CompnumTag_divide(SByteVL arg1) {
            double ar = arg1.unsafeAsDouble(0), ai = arg1.unsafeAsDouble(1);
            double br = this.unsafeAsDouble(0), bi = this.unsafeAsDouble(1);
            double denom = br * br + bi * bi;
            Reg.Result = Factory.makeCompnum
                ((ar * br + ai * bi) / denom,
                 (ai * br - ar * bi) / denom);
        }

        public override void op_truncate() {
            if (this.tag == Tags.BignumTag) {
                Reg.Result = this;
            } else if (this.tag == Tags.FlonumTag) {
                double d = this.unsafeAsDouble(0);
                if (d < 0) {
                    Reg.Result = Factory.makeFlonum(System.Math.Ceiling(d));
                } else {
                    Reg.Result = Factory.makeFlonum(System.Math.Floor(d));
                }
            } else {
                base.op_truncate();
            }
        }
        public override void op_round() {
            if (this.tag == Tags.BignumTag) {
                Reg.Result = this;
            } else if (this.tag == Tags.FlonumTag) {
                double d = this.unsafeAsDouble(0);
                Reg.Result = Factory.makeFlonum(System.Math.Round(d));
            } else {
                base.op_round();
            }
        }
        public override void op_negative() {
            if (this.tag == Tags.BignumTag) {
                Call.callMillicodeSupport1(Constants.MS_BIGNUM_NEGATE, this);
            } else if (this.tag == Tags.FlonumTag) {
                Reg.Result = Factory.makeFlonum
                    (-this.unsafeAsDouble(0));
            } else if (this.tag == Tags.CompnumTag) {
                Reg.Result = Factory.makeCompnum
                    (-this.unsafeAsDouble(0),
                     -this.unsafeAsDouble(1));
            } else {
                base.op_negative();
            }
        }
        public override void op_exact2inexact() {
            if (this.tag == Tags.FlonumTag) {
                Reg.Result = this;
            } else if (this.tag == Tags.CompnumTag) {
                Reg.Result = this;
            } else {
                base.op_exact2inexact();
            }
        }
        public override void op_inexact2exact() {
            if (this.tag == Tags.BignumTag) {
                Reg.Result = this;
            } else {
                base.op_inexact2exact();
            }
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


// Ops for SPair

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

        private string getName() {
            if (this.constants.Length >= 1) {
                SObject d = this.constants[0];
                if (d is SVL) {
                    SVL dd = (SVL) d;
                    if (dd.elements != null && dd.elements.Length >= 1) {
                        return dd.elements[0].ToString();
                    }
                }
            }
            if (rib != null && rib.Length > 0) {
                return ((Procedure)rib[0]).getName();
            } else {
                return "<unknown>";
            }

        }
        public override void write(TextWriter w) {
            w.Write("#<PROCEDURE: ");
            w.Write(getName());
            w.Write(" = ");
            w.Write(entrypoint.name());
            w.Write(">");
        }


// Ops for Procedure

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

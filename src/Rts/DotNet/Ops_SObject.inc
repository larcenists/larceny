// Operations for SObject
// Mostly, just declares virtual methods that fault with the right excode.

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
        PREDICATE_VIRTUAL_FALSE(isUString, ustringp)

        // ===================
        //   Operations
        // ===================

        OP2(eqp) { return (this == arg2) ? Factory.True : Factory.False; }
        OP1(not) { return (this == Factory.False) ? Factory.True : Factory.False; }
        OP1(nullp) { return (this == Factory.Null) ? Factory.True : Factory.False; }
        OP1(eof_objectp) { return (this == Factory.Eof) ? Factory.True : Factory.False; }
        OP1(unspecifiedp) { return (this == Factory.Unspecified) ? Factory.True : Factory.False; }
        OP1(undefinedp) { return (this == Factory.Undefined) ? Factory.True : Factory.False; }
        
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
        OP2_VIRTUAL_EXN(cell_set_nwb, EX_SETCAR)

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
        OP1_VIRTUAL_EXN(vector_length_vec, EX_VECTOR_LENGTH)
        OP2_VIRTUAL_EXN_PAIR(vector_ref, EX_VECTOR_REF, SVL)
        OP2_VIRTUAL_EXN_PAIR(vector_ref_trusted, EX_VECTOR_REF, SVL)
        OP3_VIRTUAL_EXN_PAIR(vector_set, EX_VECTOR_SET, SVL)
        OP3_VIRTUAL_EXN_PAIR(vector_set_trusted, EX_VECTOR_SET, SVL)
        OP3_VIRTUAL_EXN_PAIR(vector_set_trusted_nwb, EX_VECTOR_SET, SVL)

        // Procedure Operations
        OP1_VIRTUAL_EXN(procedure_length, EX_PROCEDURE_LENGTH)
        OP1_VIRTUAL_EXN(make_procedure, EX_MKVL)
        OP2_VIRTUAL_EXN_PAIR(procedure_ref, EX_PROCEDURE_REF, Procedure)
        OP3_VIRTUAL_EXN_PAIR(procedure_set, EX_PROCEDURE_SET, Procedure)

        // String Operations
        OP2_VIRTUAL_EXN_PAIR(make_string, EX_MKBVL, SFixnum)
        OP1_VIRTUAL_EXN(string_length, EX_STRING_LENGTH)
        OP1_VIRTUAL_EXN(string_length_str, EX_STRING_LENGTH)
        OP2_VIRTUAL_EXN_PAIR(string_ref, EX_STRING_REF, SByteVL)
        OP2_VIRTUAL_EXN_PAIR(string_ref_trusted, EX_STRING_REF, SByteVL)
        OP3_VIRTUAL_EXN_PAIR(string_set, EX_STRING_SET, SByteVL)
        OP3_VIRTUAL_EXN_PAIR(string_set_trusted, EX_STRING_SET, SByteVL)

        // UString Operations
        OP2_VIRTUAL_EXN_PAIR(make_ustring, EX_MKBVL, SFixnum)
        OP1_VIRTUAL_EXN(ustring_length, EX_STRING_LENGTH)
        OP1_VIRTUAL_EXN(ustring_length_str, EX_STRING_LENGTH)
        OP2_VIRTUAL_EXN_PAIR(ustring_ref, EX_STRING_REF, SByteVL)
        OP2_VIRTUAL_EXN_PAIR(ustring_ref_trusted, EX_STRING_REF, SByteVL)
        OP3_VIRTUAL_EXN_PAIR(ustring_set, EX_STRING_SET, SByteVL)
        OP3_VIRTUAL_EXN_PAIR(ustring_set_trusted, EX_STRING_SET, SByteVL)

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
        OP1_VIRTUAL_EXN(fxlognot, EX_LOGNOT)
        OP2_VIRTUAL_EXN_PAIR(fxlogand, EX_LOGAND, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxlogior, EX_LOGIOR, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxlogxor, EX_LOGXOR, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxlsh, EX_LSH, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxrsh, EX_RSHA, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxrsha, EX_RSHA, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(fxrshl, EX_RSHL, SFixnum)

        // Arithmetic Operations
        OP1_VIRTUAL_EXN(real_part, EX_REALPART)
        OP1_VIRTUAL_EXN(imag_part, EX_IMAGPART)

        OP2_VIRTUAL_EXN_PAIR(plus_idx_idx, EX_ADD, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(plus_fix_fix, EX_ADD, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(minus_idx_idx, EX_SUB, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(minus_fix_fix, EX_SUB, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(equal_fix_fix, EX_EQUALP, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(less_fix_fix, EX_LESSP, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(lessequal_fix_fix, EX_LESSEQP, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(greater_fix_fix, EX_GREATERP, SFixnum)
        OP2_VIRTUAL_EXN_PAIR(greaterequal_fix_fix, EX_GREATEREQP, SFixnum)

        // Misc Operations
        OP2(sys_partial_list__vector) {
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

        SPECIALOP1_VIRTUAL_EXN(enable_interrupts, EX_EINTR)
        SPECIALOP1(disable_interrupts) {
            if (Reg.interruptsEnabled) {
                        Reg.interruptsEnabled = false;
                        Reg.Result = Factory.makeFixnum((int)Reg.timer);
            } else {
                Reg.Result = Factory.False;
            }
            Exn.checkSignals();
        }
        SPECIALOP1_VIRTUAL_EXN(syscall, EX_UNSUPPORTED)
        
        SPECIALOP1_VIRTUAL_EXN(zerop, EX_ZEROP)
        SPECIALOP2(eqvp) {
            if (this == arg2) {
                Reg.Result = Factory.True;
            } else {
                this.op_eqvp_not_eq(arg2);
            }
        }
        SPECIALOP2_VIRTUAL(eqvp_not_eq) {
            Reg.Result = Factory.False;
        }
        SPECIALOP2_REVERSED_G(eqvp_not_eq) {
            Reg.Result = Factory.False;
        }
        SPECIALOP2_VIRTUAL_REVERSED_SET(eqvp_not_eq)
        SPECIALOP2_VIRTUAL_REVERSED_CASE(eqvp_not_eq, char, SChar)

        SPECIALOP2_NUMERIC_SET(numeric_equals, econtagion, MS_GENERIC_EQUAL)
        SPECIALOP2_NUMERIC_SET(less_than, pcontagion, MS_GENERIC_LESS)
        SPECIALOP2_NUMERIC_SET(less_or_equal, pcontagion, MS_GENERIC_LESSEQ)
        SPECIALOP2_NUMERIC_SET(greater_than, pcontagion, MS_GENERIC_GREATER)
        SPECIALOP2_NUMERIC_SET(greater_or_equal, pcontagion, MS_GENERIC_GREATEREQ)

        SPECIALOP2_NUMERIC_SET(plus, contagion, MS_GENERIC_ADD)
        SPECIALOP2_NUMERIC_SET(minus, contagion, MS_GENERIC_SUB)
        SPECIALOP2_NUMERIC_SET(multiply, contagion, MS_GENERIC_MUL)
        SPECIALOP2_NUMERIC_SET(divide, contagion, MS_GENERIC_DIV)
        
        SPECIALOP2_NUMERIC_SET_NC(quotient, MS_HEAVY_QUOTIENT)
        SPECIALOP2_NUMERIC_SET_NC(remainder, MS_HEAVY_REMAINDER)
        
        SPECIALOP1_VIRTUAL_EXN(truncate, EX_TRUNC)
        SPECIALOP1_VIRTUAL_EXN(round, EX_ROUND)
        SPECIALOP1_VIRTUAL_EXN(negative, EX_NEG)
        SPECIALOP1_VIRTUAL_MS(exact2inexact, MS_GENERIC_EXACT2INEXACT)
        SPECIALOP1_VIRTUAL_MS(inexact2exact, MS_GENERIC_INEXACT2EXACT)

	// below overridden in Ops_SFixnum (to avoid boxing n) but none need be
	public virtual SObject op_eqp_int32(int n) { return this.op_eqp(SFixnum.makeFixnum(n)); }
	public virtual SObject op_plus_idx_idx_int32(int n) { return this.op_plus_idx_idx(SFixnum.makeFixnum(n)); }
	public virtual SObject op_minus_idx_idx_int32(int n) { return this.op_minus_idx_idx(SFixnum.makeFixnum(n)); }
	public virtual void    op_numeric_equals_int32(int n) { this.op_numeric_equals(SFixnum.makeFixnum(n)); }
	public virtual void op_plus_int32(int n) { this.op_plus(SFixnum.makeFixnum(n)); }
	public virtual void op_minus_int32(int n) { this.op_minus(SFixnum.makeFixnum(n)); }
	public virtual SObject op_fxless_int32(int n) { return this.op_fxless(SFixnum.makeFixnum(n)); }
	public virtual SObject op_greaterequal_fix_fix_int32(int n) { return this.op_greaterequal_fix_fix(SFixnum.makeFixnum(n)); }
	public virtual SObject op_less_fix_fix_int32(int n) { return this.op_less_fix_fix(SFixnum.makeFixnum(n)); }
	public virtual SObject op_vector_ref_trusted_int32(int n) { return this.op_vector_ref_trusted(SFixnum.makeFixnum(n)); }

 	// below overridden in Ops_SFixnum (to avoid boxing n) but none need be
 	public virtual bool isEqpInt32(int n) { return (this.op_eqp(SFixnum.makeFixnum(n)) == Factory.True); }
 	public virtual bool isFxLessInt32(int n) { return (this.op_fxless(SFixnum.makeFixnum(n)) == Factory.True); }
 	public virtual bool isNumericEqualFixFixInt32(int n) { this.op_numeric_equals(SFixnum.makeFixnum(n)); return (Reg.Result == Factory.True); }
 	public virtual bool isLessFixFixInt32(int n) { this.op_less_than(SFixnum.makeFixnum(n)); return (Reg.Result == Factory.True); }

	// first *must* be overriden in Ops_SChar
	public virtual bool isCharEqualsInt32(int val) { return false; }
        public virtual SObject op_charequals_int32(int val) {
	    return this.op_char_equals( Factory.makeChar(val) );
	} 

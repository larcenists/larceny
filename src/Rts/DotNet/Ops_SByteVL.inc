// Ops for SByteVL

        PREDICATE_OVERRIDE_EXPR(isNumber, numberp, 
                                this.isBignum() || this.isFlonum() || this.isCompnum())
        CLR_PREDICATE_OVERRIDE_EXPR(isExact, this.isBignum())
        CLR_PREDICATE_OVERRIDE_EXPR(isInexact, this.isFlonum() || this.isCompnum())
        OP1_OVERRIDE(exactp) {
            if (this.isNumber()) {
                return this.isBignum() ? Factory.True : Factory.False;
            } else {
                Exn.fault(Constants.EX_EXACTP, null, this);
                return Factory.Impossible;
            }
        }
        OP1_OVERRIDE(inexactp) {
            if (this.isNumber()) {
                return (this.isFlonum() || this.isCompnum()) ? Factory.True : Factory.False;
            } else {
                Exn.fault(Constants.EX_INEXACTP, null, this);
                return Factory.Impossible;
            }
        }
        PREDICATE_OVERRIDE_EXPR(isBignum, bignump, this.tag == Tags.BignumTag)
        PREDICATE_OVERRIDE_EXPR(isFlonum, flonump, this.tag == Tags.FlonumTag)
        PREDICATE_OVERRIDE_EXPR(isCompnum, compnump, this.tag == Tags.CompnumTag)
        PREDICATE_OVERRIDE_EXPR(isComplex, complexp,
                                this.isBignum() || this.isFlonum() || this.isCompnum())
        PREDICATE_OVERRIDE_EXPR(isReal, realp, this.isBignum() || this.isFlonum())
        PREDICATE_OVERRIDE_EXPR(isRational, rationalp, this.isBignum() || this.isFlonum())
        PREDICATE_OVERRIDE_EXPR(isInteger, integerp, 
                                this.isBignum() 
                                || (this.isFlonum() && this.isIntegralFlonum()))
        PREDICATE_OVERRIDE_TRUE(isByteVectorLike, bytevector_likep)
        PREDICATE_OVERRIDE_EXPR(isByteVector, bytevectorp, this.tag == Tags.ByteVectorTag)
        PREDICATE_OVERRIDE_EXPR(isString, stringp, this.tag == Tags.StringTag)
        // ----------------------

        OP1_OVERRIDE(string_length) {
            check_typetag(Tags.StringTag, this, Constants.EX_STRING_LENGTH);
            return Factory.makeNumber (this.elements.Length);
        }
        OP1_OVERRIDE(string_length_str) {
            // check_typetag(Tags.StringTag, this, Constants.EX_STRING_LENGTH);
            return Factory.makeNumber (this.elements.Length);
        }

        OP2_CHAIN_CHECK_TAG(string_ref, Tags.StringTag, EX_STRING_REF)
        OP3_CHAIN_CHECK_TAG(string_set, Tags.StringTag, EX_STRING_SET)
        OP2_CHAIN(string_ref_trusted)
        OP3_CHAIN(string_set_trusted)

        OP1_OVERRIDE_CHECK_TAG(bytevector_length, Tags.ByteVectorTag, EX_BYTEVECTOR_LENGTH) {
            return Factory.makeNumber (this.elements.Length);
        }
        OP2_CHAIN_CHECK_TAG(bytevector_ref, Tags.ByteVectorTag, EX_BYTEVECTOR_REF)
        OP3_CHAIN_CHECK_TAG(bytevector_set, Tags.ByteVectorTag, EX_BYTEVECTOR_SET)
        OP2_CHAIN_CHECK_TAG(bytevector_equal, Tags.ByteVectorTag, EX_UNSUPPORTED)
        OP2_CHAIN_CHECK_TAG(bytevector_fill, Tags.ByteVectorTag, EX_BVFILL)

        OP1_OVERRIDE(bytevector_like_length) {
            return Factory.makeNumber (this.elements.Length);
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

        // Special Operation

        SPECIALOP1_OVERRIDE(zerop) {
            if (this.tag == Tags.BignumTag) {
                Reg.Result = (Number.getBignumLength(this) == 0) ? Factory.True : Factory.False;
            } else if (this.tag == Tags.FlonumTag) {
                Reg.Result = (this.unsafeAsDouble(0) == 0.0) ? Factory.True : Factory.False;
            } else if (this.tag == Tags.CompnumTag) {
                Reg.Result = (this.unsafeAsDouble(0) == 0.0 &&
                              this.unsafeAsDouble(1) == 0.0) ? Factory.True : Factory.False;
            } else {
                base.op_zerop();
            }
        }

        SPECIALOP2_CHAIN_SByteVL(eqvp_not_eq)
        SPECIALOP2_CHAIN_SByteVL(numeric_equals)
        SPECIALOP2_CHAIN_SByteVL(less_than)
        SPECIALOP2_CHAIN_SByteVL(less_or_equal)
        SPECIALOP2_CHAIN_SByteVL(greater_than)
        SPECIALOP2_CHAIN_SByteVL(greater_or_equal)

        SPECIALOP2_CHAIN_SByteVL(plus)
        SPECIALOP2_CHAIN_SByteVL(minus)
        SPECIALOP2_CHAIN_SByteVL(multiply)
        SPECIALOP2_CHAIN_SByteVL(divide)
        SPECIALOP2_CHAIN_SByteVL(quotient)
        SPECIALOP2_CHAIN_SByteVL(remainder)

        // Bignums
        SPECIALOP2_OVERRIDE_REV_MS(eqvp_not_eq, bignum, SByteVL, BignumTag, 
                                   MS_BIGNUM_EQUAL)
        SPECIALOP2_OVERRIDE_REV_MS(numeric_equals, bignum, SByteVL, BignumTag, 
                                   MS_BIGNUM_EQUAL)
        SPECIALOP2_OVERRIDE_REV_MS(less_than, bignum, SByteVL, BignumTag, 
                                   MS_BIGNUM_LESS)
        SPECIALOP2_OVERRIDE_REV_MS(less_or_equal, bignum, SByteVL, BignumTag, 
                                   MS_BIGNUM_LESSEQ)
        SPECIALOP2_OVERRIDE_REV_MS(greater_than, bignum, SByteVL, BignumTag, 
                                   MS_BIGNUM_GREATER)
        SPECIALOP2_OVERRIDE_REV_MS(greater_or_equal, bignum, SByteVL, BignumTag, 
                                   MS_BIGNUM_GREATEREQ)

        SPECIALOP2_OVERRIDE_REV_MS(plus, bignum, SByteVL, BignumTag, MS_BIGNUM_ADD)
        SPECIALOP2_OVERRIDE_REV_MS(minus, bignum, SByteVL, BignumTag, MS_BIGNUM_SUB)
        SPECIALOP2_OVERRIDE_REV_MS(multiply, bignum, SByteVL, BignumTag, MS_BIGNUM_MUL)
        SPECIALOP2_OVERRIDE_REV_MS(divide, bignum, SByteVL, BignumTag, MS_BIGNUM_DIV)

        // Flonums
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(eqvp_not_eq, flonum, SByteVL, FlonumTag) 
        {
            Reg.Result = (arg1.unsafeAsDouble(0) == this.unsafeAsDouble(0)) ? Factory.True : Factory.False;
        }
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(numeric_equals, flonum, SByteVL, FlonumTag) {
            Reg.Result = (arg1.unsafeAsDouble(0) == this.unsafeAsDouble(0)) ? Factory.True : Factory.False;
        }
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(less_than, flonum, SByteVL, FlonumTag) {
            Reg.Result = (arg1.unsafeAsDouble(0) < this.unsafeAsDouble(0))  ? Factory.True : Factory.False;
        }
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(less_or_equal, flonum, SByteVL, FlonumTag) {
            Reg.Result = (arg1.unsafeAsDouble(0) <= this.unsafeAsDouble(0))  ? Factory.True : Factory.False;
        }
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(greater_than, flonum, SByteVL, FlonumTag) {
            Reg.Result = (arg1.unsafeAsDouble(0) > this.unsafeAsDouble(0)) ? Factory.True : Factory.False;
        }
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(greater_or_equal, flonum, SByteVL, FlonumTag) {
            Reg.Result = (arg1.unsafeAsDouble(0) >= this.unsafeAsDouble(0)) ? Factory.True : Factory.False;
        }

        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(plus, flonum, SByteVL, FlonumTag) {
            Reg.Result = Factory.makeFlonum (arg1.unsafeAsDouble(0) + this.unsafeAsDouble(0));
        }
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(minus, flonum, SByteVL, FlonumTag) {
            Reg.Result = Factory.makeFlonum (arg1.unsafeAsDouble(0) - this.unsafeAsDouble(0));
        }
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(multiply, flonum, SByteVL, FlonumTag) {
            Reg.Result = Factory.makeFlonum (arg1.unsafeAsDouble(0) * this.unsafeAsDouble(0));
        }
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(divide, flonum, SByteVL, FlonumTag) {
            Reg.Result = Factory.makeFlonum (arg1.unsafeAsDouble(0) / this.unsafeAsDouble(0));
        }

        // Compnums
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(eqvp_not_eq, compnum, SByteVL, CompnumTag) {
            Reg.Result = (arg1.unsafeAsDouble(0) == this.unsafeAsDouble(0)
                                      &&
                          arg1.unsafeAsDouble(1) == this.unsafeAsDouble(1)) ? Factory.True : Factory.False;
        }
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(numeric_equals, compnum, SByteVL, CompnumTag) {
            Reg.Result = (arg1.unsafeAsDouble(0) == this.unsafeAsDouble(0)
                                      &&
                          arg1.unsafeAsDouble(1) == this.unsafeAsDouble(1)) ? Factory.True : Factory.False;
        }

        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(plus, compnum, SByteVL, CompnumTag) {
            Reg.Result = Factory.makeCompnum
                (arg1.unsafeAsDouble(0) + this.unsafeAsDouble(0),
                 arg1.unsafeAsDouble(1) + this.unsafeAsDouble(1));
        }
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(minus, compnum, SByteVL, CompnumTag) {
            Reg.Result = Factory.makeCompnum
                (arg1.unsafeAsDouble(0) - this.unsafeAsDouble(0),
                 arg1.unsafeAsDouble(1) - this.unsafeAsDouble(1));
        }
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(multiply, compnum, SByteVL, CompnumTag) {
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
        SPECIALOP2_OVERRIDE_REVERSED_HANDLE(divide, compnum, SByteVL, CompnumTag) {
            double ar = arg1.unsafeAsDouble(0), ai = arg1.unsafeAsDouble(1);
            double br = this.unsafeAsDouble(0), bi = this.unsafeAsDouble(1);
            double denom = br * br + bi * bi;
            Reg.Result = Factory.makeCompnum
                ((ar * br + ai * bi) / denom,
                 (ai * br - ar * bi) / denom);
        }
    
        SPECIALOP1_OVERRIDE(truncate) {
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
        SPECIALOP1_OVERRIDE(round) {
            if (this.tag == Tags.BignumTag) {
                Reg.Result = this;
            } else if (this.tag == Tags.FlonumTag) {
                double d = this.unsafeAsDouble(0);
                Reg.Result = Factory.makeFlonum(System.Math.Round(d));
            } else {
                base.op_round();
            }
        }
        SPECIALOP1_OVERRIDE(negative) {
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
        SPECIALOP1_OVERRIDE(exact2inexact) {
            if (this.tag == Tags.FlonumTag) {
                Reg.Result = this;
            } else if (this.tag == Tags.CompnumTag) {
                Reg.Result = this;
            } else {
                base.op_exact2inexact();
            }
        }
        SPECIALOP1_OVERRIDE(inexact2exact) {
            if (this.tag == Tags.BignumTag) {
                Reg.Result = this;
            } else {
                base.op_inexact2exact();
            }
        }
        OP1_OVERRIDE(real_part) {
            if (this.tag == Tags.BignumTag) {
                return this;
            } else if (this.tag == Tags.FlonumTag) {
                return this;
            } else if (this.tag == Tags.CompnumTag) {
                return Factory.makeFlonum(this.unsafeAsDouble(0));
            } else {
                return base.op_real_part();
            }
        }
        OP1_OVERRIDE(imag_part) {
            if (this.tag == Tags.BignumTag) {
                return Factory.makeFixnum(0);
            } else if (this.tag == Tags.FlonumTag) {
                return Factory.makeFixnum(0);
            } else if (this.tag == Tags.CompnumTag) {
                return Factory.makeFlonum(this.unsafeAsDouble(1));
            } else {
                return base.op_imag_part();
            }
        }

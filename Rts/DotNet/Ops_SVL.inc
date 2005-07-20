// Ops for SVL

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
                return (this.isRatnum() || this.isRectnum()) ? Factory.True : Factory.False;
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
            return Factory.makeNumber (elements.Length);
        }
        OP2_CHAIN(vector_like_ref)
        OP3_CHAIN(vector_like_set)

        OP1_OVERRIDE(vector_length) {
            check_typetag(Tags.VectorTag, Constants.EX_VECTOR_LENGTH);
            return Factory.makeNumber (elements.Length);
        }
        OP1_OVERRIDE(vector_length_vec) {
            return Factory.makeNumber (elements.Length);
        }

        OP2_CHAIN_CHECK_TAG(vector_ref, Tags.VectorTag, EX_VECTOR_REF)
        OP2_CHAIN(vector_ref_trusted)
        OP3_CHAIN_CHECK_TAG(vector_set, Tags.VectorTag, EX_VECTOR_SET)
        OP3_CHAIN(vector_set_trusted)

        // Special Operations

        SPECIALOP1_OVERRIDE(zerop) {
            if (this.tag == Tags.RectnumTag) {
                Reg.Result = Factory.False;
            } else if (this.tag == Tags.RatnumTag) {
                Reg.Result = Factory.False;
            } else {
                base.op_zerop();
            }
        }

        SPECIALOP2_CHAIN_SVL(eqvp_not_eq)
        SPECIALOP2_CHAIN_SVL(numeric_equals)
        SPECIALOP2_CHAIN_SVL(less_than)
        SPECIALOP2_CHAIN_SVL(less_or_equal)
        SPECIALOP2_CHAIN_SVL(greater_than)
        SPECIALOP2_CHAIN_SVL(greater_or_equal)

        SPECIALOP2_CHAIN_SVL(plus)
        SPECIALOP2_CHAIN_SVL(minus)
        SPECIALOP2_CHAIN_SVL(multiply)
        SPECIALOP2_CHAIN_SVL(divide)
        SPECIALOP2_CHAIN_SVL(quotient)
        SPECIALOP2_CHAIN_SVL(remainder)

        // Ratnums
        SPECIALOP2_OVERRIDE_REV_MS(eqvp_not_eq, ratnum, SVL, RatnumTag, MS_RATNUM_EQUAL)
        SPECIALOP2_OVERRIDE_REV_MS(numeric_equals, ratnum, SVL, RatnumTag, MS_RATNUM_EQUAL)
        SPECIALOP2_OVERRIDE_REV_MS(less_than, ratnum, SVL, RatnumTag, MS_RATNUM_LESS)
        SPECIALOP2_OVERRIDE_REV_MS(less_or_equal, ratnum, SVL, RatnumTag, MS_RATNUM_LESSEQ)
        SPECIALOP2_OVERRIDE_REV_MS(greater_than, ratnum, SVL, RatnumTag, MS_RATNUM_GREATER)
        SPECIALOP2_OVERRIDE_REV_MS(greater_or_equal, ratnum, SVL, RatnumTag, 
                                   MS_RATNUM_GREATEREQ)

        SPECIALOP2_OVERRIDE_REV_MS(plus, ratnum, SVL, RatnumTag, MS_RATNUM_ADD)
        SPECIALOP2_OVERRIDE_REV_MS(minus, ratnum, SVL, RatnumTag, MS_RATNUM_SUB)
        SPECIALOP2_OVERRIDE_REV_MS(multiply, ratnum, SVL, RatnumTag, MS_RATNUM_MUL)
        SPECIALOP2_OVERRIDE_REV_MS(divide, ratnum, SVL, RatnumTag, MS_RATNUM_DIV)

        // Rectnums
        SPECIALOP2_OVERRIDE_REV_MS(eqvp_not_eq, rectnum, SVL, RectnumTag, MS_RECTNUM_EQUAL)
        SPECIALOP2_OVERRIDE_REV_MS(numeric_equals, rectnum, SVL, RatnumTag, MS_RECTNUM_EQUAL)

        SPECIALOP2_OVERRIDE_REV_MS(plus, rectnum, SVL, RatnumTag, MS_RECTNUM_ADD)
        SPECIALOP2_OVERRIDE_REV_MS(minus, rectnum, SVL, RatnumTag, MS_RECTNUM_SUB)
        SPECIALOP2_OVERRIDE_REV_MS(multiply, rectnum, SVL, RatnumTag, MS_RECTNUM_MUL)
        SPECIALOP2_OVERRIDE_REV_MS(divide, rectnum, SVL, RatnumTag, MS_RECTNUM_DIV)

        SPECIALOP1_OVERRIDE(truncate) {
            if (this.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RATNUM_TRUNCATE, this);
            } else {
                base.op_truncate();
            }
        }
        SPECIALOP1_OVERRIDE(round) {
            if (this.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RATNUM_ROUND, this);
            } else {
                base.op_truncate();
            }
        }
        SPECIALOP1_OVERRIDE(negative) {
            if (this.tag == Tags.RatnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RATNUM_NEGATE, this);
            } else if (this.tag == Tags.RectnumTag) {
                Call.callMillicodeSupport1(Constants.MS_RECTNUM_NEGATE, this);
            } else {
                base.op_truncate();
            }
        }
        SPECIALOP1_OVERRIDE(inexact2exact) {
            if (this.tag == Tags.RatnumTag) {
                Reg.Result = this;
            } else if (this.tag == Tags.RectnumTag) {
                Reg.Result = this;
            } else {
                base.op_truncate();
            }
        }

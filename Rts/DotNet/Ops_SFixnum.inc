// Ops for SFixnum

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
        OP2_OVERRIDE_REVERSED(vector_ref_trusted, SVL) {
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
        OP3_OVERRIDE_REVERSED(vector_set_trusted, SVL) {
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
        OP2_OVERRIDE_REVERSED(string_ref, SByteVL) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            if (0 <= index && index < bytes.Length) {
                return Factory.makeChar(bytes[index]);
            } else {
                Exn.fault(Constants.EX_STRING_REF, null, arg1, this);
                return Factory.Impossible;
            }
        }
        OP2_OVERRIDE_REVERSED(string_ref_trusted, SByteVL) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            return Factory.makeChar(bytes[index]);
        }
        OP3_OVERRIDE_REVERSED(string_set, SByteVL) {
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
        OP3_OVERRIDE_REVERSED(string_set_trusted, SByteVL) {
            byte[] bytes = arg1.elements;
            int index = this.value;
            bytes[index] = (byte) ((SChar)arg3).val;
            return Factory.Unspecified;
        }

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

        OP2_CHAIN(plus_idx_idx)
        OP2_CHAIN(minus_idx_idx)
        OP2_CHAIN(equal_fix_fix)
        OP2_CHAIN(less_fix_fix)
        OP2_CHAIN(lessequal_fix_fix)
        OP2_CHAIN(greater_fix_fix)
        OP2_CHAIN(greaterequal_fix_fix)

        OP2_OVERRIDE_REVERSED(plus_idx_idx, SFixnum) {
            return Factory.wrap(arg1.value + this.value);
        }
        OP2_OVERRIDE_REVERSED(minus_idx_idx, SFixnum) {
            return Factory.wrap(arg1.value - this.value);
        }
        OP2_OVERRIDE_REVERSED(equal_fix_fix, SFixnum) {
            return Factory.wrap(arg1.value == this.value);
        }
        OP2_OVERRIDE_REVERSED(less_fix_fix, SFixnum) {
            return Factory.wrap(arg1.value < this.value);
        }
        OP2_OVERRIDE_REVERSED(lessequal_fix_fix, SFixnum) {
            return Factory.wrap(arg1.value <= this.value);
        }
        OP2_OVERRIDE_REVERSED(greater_fix_fix, SFixnum) {
            return Factory.wrap(arg1.value > this.value);
        }
        OP2_OVERRIDE_REVERSED(greaterequal_fix_fix, SFixnum) {
            return Factory.wrap(arg1.value >= this.value);
        }

        // Special Operations

        SPECIALOP1_OVERRIDE(enable_interrupts) {
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
        SPECIALOP1_OVERRIDE(syscall) {
            // subtract one 'cuz the first arg is just the value
            // to which we want to dispatch.
            int num_args = this.value - 1;
            Sys num_syscall = (Sys) ((SFixnum)Reg.register1).intValue();
            Syscall.dispatch(num_args, num_syscall);
        }
        SPECIALOP1_OVERRIDE(zerop) {
            Reg.Result = Factory.wrap(this.value == 0);
        }

        SPECIALOP2_CHAIN_FIXNUM(eqvp_not_eq)
        SPECIALOP2_CHAIN_FIXNUM(numeric_equals)
        SPECIALOP2_CHAIN_FIXNUM(less_than)
        SPECIALOP2_CHAIN_FIXNUM(less_or_equal)
        SPECIALOP2_CHAIN_FIXNUM(greater_than)
        SPECIALOP2_CHAIN_FIXNUM(greater_or_equal)

        SPECIALOP2_CHAIN_FIXNUM(plus)
        SPECIALOP2_CHAIN_FIXNUM(minus)
        SPECIALOP2_CHAIN_FIXNUM(multiply)
        SPECIALOP2_CHAIN_FIXNUM(divide)
        SPECIALOP2_CHAIN_FIXNUM(quotient)
        SPECIALOP2_CHAIN_FIXNUM(remainder)

        SPECIALOP2_OVERRIDE_REVERSED_CASE(eqvp_not_eq, fixnum, SFixnum) {
            Reg.Result = Factory.wrap(arg1.value == this.value);
        }
        SPECIALOP2_OVERRIDE_REVERSED_CASE(numeric_equals, fixnum, SFixnum) {
            Reg.Result = Factory.wrap(arg1.value == this.value);
        }
        SPECIALOP2_OVERRIDE_REVERSED_CASE(less_than, fixnum, SFixnum) {
            Reg.Result = Factory.wrap(arg1.value < this.value);
        }
        SPECIALOP2_OVERRIDE_REVERSED_CASE(less_or_equal, fixnum, SFixnum) {
            Reg.Result = Factory.wrap(arg1.value <= this.value);
        }
        SPECIALOP2_OVERRIDE_REVERSED_CASE(greater_than, fixnum, SFixnum) {
            Reg.Result = Factory.wrap(arg1.value > this.value);
        }
        SPECIALOP2_OVERRIDE_REVERSED_CASE(greater_or_equal, fixnum, SFixnum) {
            Reg.Result = Factory.wrap(arg1.value >= this.value);
        }

        SPECIALOP2_OVERRIDE_REVERSED_CASE(plus, fixnum, SFixnum) {
            Reg.Result = Factory.wrap(arg1.value + this.value);
        }
        SPECIALOP2_OVERRIDE_REVERSED_CASE(minus, fixnum, SFixnum) {
            Reg.Result = Factory.wrap(arg1.value - this.value);
        }
        SPECIALOP2_OVERRIDE_REVERSED_CASE(multiply, fixnum, SFixnum) {
            long a = arg1.value;
            long b = this.value;
            Reg.Result = Factory.wrap(a * b);
        }
        SPECIALOP2_OVERRIDE_REVERSED_CASE(divide, fixnum, SFixnum) {
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
        SPECIALOP2_OVERRIDE_REVERSED_CASE(quotient, fixnum, SFixnum) {
            int a = arg1.value, b = this.value;
            if (b == 0) {
                Exn.fault(Constants.EX_QUOTIENT, null, arg1, this);
            }
            Reg.Result = Factory.wrap(a / b);
        }
        SPECIALOP2_OVERRIDE_REVERSED_CASE(quotient, bignum, SByteVL) {
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

        SPECIALOP2_OVERRIDE_REVERSED_CASE(remainder, fixnum, SFixnum) {
            int a = arg1.value, b = this.value;
            if (b == 0) {
                Exn.fault(Constants.EX_REMAINDER, null, arg1, this);
            }
            Reg.Result = Factory.wrap(a % b);
        }
        SPECIALOP2_OVERRIDE_REVERSED_CASE(remainder, bignum, SByteVL) {
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

        SPECIALOP1_OVERRIDE(truncate) {
            Reg.Result = this;
        }
        SPECIALOP1_OVERRIDE(round) {
            Reg.Result = this;
        }
        SPECIALOP1_OVERRIDE(negative) {
            Reg.Result = Factory.wrap(-this.value);
        }
        SPECIALOP1_OVERRIDE(exact2inexact) {
            Reg.Result = Factory.makeFlonum((double)this.value);
        }
        SPECIALOP1_OVERRIDE(inexact2exact) {
            Reg.Result = this;
        }

    
/* Predicates
 * PREDICATE_VIRTUAL_FALSE defines a virtual predicate pair returning false/#f
 * PREDICATE_OVERRIDE_TRUE overrides a virtual predicate to return true/#t
 * PREDICATE_OVERRIDE_EXPR overrides a virtual predicate to return result of body.
 */
#define OP1_PREDICATE_VIRTUAL_FALSE(method) \
    public virtual SObject op_##method() {  return Factory.False; }
#define OP1_PREDICATE_OVERRIDE_TRUE(method) \
    public override SObject op_##method() { return Factory.True; }
#define OP1_PREDICATE_OVERRIDE_FALSE(method) \
    public override SObject op_##method() { return Factory.False; }
#define OP1_PREDICATE_OVERRIDE_EXPR(method, expr) \
    public override SObject op_##method() { return Factory.makeBoolean(expr); }

#define CLR_PREDICATE_VIRTUAL_FALSE(method) \
    public virtual bool method() { return false; }
#define CLR_PREDICATE_OVERRIDE_TRUE(method) \
    public override bool method() { return true; }
#define CLR_PREDICATE_OVERRIDE_FALSE(method) \
    public override bool method() { return false; }
#define CLR_PREDICATE_OVERRIDE_EXPR(method, expr) \
    public override bool method() { return (expr); }

#define PREDICATE_VIRTUAL_FALSE(cmethod, smethod) \
    OP1_PREDICATE_VIRTUAL_FALSE(smethod) \
    CLR_PREDICATE_VIRTUAL_FALSE(cmethod)
#define PREDICATE_OVERRIDE_TRUE(cmethod, smethod) \
    OP1_PREDICATE_OVERRIDE_TRUE(smethod) \
    CLR_PREDICATE_OVERRIDE_TRUE(cmethod)
#define PREDICATE_OVERRIDE_FALSE(cmethod, smethod) \
    OP1_PREDICATE_OVERRIDE_FALSE(smethod) \
    CLR_PREDICATE_OVERRIDE_FALSE(cmethod)
#define PREDICATE_OVERRIDE_EXPR(cmethod, smethod, expr) \
    OP1_PREDICATE_OVERRIDE_EXPR(smethod, expr) \
    CLR_PREDICATE_OVERRIDE_EXPR(cmethod, expr)

/* OP1_VIRTUAL_EXN defines an operation of one argument which
 *   by default throws the given exception
 */
#define OP1_VIRTUAL_EXN(method, excode) \
    public virtual SObject op_##method() { \
        Exn.fault(Constants.excode, null, this); \
        return Factory.Impossible; \
    }

/* OP1(method) declares an operation of one argument
 */
#define OP1(method) \
    public SObject op_##method()
#define OP1_OVERRIDE(method) \
    public override SObject op_##method() 

#define OP1_OVERRIDE_CHECK_TAG(method, tag, excode) \
    public override SObject op_##method() {\
        check_typetag(tag, Constants.excode); \
        return implementation_##method(); \
    } \
    private SObject implementation_##method()

/* OP2_VIRTUAL_EXN defines an operation of two arguments
 *   that by default throws the given exception
 */
#define OP2_VIRTUAL_EXN(method, excode) \
    public virtual SObject op_##method(SObject arg2) { \
        Exn.fault(Constants.excode, null, this, arg2); \
        return Factory.Impossible; \
    }
#define OP2_VIRTUAL_EXN_REVERSED(method, excode, type) \
    public/**/ virtual SObject op_reversed_##method(type arg1) {\
        Exn.fault(Constants.excode, null, arg1, this); \
        return Factory.Impossible; \
    }
#define OP2_VIRTUAL_EXN_PAIR(method, excode, type) \
    OP2_VIRTUAL_EXN(method, excode) \
    OP2_VIRTUAL_EXN_REVERSED(method, excode, type)

#define OP2(method) \
    public SObject op_##method(SObject arg2)

/* OP2_CHAIN overrides the operation to chain to 
 *   the reversed form of itself on arg2
 */
#define OP2_CHAIN(method) \
    public override SObject op_##method(SObject arg2) { \
        return arg2.op_reversed_##method(this); \
    }
#define OP2_CHAIN_CHECK_TAG(method, tag, excode) \
    public override SObject op_##method(SObject arg2) {\
        check_typetag(tag, arg2, Constants.excode); \
        return arg2.op_reversed_##method(this); \
    }
#define OP2_OVERRIDE(method) \
    public override SObject op_##method(SObject arg2)
#define OP2_OVERRIDE_REVERSED(method, type) \
    public/**/ override SObject op_reversed_##method(type arg1)


#define OP3_VIRTUAL_EXN(method, excode) \
    public virtual SObject op_##method(SObject arg2, SObject arg3) {\
        Exn.fault(Constants.excode, null, this, arg2, arg3); \
        return Factory.Impossible; \
    }
#define OP3_VIRTUAL_EXN_REVERSED(method, excode, type) \
    public/**/ virtual SObject op_reversed_##method(type arg1, SObject arg3) { \
        Exn.fault(Constants.excode, null, arg1, this, arg3); \
        return Factory.Impossible; \
    }
#define OP3_VIRTUAL_EXN_PAIR(method, excode, type) \
    OP3_VIRTUAL_EXN(method, excode) \
    OP3_VIRTUAL_EXN_REVERSED(method, excode, type)

#define OP3_CHAIN(method) \
    public override SObject op_##method(SObject arg2, SObject arg3) {\
        return arg2.op_reversed_##method(this, arg3); \
    }
#define OP3_CHAIN_CHECK_TAG(method, tag, excode) \
    public override SObject op_##method(SObject arg2, SObject arg3) {\
        check_typetag(tag, arg2, arg3, Constants.excode); \
        return arg2.op_reversed_##method(this, arg3); \
    }
#define OP3_OVERRIDE(method) \
    public override SObject op_##method(SObject arg2, SObject arg3)
#define OP3_OVERRIDE_REVERSED(method, type) \
    public/**/ override SObject op_reversed_##method(type arg1, SObject arg3)

/* Special Operations */

#define SPECIALOP1_VIRTUAL_EXN(method, excode) \
    public virtual void op_##method() { \
        Exn.fault(Constants.excode, null, this); \
    }
#define SPECIALOP1_VIRTUAL_MS(method, mscode) \
    public virtual void op_##method() { \
        Call.callMillicodeSupport1(Constants.mscode, this); \
    }
#define SPECIALOP1(method) \
    public void op_##method()
#define SPECIALOP1_OVERRIDE(method) \
    public override void op_##method()

#define SPECIALOP2(method) \
    public void op_##method(SObject arg2)

#define SPECIALOP2_VIRTUAL(method) \
    public virtual void op_##method(SObject arg2)


#define SPECIALOP2_NUMERIC_DEFAULT(method, contagion_method, generic_code) \
    public virtual void op_##method(SObject arg2) { \
        Procedure generic = Call.getSupportProcedure(Constants.generic_code); \
        Call.contagion_method(this, arg2, generic); \
    }
#define SPECIALOP2_REVERSED_G(method) \
    public/**/ virtual void op_reversed_generic_##method(SObject arg1)

#define SPECIALOP2_REVERSED_GENERIC(method, contagion_method, generic) \
    public/**/ void op_reversed_generic_##method(SObject arg1) { \
        Procedure generic = Call.getSupportProcedure(Constants.generic); \
        Call.contagion_method(arg1, this, generic); \
    }
#define SPECIALOP2_VIRTUAL_REVERSED_CASE(method, case, type) \
    public/**/ virtual void op_reversed_##case##_##method(type arg1) { \
        this.op_reversed_generic_##method(arg1); \
    }

#define SPECIALOP2_NUMERIC_SET(method, contagion_method, generic_code) \
    SPECIALOP2_NUMERIC_DEFAULT(method, contagion_method, generic_code) \
    SPECIALOP2_REVERSED_GENERIC(method, contagion_method, generic_code) \
    SPECIALOP2_VIRTUAL_REVERSED_SET(method)

#define SPECIALOP2_VIRTUAL_REVERSED_SET(method) \
    SPECIALOP2_VIRTUAL_REVERSED_CASE(method, fixnum, SFixnum) \
    SPECIALOP2_VIRTUAL_REVERSED_CASE(method, bignum, SByteVL) \
    SPECIALOP2_VIRTUAL_REVERSED_CASE(method, flonum, SByteVL) \
    SPECIALOP2_VIRTUAL_REVERSED_CASE(method, compnum, SByteVL) \
    SPECIALOP2_VIRTUAL_REVERSED_CASE(method, ratnum, SVL) \
    SPECIALOP2_VIRTUAL_REVERSED_CASE(method, rectnum, SVL)

#define SPECIALOP2_OVERRIDE_REVERSED_CASE(method, case, type) \
    public/**/ override void op_reversed_##case##_##method(type arg1)

#define SPECIALOP2_CHAIN_FIXNUM(method) \
    public override void op_##method(SObject arg2) { \
        arg2.op_reversed_fixnum_##method(this); \
    }
#define SPECIALOP2_CHAIN_SByteVL(method) \
    public override void op_##method(SObject arg2) { \
        if (this.tag == Tags.BignumTag) { \
            arg2.op_reversed_bignum_##method(this); \
        } else if (this.tag == Tags.FlonumTag) { \
            arg2.op_reversed_flonum_##method(this); \
        } else if (this.tag == Tags.CompnumTag) { \
            arg2.op_reversed_compnum_##method(this); \
        } else { \
            base.op_##method(this); \
        } \
    }
#define SPECIALOP2_CHAIN_SVL(method) \
    public override void op_##method(SObject arg2) { \
       if (this.tag == Tags.RatnumTag) { \
           arg2.op_reversed_ratnum_##method(this); \
       } else if (this.tag == Tags.RectnumTag) { \
           arg2.op_reversed_rectnum_##method(this); \
       } else { \
           base.op_##method(this); \
       } \
    }

#define SPECIALOP2_OVERRIDE_REVERSED_HANDLE(method, case, type, ttag) \
    public/**/ override void op_reversed_##case##_##method(type arg1) { \
        if (this.tag == Tags.ttag) { \
            this.op_reversed_##case##_##ttag##_##method(arg1); \
        } else { \
            base.op_reversed_##case##_##method(arg1); \
        } \
    } \
    private void op_reversed_##case##_##ttag##_##method(type arg1) 

#define SPECIALOP2_OVERRIDE_REV_MS(method, case, type, ttag, mscode) \
    public/**/ override void op_reversed_##case##_##method(type arg1) { \
        if (this.tag == Tags.ttag) { \
            Call.callMillicodeSupport2(Constants.mscode, arg1, this); \
        } else { \
            base.op_reversed_##case##_##method(arg1); \
        } \
    }

/* ---- */
// NC = No Contagion
#define SPECIALOP2_NUMERIC_SET_NC(method, generic_code) \
    SPECIALOP2_NUMERIC_DEFAULT_NC(method, generic_code) \
    SPECIALOP2_REVERSED_GENERIC_NC(method, generic_code) \
    SPECIALOP2_VIRTUAL_REVERSED_SET(method)

#define SPECIALOP2_NUMERIC_DEFAULT_NC(method, generic) \
    public virtual void op_##method(SObject arg2) { \
        Call.callMillicodeSupport2(Constants.generic, this, arg2); \
    }

#define SPECIALOP2_REVERSED_GENERIC_NC(method, generic) \
    public/**/ void op_reversed_generic_##method(SObject arg1) { \
        Call.callMillicodeSupport2(Constants.generic, arg1, this); \
    }

#define SPECIALOP2_REVERSED_G(method) \
    public/**/ virtual void op_reversed_generic_##method(SObject arg1)

#define PREDSF(method) \
        public virtual SObject method() { return Factory.False; }
#define PREDF(method) \
        public virtual bool method() { return false; }

#define PRED(predicate, operation) \
        PREDSF(operation) \
        PREDF(predicate)

#define VEX1(method, excode) \
        public virtual SObject method() { \
            Exn.fault(Constants.excode, null, this); \
            return Factory.Impossible; \
        }
#define VEX2(method, excode) \
        public virtual SObject method(SObject arg2) { \
            Exn.fault(Constants.excode, null, this, arg2); \
            return Factory.Impossible; \
        }
#define RVEX2(method, excode, type) \
        public virtual SObject method(type arg1) { \
            Exn.fault(Constants.excode, null, arg1, this); \
            return Factory.Impossible; \
        }
#define VEX3(method, excode) \
        public virtual SObject method(SObject arg2, SObject arg3) { \
            Exn.fault(Constants.excode, null, this, arg2, arg3); \
            return Factory.Impossible; \
        }
#define RVEX3(method, excode, type) \
        public virtual SObject method(type arg2, SObject arg3) { \
            Exn.fault(Constants.excode, null, this, arg2, arg3); \
            return Factory.Impossible; \
        }

#define VEX2X(method, excode, type) \
        VEX2(method, excode) \
        RVEX2(method##_2, excode, type)
#define VEX3X(method, excode, type) \
        VEX3(method, excode) \
        RVEX3(method##_2, excode, type)

#define DISPATCH2(method) \
        public override SObject method(SObject arg2) {\
            return arg2.method##_2(this); \
        }
#define DISPATCH3(method) \
        public override SObject method(SObject arg2, SObject arg3) {\
            return arg2.method##_2(this, arg3); \
        }

#define DISPATCH2_W_TAG(method, tag, excode) \
        public override SObject method(SObject arg2) {\
            check_typetag(tag, arg2, Constants.excode); \
            return arg2.method##_2(this); \
        }
#define DISPATCH3_W_TAG(method, tag, excode) \
        public override SObject method(SObject arg2, SObject arg3) {\
            check_typetag(tag, arg2, arg3, Constants.excode); \
            return arg2.method##_2(this, arg3); \
        }

#define IMPL1(method) \
        public override SObject method()
#define IMPL1_W_TAG(method, tag, excode) \
        public override SObject method() {\
            check_typetag(tag, Constants.excode); \
            return method##_implementation1(); \
        } \
        private SObject method##_implementation1()

#define IMPL2(method) \
        public override SObject method(SObject arg2)

#define OVERRIDE2X(method, type) \
        public override SObject method##_2(type arg1)
#define OVERRIDE3X(method, type) \
        public override SObject method##_2(type arg1, SObject arg3)

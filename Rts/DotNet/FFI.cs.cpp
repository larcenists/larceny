#include "Macros.h"

using System;
using System.Reflection;
using Scheme.RT;
using Scheme.Rep;

namespace Scheme.RT {
    public class FFI {

        public static void ffi_syscall() {
            try {
                ffi_syscall_main();
            } catch (Exception e) {
                Exn.error("exception in ffi: " + e.ToString());
            }
        }

        private static void ffi_syscall_main() {
            SObject scode = Reg.register2;
            SObject arg1 = Reg.register3;
            SObject arg2 = Reg.register4;
            SObject arg3 = Reg.register5;

            if (!scode.isFixnum()) {
                Exn.error("ffi: expected code to be a fixnum", scode);
                return;
            }
            int code = ((SFixnum)scode).value;
            switch (code) {
            case 0: // get type
            {
                string name = ((SByteVL)arg1).asString();
                Type t = Type.GetType(name);
                Reg.Result = Factory.makeForeignF(t);
                return;
            }
            case 1: // get method
            {
                Type t = (Type) ((Foreign)arg1).value;
                string name = ((SByteVL)arg2).asString();
                SObject[] typev = ((SVL)arg3).elements;
                Type[] types = new Type[typev.Length];
                for (int i = 0; i < types.Length; ++i) {
                    types[i] = (Type) ((Foreign)typev[i]).value;
                }
                MethodInfo mi = t.GetMethod(name, types);
                Reg.Result = Factory.makeForeignF(mi);
                return;
            }
            case 2: // datum2foreign
            {
                int conversion = ((SFixnum)arg1).value;
                Reg.Result = datum2foreign(conversion, arg2);
                return;
            }
            case 3: // foreign2datum
            {
                int conversion = ((SFixnum)arg1).value;
                Reg.Result = foreign2datum(conversion, arg2);
                return;
            }
            case 4: // invoke
            {
                MethodInfo m = (MethodInfo) ((Foreign)arg1).value;
                object obj = null;
                if (!m.IsStatic) {
                    obj = unwrapF(arg2);
                }
                SObject[] sargv = ((SVL)arg3).elements;
                object[] args = new object[sargv.Length];
                for (int i = 0; i < args.Length; ++i) {
                    args[i] = unwrapF(sargv[i]);
                }
                object result;
                try {
                    result = m.Invoke(obj, args);
                } catch (Exception e) {
                    Exn.error("ffi:invoke: error in foreign function: " + e);
                    return;
                }

                Reg.Result = wrapF(result);
                return;
            }
            case 5: // get field
            {
                Type t = (Type) ((Foreign)arg1).value;
                string name = ((SByteVL)arg2).asString();
                Reg.Result = getField(t, name);
                return;
            }
            case 6: // get property
            {
                Type t = (Type) ((Foreign)arg1).value;
                string name = ((SByteVL)arg2).asString();
                Reg.Result = getProperty(t, name);
                return;
            }
            case 7: // isa?
            {
                object obj = ((Foreign)arg1).value;
                Type type = (Type) ((Foreign)arg2).value;
                Reg.Result = Factory.wrap(type.IsInstanceOfType(obj));
                return;
            }
            case 8: // field-get
            {
                FieldInfo f = (FieldInfo) ((Foreign)arg1).value;
                object obj = null;
                if (!f.IsStatic) {
                    obj = unwrapF(arg2);
                }
                try {
                    Reg.Result = wrapF(f.GetValue(obj));
                    return;
                } catch (Exception e) {
                    Exn.error("ffi:field-get: " + e);
                    return;
                }
            }
            case 9: // field-set
            {
                FieldInfo f = (FieldInfo) ((Foreign)arg1).value;
                object obj = null;
                if (!f.IsStatic) {
                    obj = unwrapF(arg2);
                }
                object newvalue = unwrapF(arg3);
                try {
                    f.SetValue(obj, newvalue);
                    Reg.Result = Factory.Unspecified;
                    return;
                } catch (Exception e) {
                    Exn.error("ffi:field-set: " + e);
                    return;
                }
            }
            case 10: // foreign?
            {
                Reg.Result = Factory.wrap(arg1 is Foreign);
                return;
            }
            case 11: // get-constructor
            {
                Type t = (Type) ((Foreign)arg1).value;
                SObject[] typev = ((SVL)arg2).elements;
                Type[] types = new Type[typev.Length];
                for (int i = 0; i < types.Length; ++i) {
                    types[i] = (Type) ((Foreign)typev[i]).value;
                }
                Reg.Result = Factory.makeForeignF(t.GetConstructor(types));
                return;
            }
            case 12: // invoke-constructor
            {
                ConstructorInfo m = (ConstructorInfo) ((Foreign)arg1).value;
                SObject[] sargv = ((SVL)arg2).elements;
                object[] args = new object[sargv.Length];
                for (int i = 0; i < args.Length; ++i) {
                    args[i] = unwrapF(sargv[i]);
                }
                try {
                    Reg.Result = wrapF(m.Invoke(args));
                    return;
                } catch (Exception e) {
                    Exn.error("ffi:invoke-constructor: " + e);
                    return;
                }
            }
            case 13: // equals?
            {
                object a = unwrapF(arg1);
                object b = unwrapF(arg2);
                if (a == null) {
                    Reg.Result = Factory.wrap(b == null);
                } else if (b == null) {
                    Reg.Result = Factory.wrap(a == null);
                } else {
                    Reg.Result = Factory.wrap(a.Equals(b));
                }
                return;
            }

            }
            Exn.error("bad ffi syscall code");
            return;
        }

        private static SObject wrapF(object o) {
            if (o is Int32 && SFixnum.inFixnumRange((int)o)) {
                return Factory.wrap((int)o);
            } else {
                return Factory.makeForeign(o);
            }
        }
        private static object unwrapF(SObject s) {
            if (s is SFixnum) {
                return ((SFixnum)s).value;
            } else if (s is Foreign) {
                return ((Foreign)s).value;
            } else {
                FAULT_ERROR("cannot unwrap foreign argument");
            }
        }

        private static SObject datum2foreign(int conversion, SObject obj) {
            // datum->foreign : conversion value -> F
            switch (conversion) {
                case 0: // object
                {
                    if (obj is SFixnum || obj is Foreign) {
                        return obj;
                    } else {
                        FAULT_ERROR("datum->foreign (object) expected F");
                    }
                }
                case 1: // schemeobject
                {
                    return Factory.makeForeign(obj);
                }
                case 2: // string
                {
                    if (obj is SByteVL) {
                        string value = ((SByteVL)obj).asString();
                        return Factory.makeForeign(value);
                    } else {
                        FAULT_ERROR("datum->foreign (string) expected string");
                    }
                }
                case 3: // symbol
                {
                    Exn.error("datum->foreign (symbol) not implemented in runtime");
                    return Factory.Impossible;
                }
                case 4: // bytes
                {
                    if (obj is SByteVL) {
                        return Factory.makeForeign(((SByteVL)obj).elements);
                    } else {
                        Exn.error("datum->foreign (bytes) expected bytevector");
                        return Factory.Impossible;
                    }
                }
                case 5: // int
                {
                    if (obj is SFixnum) {
                        return obj;
                    } else if (obj.isBignum()) {
                        SByteVL n = (SByteVL)obj;
                        if (Number.getBignumLength(n) == 1) {
                            uint magn = (uint)(Number.bignumRef(n, 1) << 16)
                                      + (uint)Number.bignumRef(n, 0);
                            int val = Number.getBignumSign(n);
                            return Factory.makeForeign(val);
                        }
                    }
                    Exn.error("datum->foreign (int) expected small integer");
                    return Factory.Impossible;
                }
                case 6: // float
                {
                    if (obj.isFlonum()) {
                        double value = ((SByteVL)obj).unsafeAsDouble(0);
                        return Factory.makeForeign((float)value);
                    } else {
                        Exn.error("datum->foreign (float) expected flonum");
                        return Factory.Impossible;
                    }
                }
                case 7: // double
                {
                    if (obj.isFlonum()) {
                        double value = ((SByteVL)obj).unsafeAsDouble(0);
                        return Factory.makeForeign(value);
                    } else {
                        Exn.error("datum->foreign (float) expected flonum");
                        return Factory.Impossible;
                    }
                }
                case 8: // void
                {
                    Exn.error("datum->foreign (void) not allowed");
                    return Factory.Impossible;
                }
            }
            Exn.error("datum->foreign: unknown conversion");
            return Factory.Impossible;
        }
        private static SObject foreign2datum(int conversion, SObject obj) {
            // foreign->datum : conversion F -> value
            object value;
            if (obj is Foreign) {
                value = ((Foreign)obj).value;
            } else if (obj is SFixnum) {
                value = ((SFixnum)obj).value;
            } else {
                Exn.error("foreign->datum: argument is not foreign-box or fixnum");
                return Factory.Impossible;
            }
            
            switch (conversion) {
                case 0: { // object
                    return obj;
                }
                case 1: { // schemeobject
                    if (value is SObject) {
                        return (SObject)value;
                    } else {
                        Exn.error("foreign->datum (schemeobject): not a scheme value");
                        return Factory.Impossible;
                    }
                }
                case 2: { // string
                    if (value is string) {
                        return Factory.wrap((string)value);
                    } else {
                        Exn.error("foreign->datum (string): not a string");
                        return Factory.Impossible;
                    }
                }
                case 3: { // symbol
                    Exn.error("foreign->datum (symbol): not handled by runtime");
                    return Factory.Impossible;
                }
                case 4: { // bytes
                    if (value is byte[]) {
                        return Factory.makeString((byte[])value);
                    } else {
                        Exn.error("foreign->datum (bytes): not byte[]");
                        return Factory.Impossible;
                    }
                }
                case 5: { // int
                    if (value is int) {
                        return Factory.makeNumber((int)value);
                    } else if (value is long) {
                        return Factory.makeNumber((long) value);
                    } else {
                        Exn.error("foreign->datum (int): not an integer");
                        return Factory.Impossible;
                    }
                }
                case 6: { // float
                    if (value is float) {
                        return Factory.wrap((float)value);
                    } else {
                        Exn.error("foreign->datum (float): not a float");
                        return Factory.Impossible;
                    }
                }
                case 7: { // double
                    if (value is double) {
                        return Factory.wrap((double)value);
                    } else {
                        Exn.error("foreign->datum (double): not a double");
                        return Factory.Impossible;
                    }
                }
                case 8: { // void
                    return Factory.Unspecified;
                }
            }
            Exn.error("foreign->datum: unknown conversion");
            return Factory.Impossible;
        }

        public static SObject getMethod(Type type, string name, Type[] formals) {
            MethodInfo m = type.GetMethod(name, formals);
            return Factory.makeForeignF(m);
        }

        public static SObject getField(Type type, string name) {
            FieldInfo f = type.GetField(name);
            return Factory.makeForeignF(f);
        }

        public static SObject getProperty(Type type, string name) {
            PropertyInfo p = type.GetProperty(name);
            if (p == null) {
                Exn.internalError("no such property");
                return null;
            }
            MethodInfo pget = p.GetGetMethod(false);
            MethodInfo pset = p.GetSetMethod(false);
            return Factory.makePair
                (Factory.makeForeignF(pget),
                 Factory.makeForeignF(pset));
        }

        // Public Constants
        public static readonly bool TRUE = true;
        public static readonly bool FALSE = false;
        public static readonly object NULL = null;

    }
}

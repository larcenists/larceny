using System;
using System.Reflection;
using Scheme.RT;
using Scheme.Rep;

namespace Scheme.RT {
    public class FFI {

        public static void ffi_syscall() {
            int code = ((SFixnum)Reg.register2).value;
            switch (code) {
            case 0:
                Reg.Result = getType(((SByteVL)Reg.register3).asString());
                return;
            case 1:
                Type t = (Type) ((Foreign)Reg.register3).value;
                string name = ((SByteVL)Reg.register4).asString();
                SObject[] typev = ((SVL)Reg.register5).elements;
                Type[] types = new Type[typev.Length];
                for (int i = 0; i < types.Length; ++i) {
                    types[i] = (Type) ((Foreign)typev[i]).value;
                }
                Reg.Result = getMethod(t, name, types);
                return;
            case 2:
                Reg.Result = Factory.makeForeign(naturalConversion(Reg.register3));
                return;
            case 3:
                MethodInfo m = (MethodInfo) ((Foreign)Reg.register3).value;
                object obj = ((Foreign)Reg.register4).value;
                SObject[] sargv = ((SVL)Reg.register5).elements;
                object[] args = new object[sargv.Length];
                for (int i = 0; i < args.Length; ++i) {
                    args[i] = ((Foreign)sargv[i]).value;
                }
                Reg.Result = invokeMethod(m, obj, args);
                return;
            }
            Exn.fault(Constants.EX_UNSUPPORTED, "bad ffi syscall code");
            return;
        }

        public static SObject getType(string name) {
            Type t = Type.GetType(name);
            return Factory.makeForeign(Type.GetType(name));
        }
        
        public static SObject getMethod(Type type, string name, Type[] formals) {
            MethodInfo m = type.GetMethod(name, formals);
            if (m == null) {
                Exn.internalError("no such method");
                return null;
            }
            return Factory.makeForeign(m);
        }

        public static object naturalConversion(SObject obj) {
            if (obj.isFixnum()) {
                return ((SFixnum)obj).value;
            } else if (obj.isChar()) {
                return ((SChar)obj).val;
            } else if (obj.isString()) {
                return ((SByteVL)obj).asString();
            } else if (obj == SObject.False) {
                return null;
            } else {
                Exn.fault(Constants.EX_UNSUPPORTED, "natural-conversion: none available for " + obj);
                return null;
            }
        }
        
        public static SObject invokeMethod(MethodInfo m, object obj, object[] args) {
            object result = m.Invoke(obj, args);
            return Factory.makeForeign(result);
        }
    }
}
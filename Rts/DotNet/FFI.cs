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
                Exn.debug.WriteLine("exception in ffi: " + e.ToString());
                Exn.fault(Constants.EX_UNSUPPORTED);
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
                Reg.Result = getType(((SByteVL)arg1).asString());
                return;
            case 1: // get method
            {
                Type t = (Type) ((Foreign)arg1).value;
                string name = ((SByteVL)arg2).asString();
                SObject[] typev = ((SVL)arg3).elements;
                Type[] types = new Type[typev.Length];
                for (int i = 0; i < types.Length; ++i) {
                    types[i] = (Type) ((Foreign)typev[i]).value;
                }
                Reg.Result = getMethod(t, name, types);
                return;
            }
            case 2: // foreign-box
                // Reg.Result = Factory.makeForeign(Scheme2CLR(arg1));
                Reg.Result = Factory.makeForeign(arg1);
                return;
            case 3: // foreign-unbox
                // Reg.Result = CLR2Scheme(((Foreign)arg1).value);
                object value = ((Foreign)arg1).value;
                if (value is SObject) {
                    Reg.Result = (SObject) value;
                } else {
                    Reg.Result = Factory.False;
                }
                return;
            case 4: // invoke
            {
                MethodInfo m = (MethodInfo) ((Foreign)arg1).value;
                object obj = null;
                if (!m.IsStatic) {
                    obj = ((Foreign)arg2).value;
                }
                SObject[] sargv = ((SVL)arg3).elements;
                object[] args = new object[sargv.Length];
                for (int i = 0; i < args.Length; ++i) {
                    args[i] = ((Foreign)sargv[i]).value;
                }
                Reg.Result = invokeMethod(m, obj, args);
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
                    obj = ((Foreign)arg2).value;
                }
                Reg.Result = Factory.makeForeign(f.GetValue(obj));
                return;
            }
            case 9: // field-set
            {
                FieldInfo f = (FieldInfo) ((Foreign)arg1).value;
                object obj = null;
                if (!f.IsStatic) {
                    obj = ((Foreign)arg2).value;
                }
                object newvalue = ((Foreign)arg3).value;
                f.SetValue(obj, newvalue);
                Reg.Result = Factory.Unspecified;
                return;
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
                    args[i] = ((Foreign)sargv[i]).value;
                }
                Reg.Result = Factory.makeForeign(m.Invoke(args));
                return;
            }

            }
            Exn.fault(Constants.EX_UNSUPPORTED, "bad ffi syscall code");
            return;
        }

        public static SObject getType(string name) {
            Type t = Type.GetType(name);
            return Factory.makeForeignF(Type.GetType(name));
        }
        
        public static SObject getMethod(Type type, string name, Type[] formals) {
            MethodInfo m = type.GetMethod(name, formals);
            if (m == null) {
                Exn.internalError("no such method");
                return null;
            }
            return Factory.makeForeignF(m);
        }

        public static SObject getField(Type type, string name) {
            FieldInfo f = type.GetField(name);
            if (f == null) {
                Exn.internalError("no such field");
                return null;
            }
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

        public static object Scheme2CLR(SObject obj) {
            if (obj.isFixnum()) {
                return ((SFixnum)obj).value;
            } else if (obj.isChar()) {
                return ((SChar)obj).val;
            } else if (obj.isString()) {
                return ((SByteVL)obj).asString();
            } else if (obj == Factory.False) {
                return null;
            } else {
                return null;
            }
        }
        
        public static SObject CLR2Scheme(object obj) {
            if (obj is string) {
                return Factory.wrap((string)obj);
            } else if (obj is Int32) {
                return Factory.wrap((int)obj);
            } else if (obj == null) {
                return Factory.False;
            } else {
                return Factory.False;
            }
        }
        
        public static SObject invokeMethod(MethodInfo m, object obj, object[] args) {
            object result = m.Invoke(obj, args);
            return Factory.makeForeign(result);
        }
        
        public static SObject ForeignNull = null;
        
        
    }
}

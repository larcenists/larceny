using System;
using System.Reflection;
using Scheme.RT;
using Scheme.Rep;
using System.Windows.Forms;

namespace Scheme.RT {
    public class FFI_message_filter : IMessageFilter {
        Procedure scheme_filter;

        public FFI_message_filter (Procedure _scheme_filter)
        {
            this.scheme_filter = _scheme_filter;
        }

        public bool PreFilterMessage (ref Message m)
        {
            return Call.callback (scheme_filter, Factory.makeForeign (m)) != Factory.False;
        }
    }


    public class FFI {

        // Public Constants
        public const bool TRUE = true;
        public const bool FALSE = false;
        public const object NULL = null;

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
            SObject arg4 = Reg.register6;

            if (!scode.isFixnum()) {
                Exn.error("ffi: expected code to be a fixnum", scode);
                return;
            }
            int code = ((SFixnum)scode).value;
            switch (code) {
            case 0: // get type
            {
                string name = ((SByteVL)arg1).asString();
                // case insensitive lookup
                Reg.Result = Factory.makeForeignF (Type.GetType (name, false, true));
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
                Reg.Result = type.IsInstanceOfType(obj) ? Factory.True : Factory.False;
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
                Reg.Result = (arg1 is Foreign) ? Factory.True : Factory.False;
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
                    Reg.Result = (b == null) ? Factory.True : Factory.False;
                } else if (b == null) {
                    Reg.Result = (a == null) ? Factory.True : Factory.False;
                } else {
                    Reg.Result = (a.Equals(b)) ? Factory.True : Factory.False;
                }
                return;
            }
            case 14: // get-property-value
            {
                PropertyInfo pi = (PropertyInfo) ((Foreign) arg1).value;
                object obj = unwrapF (arg2);
                SObject[] sargv = ((SVL) arg3).elements;
                object[] args = new object [sargv.Length];
                for (int i = 0; i < args.Length; ++i)
                    args[i] = unwrapF (sargv[i]);
                object result;
                try {
                    result = pi.GetValue (obj, args);
                    }
                catch (Exception e) {
                    Exn.error ("ffi:invoke: error in foreign function: " + e);
                    return;
                   }
                Reg.Result = wrapF (result);
                return;
            }
            case 15: // set-property-value
            {
                PropertyInfo pi = (PropertyInfo) ((Foreign) arg1).value;
                object obj = unwrapF (arg2);
                object newval = unwrapF (arg3);
                SObject[] sargv = ((SVL) arg4).elements;
                object[] args = new object [sargv.Length];
                for (int i = 0; i < args.Length; ++i)
                    args[i] = unwrapF (sargv[i]);
                try {
                       pi.SetValue (obj, newval, args);
                    }
                catch (Exception e) {
                    Exn.error ("ffi:invoke: error in foreign function: " + e);
                    return;
                   }
                Reg.Result = Factory.Unspecified;
                return;
            }
            case 16: // to-string
            {
                Reg.Result = Factory.makeString (unwrapF (arg1).ToString());
                return;
            }
            case 17: // get-property-value-boolean
            {
                PropertyInfo pi = (PropertyInfo) ((Foreign) arg1).value;
                object obj = unwrapF (arg2);
                SObject[] sargv = ((SVL) arg3).elements;
                object[] args = new object [sargv.Length];
                for (int i = 0; i < args.Length; ++i)
                    args[i] = unwrapF (sargv[i]);
                object result;
                try {
                    result = pi.GetValue (obj, args);
                    }
                catch (Exception e) {
                    Exn.error ("ffi:invoke: error in foreign function: " + e);
                    return;
                   }
                Reg.Result =  ((bool)result != false) ? Factory.True : Factory.False;
                return;
            }
            case 18: // get-property-value-int
            {
                PropertyInfo pi = (PropertyInfo) ((Foreign) arg1).value;
                object obj = unwrapF (arg2);
                SObject[] sargv = ((SVL) arg3).elements;
                object[] args = new object [sargv.Length];
                for (int i = 0; i < args.Length; ++i)
                    args[i] = unwrapF (sargv[i]);
                object result;
                try {
                    result = pi.GetValue (obj, args);
                    }
                catch (Exception e) {
                    Exn.error ("ffi:invoke: error in foreign function: " + e);
                    return;
                   }
                Reg.Result = Factory.makeNumber ((int)result);
                return;
            }
            case 19: // array ref
            {
               Array ra = (Array)((Foreign) arg1).value;
               int index = ((SFixnum) arg2).value;
               Reg.Result = wrapF (ra.GetValue (index));
               return;
            }
            case 20: // object-type
            {
              Reg.Result = Factory.makeForeign (unwrapF (arg1).GetType());
              return;
            }
            case 21: // callback test
            {
              Reg.Result = Call.callback ((Procedure)arg1, Factory.makeString ("foo"));
              return;
            }
            }
            Exn.error("bad ffi syscall code");
            return;
        }

        private static SObject wrapF(object o) {
            if (o is Int32 && SFixnum.inFixnumRange((int)o)) {
                return Factory.makeFixnum ((int)o);
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
                Exn.error("cannot unwrap foreign argument"); return Factory.Impossible;;
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
                        Exn.error("datum->foreign (object) expected F"); return Factory.Impossible;;
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
                        Exn.error("datum->foreign (string) expected string"); return Factory.Impossible;;
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
                case 9: // message filter
                {
                    if (obj is Procedure)
                        return Factory.makeForeign (new FFI_message_filter ((Procedure) obj));
                    else {
                        Exn.error("datum->foreign (message filter) expected procedure");
                        return Factory.Impossible;
                        }
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
                        return Factory.makeString ((string)value);
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
                    if (value is Enum)   return Factory.makeNumber ((int)    value);
                    if (value is Byte)   return Factory.makeFixnum ((byte)   value);
                    if (value is SByte)  return Factory.makeFixnum ((sbyte)  value);
                    if (value is char)   return Factory.makeFixnum ((char)   value);
                    if (value is short)  return Factory.makeFixnum ((short)  value);
                    if (value is int)    return Factory.makeNumber ((int)    value);
                    if (value is long)   return Factory.makeNumber ((long)   value);
                    if (value is ushort) return Factory.makeFixnum ((ushort) value);
                    if (value is uint)   return Factory.makeNumber ((uint)   value);
                    if (value is ulong)  return Factory.makeNumber ((ulong)  value);

                    Exn.error("foreign->datum (int): not an integer");
                    return Factory.Impossible;
                }
                case 6: { // float
                    if (value is float) {
                        return Factory.makeFlonum ((float)value);
                    } else {
                        Exn.error("foreign->datum (float): not a float");
                        return Factory.Impossible;
                    }
                }
                case 7: { // double
                    if (value is double) {
                        return Factory.makeFlonum ((double)value);
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
            return Factory.makeForeignF (type.GetMethod(name, formals));
        }

        public static SObject getField(Type type, string name) {
            return Factory.makeForeignF (type.GetField(name));
        }

        public static SObject getProperty(Type type, string name) {
            return Factory.makeForeignF (type.GetProperty (name));
        }

    }
}

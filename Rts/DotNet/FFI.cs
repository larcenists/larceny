#line 1 "c:\\Home\\Jrm\\CommonLarceny\\Rts\\DotNet\\FFI.cs.cpp"
#line 1 "c:\\home\\jrm\\commonlarceny\\rts\\dotnet\\Macros.h"







#line 2 "c:\\Home\\Jrm\\CommonLarceny\\Rts\\DotNet\\FFI.cs.cpp"

using System;
using System.Reflection;
using Scheme.RT;
using Scheme.Rep;
// using System.Windows.Forms;

namespace Scheme.RT {






    public class FFI {


        public const bool TRUE = true;
        public const bool FALSE = false;
        public const object NULL = null;

        public static void ffi_syscall() {
            try {
                SObject scode = Reg.register2;
                ffi_syscall_main(((SFixnum)scode).value);
            } catch (Exception e) {
                Exn.error("exception in ffi: " + e.ToString());
            }
        }

        private static void ffi_syscall_main (int code) {





          switch (code) {

            case 0:

            {
                Version v = Environment.Version;

                Reg.Result =
                    Factory.makePair
                    (Factory.makeNumber (v.Major),
                     Factory.makePair
                     (Factory.makeNumber (v.Minor),
                      (v.Build == -1)
                      ? (SObject) Factory.Null
                      : (SObject) Factory.makePair (Factory.makeNumber (v.Build),
                                                    (v.Revision == -1)
                                                    ? (SObject) Factory.Null
                                                    : (SObject) Factory.makePair (Factory.makeNumber (v.Revision),
                                                                                  Factory.Null))));
                return;
            }

            case 1:

            {

                Reg.Result =
                    Factory.makePair (Factory.makeNumber (0),
                                      Factory.makePair (Factory.makeNumber (0), Factory.Null));
                return;
            }

            case 2:

            {




                Reg.Result = (Reg.register3 is ForeignBox) ? Factory.True : Factory.False;
                return;
            }

            case 3:
            {
                SObject arg1 = Reg.register3;
                object val = ((ForeignBox) arg1).value;
                Reg.Result = Factory.makeString ((val == null) ? "null" : val.ToString());
                return;
            }

            case 4:
            {
                SObject arg1 = Reg.register3;
                Reg.Result = Factory.makeForeignBox (((ForeignBox) arg1).value.GetType());
                return;
            }

            case 5:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                object obj = ((ForeignBox)arg1).value;
                Type type = (Type) ((ForeignBox)arg2).value;
                Reg.Result = type.IsInstanceOfType(obj) ? Factory.True : Factory.False;
                return;
            }

            case 6:
            {


                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;


                if (arg1 == arg2) {
                    Reg.Result = Factory.True;
                    }
                else if (arg1 is ForeignBox) {
                    if (arg2 is ForeignBox) {


                        if (((ForeignBox)arg1).value == null) {
                            Reg.Result = (((ForeignBox)arg2).value == null)
                                ? Factory.True
                                : Factory.False;
                            }
                        else if (((ForeignBox)arg2).value == null) {
                            Reg.Result = Factory.False;
                            }
                        else {
                            Reg.Result = ((((ForeignBox)arg1).value).Equals (((ForeignBox)arg2).value))
                                ? Factory.True
                                : Factory.False;
                            }
                        }
                    else if (arg2 is SFixnum) {
                        Reg.Result = Factory.False;
                        }
                    else if (arg2 is SImmediate) {
                        Reg.Result = Factory.False;
                        }
                    else
                        Exn.error("cannot unwrap foreign argument");
                    }
                else if (arg1 is SFixnum) {
                    if (arg2 is SFixnum) {
                        Reg.Result = ((((ForeignBox)arg1).value) == (((ForeignBox)arg2).value))
                            ? Factory.True
                            : Factory.False;
                        }
                    else if (arg2 is ForeignBox)  {
                        Reg.Result = Factory.False;
                        }
                    else if (arg2 is SImmediate) {
                        Reg.Result = Factory.False;
                        }
                    else
                        Exn.error("cannot unwrap foreign argument");
                    }

                else if (arg1 is SImmediate) {
                    if (arg2 is SImmediate) {
                        Reg.Result = Factory.False;
                        }
                    else if (arg2 is ForeignBox) {
                        Reg.Result = Factory.False;
                        }
                    else if (arg2 is SFixnum) {
                        Reg.Result = Factory.False;
                        }
                    else
                        Exn.error("cannot unwrap foreign argument");
                    }
                else
                    Exn.error("cannot unwrap foreign argument");
                return;
            }

            case 7:
            {



                Type ft = Type.GetType (((SByteVL)(Reg.register3)).asString(), false, true);

                Reg.Result = (ft == null) ? Factory.False : Factory.makeForeignBox (ft);
                return;
            }



            case 8:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                FieldInfo fi = ((Type) ((ForeignBox)arg1).value).GetField (((SByteVL)arg2).asString());
                Reg.Result = (fi == null) ? Factory.False : Factory.makeForeignBox (fi);
                return;
            }


            case 9:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                SObject[] typev = ((SVL)arg2).elements;
                Type[] types = new Type[typev.Length];
                for (int i = 0; i < types.Length; ++i) {
                    types[i] = (Type) ((ForeignBox)typev[i]).value;
                }
                ConstructorInfo ci = ((Type) ((ForeignBox)arg1).value).GetConstructor (types);
                Reg.Result = (ci == null) ? Factory.False : Factory.makeForeignBox (ci);
                return;
            }

            case 10:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                SObject arg3 = Reg.register5;
                SObject[] typev = ((SVL)arg3).elements;
                Type[] types = new Type[typev.Length];
                for (int i = 0; i < types.Length; ++i) {
                    types[i] = (Type) ((ForeignBox)typev[i]).value;
                }
                MethodInfo mi = ((Type) ((ForeignBox)arg1).value).GetMethod(((SByteVL)arg2).asString(), types);
                Reg.Result = (mi == null) ? Factory.False : Factory.makeForeignBox (mi);
                return;
            }

            case 11:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                SObject arg3 = Reg.register5;
                SObject[] typev = ((SVL)arg3).elements;
                Type[] types = new Type[typev.Length];
                for (int i = 0; i < types.Length; ++i) {
                    types[i] = (Type) ((ForeignBox)typev[i]).value;
                }
                PropertyInfo pi = ((Type) ((ForeignBox)arg1).value).GetProperty (((SByteVL)arg2).asString(), types);
                Reg.Result = (pi == null) ? Factory.False : Factory.makeForeignBox (pi);
                return;
            }

            case 12:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                FieldInfo fi = (FieldInfo) ((ForeignBox)arg1).value;
                try {
                    Reg.Result = Factory.makeForeignBox (fi.GetValue (fi.IsStatic ? null : ((ForeignBox) arg2).value));
                    return;
                } catch (Exception e) {
                    Exn.error("ffi:field-get: " + e);
                    return;
                }
            }

            case 13:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                SObject arg3 = Reg.register5;
                FieldInfo fi = (FieldInfo) ((ForeignBox)arg1).value;
                try {
                    fi.SetValue (fi.IsStatic ? null : ((ForeignBox)arg2).value, ((ForeignBox) arg3).value);
                    Reg.Result = Factory.Unspecified;
                    return;
                } catch (Exception e) {
                    Exn.error("ffi:field-set: " + e);
                    return;
                }
            }

            case 14:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                ConstructorInfo ci = (ConstructorInfo) ((ForeignBox)arg1).value;
                SObject[] sargv = ((SVL)arg2).elements;
                object[] args = new object[sargv.Length];
                for (int i = 0; i < args.Length; ++i) {
                    args[i] = ((ForeignBox)(sargv[i])).value;
                }
                try {
                    Reg.Result = Factory.makeForeignBox (ci.Invoke (args));
                    return;
                } catch (Exception e) {
                    Exn.error("ffi:invoke-constructor: " + e);
                    return;
                }
            }

            case 15:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                SObject arg3 = Reg.register5;
                MethodInfo mi = (MethodInfo) ((ForeignBox)arg1).value;
                SObject[] sargv = ((SVL)arg3).elements;
                object[] args = new object[sargv.Length];
                for (int i = 0; i < args.Length; i++)
                    args [i] = ((ForeignBox)(sargv[i])).value;





                object result;
                try {
                    result = mi.Invoke (mi.IsStatic ? null : ((ForeignBox)arg2).value, args);
                    }
                catch (Exception e) {
                    Exn.error("ffi:invoke: error in foreign function: " + e);
                    return;
                    }

                Reg.Result = Factory.makeForeignBox (result);
                return;
            }

            case 16:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                SObject arg3 = Reg.register5;
                SObject[] sargv = ((SVL) arg3).elements;
                object[] args = new object [sargv.Length];
                for (int i = 0; i < args.Length; ++i)
                    args[i] = ((ForeignBox)(sargv[i])).value;
                object result;
                try {
                    result = ((PropertyInfo) ((ForeignBox) arg1).value).GetValue (((ForeignBox)arg2).value, args);
                    }
                catch (Exception e) {
                    Exn.error ("ffi:invoke: error in foreign function: " + e);
                    return;
                    }
                Reg.Result = Factory.makeForeignBox (result);
                return;
            }

            case 17:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                SObject arg3 = Reg.register5;
                SObject arg4 = Reg.register6;
                SObject[] sargv = ((SVL) arg4).elements;
                object[] args = new object [sargv.Length];
                for (int i = 0; i < args.Length; ++i)
                    args[i] = ((ForeignBox)(sargv[i])).value;
                try {
                    ((PropertyInfo) ((ForeignBox) arg1).value).SetValue (((ForeignBox) arg2).value, ((ForeignBox) arg3).value, args);
                    }
                catch (Exception e) {
                    Exn.error ("ffi:invoke: error in foreign function: " + e);
                    return;
                   }
                Reg.Result = Factory.Unspecified;
                return;
            }

            case 18:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                Reg.Result =
                    Factory.makeForeignBox (((Array)((ForeignBox) arg1).value).GetValue (((SFixnum) arg2).value));
                return;
            }

            case 19:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                Reg.Result = datum2foreign (((SFixnum)arg1).value, arg2);
                return;
            }

            case 20:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                Reg.Result = foreign2datum (((SFixnum)arg1).value, arg2);
                return;
            }

            case 21:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                SObject arg3 = Reg.register5;
                SObject[] sargv = ((SVL) arg3).elements;
                object[] args = new object [sargv.Length];
                for (int i = 0; i < args.Length; ++i)
                    args[i] = ((ForeignBox)(sargv[i])).value;
                object result;
                try {
                    result = ((PropertyInfo) ((ForeignBox) arg1).value).GetValue (((ForeignBox) arg2).value, args);
                    }
                catch (Exception e) {
                    Exn.error ("ffi:invoke: error in foreign function: " + e);
                    return;
                   }
                Reg.Result = Factory.makeBoolean ((bool)result != false);
                return;
            }
            case 22:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                SObject arg3 = Reg.register5;
                SObject[] sargv = ((SVL) arg3).elements;
                object[] args = new object [sargv.Length];
                for (int i = 0; i < args.Length; ++i)
                    args[i] = ((ForeignBox) (sargv[i])).value;
                object result;
                try {
                    result = ((PropertyInfo) ((ForeignBox) arg1).value).GetValue (((ForeignBox) arg2).value, args);
                    }
                catch (Exception e) {
                    Exn.error ("ffi:invoke: error in foreign function: " + e);
                    return;
                   }
                Reg.Result = Factory.makeNumber((int)result);
                return;
            }
            case 23:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                SObject arg3 = Reg.register5;
                SObject[] sargv = ((SVL) arg3).elements;
                object[] args = new object [sargv.Length];
                for (int i = 0; i < args.Length; ++i)
                    args[i] = ((ForeignBox)(sargv[i])).value;
                object result;
                try {
                    result = ((PropertyInfo) ((ForeignBox) arg1).value).GetValue (((ForeignBox) arg2).value, args);
                    }
                catch (Exception e) {
                    Exn.error ("ffi:invoke: error in foreign function: " + e);
                    return;
                   }
		//                Reg.Result = Factory.makeForeignBox (System.Windows.Forms.NativeWindow.FromHandle ((IntPtr)result));
                return;
            }
            case 24:
            {
                SObject arg1 = Reg.register3;
                SObject arg2 = Reg.register4;
                SObject arg3 = Reg.register5;
                SObject[] sargv = ((SVL) arg3).elements;
                object[] args = new object [sargv.Length];
                for (int i = 0; i < args.Length; ++i)
                    args[i] = ((ForeignBox)(sargv[i])).value;
                object result;
                try {
                    result = ((PropertyInfo) ((ForeignBox) arg1).value).GetValue (((ForeignBox) arg2).value, args);
                    }
                catch (Exception e) {
                    Exn.error ("ffi:invoke: error in foreign function: " + e);
                    return;
                   }
                Reg.Result = Factory.makeNumber ((int) ((IntPtr) result));
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
                return Factory.makeForeignBox (o);
            }
        }
        private static object unwrapF(SObject s) {
            if (s is SFixnum) {
                return ((SFixnum)s).value;
            } else if (s is ForeignBox) {
                return ((ForeignBox)s).value;
            } else {
                Exn.error("cannot unwrap foreign argument"); return Factory.Impossible;;
            }
        }

        private static SObject datum2foreign(int conversion, SObject obj) {




            switch (conversion) {

                case 0:
                {
                    return Factory.makeForeignBox (obj);
                }


                case 1:
                {
                    return Factory.makeForeignBox (((SByteVL)obj).asString());
                }


                case 2:
                {
                    return Factory.makeForeignBox ((Byte) ((SFixnum)obj).value);
                }

                case 3:
                {
                    return Factory.makeForeignBox ((UInt16) ((SFixnum)obj).value);
                }

                case 4:
                {
                    return Factory.makeForeignBox ((UInt32) ((SFixnum)obj).value);
                }

                case 5:
                {
                    return Factory.makeForeignBox ((SByte) ((SFixnum)obj).value);
                }

                case 6:
                {
                    return Factory.makeForeignBox ((Int16) ((SFixnum)obj).value);
                }

                case 7:
                {
                    return Factory.makeForeignBox ((Int32) ((SFixnum)obj).value);
                }























































































#line 646 "c:\\Home\\Jrm\\CommonLarceny\\Rts\\DotNet\\FFI.cs.cpp"

            }
            Exn.error("datum->foreign: unknown conversion");
            return Factory.Impossible;
        }
        private static SObject foreign2datum(int conversion, SObject obj) {

            object value;
            if (obj is ForeignBox) {
                value = ((ForeignBox)obj).value;
            } else if (obj is SFixnum) {
                value = ((SFixnum)obj).value;
            } else {
                Exn.error("foreign->datum: argument is not foreign-box or fixnum");
                return Factory.Impossible;
            }

            switch (conversion) {
                case 0: {
                    return obj;
                }
                case 1: {
                    if (value is SObject) {
                        return (SObject)value;
                    } else {
                        Exn.error("foreign->datum (schemeobject): not a scheme value");
                        return Factory.Impossible;
                    }
                }
                case 2: {
                    if (value is string) {
                        return Factory.makeString ((string)value);
                    } else {
                        Exn.error("foreign->datum (string): not a string");
                        return Factory.Impossible;
                    }
                }
                case 3: {
                    Exn.error("foreign->datum (symbol): not handled by runtime");
                    return Factory.Impossible;
                }
                case 4: {
                    if (value is byte[]) {
                        return Factory.makeString((byte[])value);
                    } else {
                        Exn.error("foreign->datum (bytes): not byte[]");
                        return Factory.Impossible;
                    }
                }
                case 5: {
                    if (value is Enum) {
                       Type enum_type = value.GetType ();
                       Type underlying_type = Enum.GetUnderlyingType (enum_type);
                       if (underlying_type == typeof (Byte))   return Factory.makeFixnum ((byte)   value);
                       if (underlying_type == typeof (SByte))  return Factory.makeFixnum ((sbyte)  value);
                       if (underlying_type == typeof (char))   return Factory.makeFixnum ((char)   value);
                       if (underlying_type == typeof (short))  return Factory.makeFixnum ((short)  value);
                       if (underlying_type == typeof (int))    return Factory.makeNumber ((int)    value);
                       if (underlying_type == typeof (long))   return Factory.makeNumber ((long)   value);
                       if (underlying_type == typeof (ushort)) return Factory.makeFixnum ((ushort) value);
                       if (underlying_type == typeof (uint))   return Factory.makeNumber ((uint)   value);
                       if (underlying_type == typeof (ulong))  return Factory.makeNumber ((ulong)  value);
                       Exn.error ("foreign->datum (enum): not an integer");
                       return Factory.Impossible;
                      }
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
                case 6: {
                    if (value is float) {
                        return Factory.makeFlonum((float)value);
                    } else {
                        Exn.error("foreign->datum (float): not a float");
                        return Factory.Impossible;
                    }
                }
                case 7: {
                    if (value is double) {
                        return Factory.makeFlonum((double)value);
                    } else {
                        Exn.error("foreign->datum (double): not a double");
                        return Factory.Impossible;
                    }
                }
                case 8: {
                    return Factory.Unspecified;
                }
            }
            Exn.error("foreign->datum: unknown conversion");
            return Factory.Impossible;
        }
    }
}

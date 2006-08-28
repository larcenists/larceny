#include "Ops.h"

using System;
using System.Collections;
using System.IO;
using System.Text;
using Scheme.RT;
using Scheme.Rep;

namespace Scheme.Rep {

    // -------------------------------------------
    // SObject
    // -------------------------------------------
    public abstract class SObject {
        // Debugging
        public override string ToString() {
            System.IO.StringWriter sw = new System.IO.StringWriter();
            this.write(sw);
            return sw.ToString();
        }
        public virtual void write(TextWriter w) {
            w.Write("#<object:");
            w.Write(this.GetType());
            w.Write(">");
        }

#       include "Ops_SObject.inc"
    }

    // -------------------------------------------
    // SImmediate
    // -------------------------------------------
    public sealed class SImmediate : SObject {
        public readonly string rep;
        public SImmediate(String rep) {
            this.rep = rep;
        }
        public override void write(TextWriter w) {
            w.Write(rep);
        }

#       include "Ops_SImmediate.inc"
    }

    // -------------------------------------------
    // SChar
    // -------------------------------------------
    public sealed class SChar : SObject {
        public const int CHAR_COUNT = 256;
        public static readonly SChar[] characters =
            new SChar[CHAR_COUNT];

        public char val;
        private SChar(char c) {
            this.val = c;
        }
        public static void Initialize ()
        {
            for (int i = 0; i < CHAR_COUNT; ++i) {
                characters[i] = new SChar((char)i);
            }
        }
        public static SChar makeChar(int c) {
          try {
              return characters[c];
              }
          catch (Exception) {
              Exn.internalError("not a valid char");
              return new SChar((char)c);
              }
        }
        public override void write(TextWriter w) {
            w.Write("#\\");
            w.Write(val);
        }

#       include "Ops_SChar.inc"
    }

    // -------------------------------------------
    // The numeric representations
    // -------------------------------------------
    /*
     * Numbers are represented in the following ways:
     *     - fixnum  = SFixnum
     *     - bignum  = SByteVL/bignum
     *     - flonum  = SByteVL/flonum
     *     - ratnum  = SVL/ratnum
     *     - rectnum = SVL/rectnum
     *     - compnum = SByteVL/compnum
     */

    // -------------------------------------------
    // SFixnum
    // -------------------------------------------
    public sealed class SFixnum : SObject {
        public readonly int value;
        public static SFixnum[] pool;
      // NOTE THE COMPILER KNOWS ABOUT THESE CONSTANTS
      // See Asm/IL/config.sch
        public const int minPreAlloc = -32768;
        public const int maxPreAlloc = 65535;
        public const int MAX = (1 << 29) - 1;
        public const int MIN = -(1 << 29);
        public const int BITS = 30;

        public static SFixnum zero;
        public static SFixnum one;
        public static SFixnum two;
        public static SFixnum three;
        public static SFixnum four;

        // Stores numbers minPreAlloc to maxPreAlloc
        //          0 -> (maxPreAlloc - minPreAlloc + 1)
        // minPreAlloc -> maxPreAlloc
        public static void Initialize()
        {
            pool = new SFixnum[maxPreAlloc - minPreAlloc + 1];
            for (int i = 0; i < pool.Length ; i++)
                pool[i] = new SFixnum(i + minPreAlloc);
            zero = makeFixnum (0);
            one = makeFixnum (1);
            two = makeFixnum (2);
            three = makeFixnum (3);
            four = makeFixnum (4);
        }
        private SFixnum(int value) {
            this.value = value;
        }
        public override void write(TextWriter w) {
            w.Write(value);
        }
        public int intValue() {
            return value;
        }
        public static bool inFixnumRange(short n) {
          return true; // (n <= MAX) && (n >= MIN);
        }
        public static bool inFixnumRange(ushort n) {
          return true;  //n <= MAX
        }
        public static bool inFixnumRange(int n) {
            return (n <= MAX) && (n >= MIN);
        }
        public static bool inFixnumRange(uint n) {
            return n <= MAX;
        }
        public static bool inFixnumRange(long n) {
            return (n <= MAX) && (n >= MIN);
        }
        public static bool inFixnumRange(ulong n) {
            return n <= ((ulong)MAX);
        }
        public static SFixnum makeFixnum(int val) {
            if (val >= minPreAlloc && val <= maxPreAlloc)
                return pool[val - minPreAlloc];
            else
                return new SFixnum(val);
        }

#       include "Ops_SFixnum.inc"
    }

    // -------------------------------------------
    // STagged
    // -------------------------------------------
    public class STagged : SObject {
        public int tag;

        public STagged (int tag) {
          this.tag = tag;
	}

        public void check_typetag(int tag, int excode) {
            if (this.tag != tag) Exn.fault(excode, null, this);
        }
        public void check_typetag(int tag, SObject arg2, int excode) {
            if (this.tag != tag) Exn.fault(excode, null, this, arg2);
        }
        public void check_typetag(int tag, SObject arg2, SObject arg3, int excode) {
            if (this.tag != tag) Exn.fault(excode, null, this, arg2, arg3);
        }

#       include "Ops_STagged.inc"
    }

    // -------------------------------------------
    // SVL (vector-like)
    // -------------------------------------------
    public sealed class SVL : STagged {
        public readonly SObject[] elements;

        public SVL (int tag, int size, SObject fill) : base (tag) {
            this.elements = new SObject[size];
            for (int i = 0; i < size; ++i) {elements[i] = fill;};
        }

        public SVL (int tag, SObject[] vec) : base (tag) {
            this.elements = vec;
        }

        public int length() {
            return elements.Length;
        }
        public SObject elementAt(int index) {
            return elements[index];
        }
        public SObject elementAt(SFixnum index) {
            return elements[index.intValue()];
        }
        public void setElementAt(int index, SObject elt) {
            elements[index] = elt;
        }

        public override void write(TextWriter w) {
            if (this.tag == Tags.VectorTag) {
                w.Write("#<vector>");
            } else if (this.tag == Tags.RectnumTag) {
                w.Write("#<rectnum>");
            } else if (this.tag == Tags.RatnumTag) {
                w.Write("#<ratnum>");
            } else if (this.tag == Tags.PortTag) {
                w.Write("#<port>");
            } else if (this.tag == Tags.StructureTag) {
                w.Write("#<structure>");
            } else if (this.tag == Tags.SymbolTag) {
                SByteVL name = (SByteVL) this.elements[0];
                w.Write(name.asString());
            } else {
                w.Write("#<vectorlike: " + this.tag + ">");
            }
        }

#       include "Ops_SVL.inc"
    }

    // -------------------------------------------
    // SByteVL (bytevector-like)
    // -------------------------------------------
    public sealed class SByteVL : STagged {
        public readonly byte[] elements;
        public static System.Text.Encoding stringEncoding
            = new System.Text.ASCIIEncoding();

        public SByteVL(int tag, byte[] vec) : base (tag) {
            this.elements = vec;
        }
        public SByteVL(int tag, int size, byte fill) : base (tag) {
            this.elements = new byte[size];
            if (fill == 0)
                Array.Clear (this.elements, 0, size);
            else {
                for (int i = 0; i < size; i++) elements[i] = fill;
                }
        }

        public int length() {
           return elements.Length;
        }

        public byte getByte(int index) {
           return elements[index];
        }
        public void setByte(int index, byte b) {
           elements [index] = b;
        }

        public void fill(byte b) {
            if (b == 0)
                Array.Clear (this.elements, 0, this.elements.Length);
            else {
                for (int i = 0; i < this.elements.Length; i++) elements[i] = b;
                }
        }

        public short getInt16(int index) {
            return System.BitConverter.ToInt16(elements, index*2);
        }
        public void setInt16(int index, short s) {
            byte[] bytes = System.BitConverter.GetBytes(s);
            elements[index*2] = bytes[0];
            elements[index*2 + 1] = bytes[1];
        }

        public ushort getUInt16(int index) {
            return System.BitConverter.ToUInt16(elements, index*2);
        }
        public void setUInt16(int index, ushort s) {
            byte[] bytes = System.BitConverter.GetBytes(s);
            elements[index*2] = bytes[0];
            elements[index*2 + 1] = bytes[1];
        }

        public int getInt32(int index) {
            return System.BitConverter.ToInt32(elements, index*4);
        }
        public void setInt32(int index, int n) {
            byte[] bytes = System.BitConverter.GetBytes(n);
            int i = index*4;
            elements[i] = bytes[0];
            elements[i+1] = bytes[1];
            elements[i+2] = bytes[2];
            elements[i+3] = bytes[3];
        }

        public uint getUInt32(int index) {
            return System.BitConverter.ToUInt32(elements, index*4);
        }
        public void setUInt32(int index, uint n) {
            byte[] bytes = System.BitConverter.GetBytes(n);
            int i = index*4;
            elements[i] = bytes[0];
            elements[i+1] = bytes[1];
            elements[i+2] = bytes[2];
            elements[i+3] = bytes[3];
        }

        // unsafeAsDouble: interprets bytes as the bit representation
        //     of a double value
        public double unsafeAsDouble(int steps) {
            return System.BitConverter.ToDouble(elements, 4 + steps * 8);
              // steps * sizeof(double)) + offset
        }
        public void unsafeSetDouble(int steps, double d) {
            byte[] b = System.BitConverter.GetBytes(d);
            for (int i = 0; i < 8; ++i) {
                elements[i+4 + steps * 8] = b[i];
            }
        }

        private bool isIntegralFlonum() {
            double v = this.unsafeAsDouble(0);
            bool b = (Math.Ceiling(v) == Math.Floor(v));
            return b;
        }

        public override void write(TextWriter w) {
            if (this.tag == Tags.ByteVectorTag) {
                w.Write("#<bytevector*");
                w.Write(elements.Length);
                w.Write(">");
            } else if (this.tag == Tags.StringTag) {
                w.Write("\"");
                w.Write(this.asString());
                w.Write("\"");
            } else if (this.tag == Tags.CompnumTag) {
                w.Write("#<compnum>");
            } else if (this.tag == Tags.BignumTag) {
                w.Write("#<bignum[{0}]>", Number.getBignumLength(this));
            } else if (this.tag == Tags.FlonumTag) {
                w.Write(this.unsafeAsDouble(0));
            } else {
                w.Write("#<bytevector-like ");
                w.Write(this.tag);
                w.Write(">");
            }
        }

        // asString returns a CLR string with the same characters as the Scheme string
        // It does not add ""
        public string asString() {
            return stringEncoding.GetString(elements);
        }

#       include "Ops_SByteVL.inc"
    }

    // -------------------------------------------
    // SPair
    // -------------------------------------------
    public sealed class SPair : SObject {
        public SObject first;
        public SObject rest;

        public override void write(TextWriter w) {
            w.Write("(");
            writeInList(this, w);
            w.Write(")");
        }

        public static void writeInList(SPair p, TextWriter w) {
            while (true) {
                w.Write(p.first);
                if (p.rest.isPair()) {
                    w.Write(" ");
                    p = (SPair)p.rest;
                } else if (p.rest == Factory.Null) {
                    return;
                } else {
                    w.Write(" . ");
                    p.rest.write(w);
                    return;
                }
            }
        }

        public SPair(SObject elem, SObject rest) {
            this.first = elem;
            this.rest = rest;
        }

        public SObject getFirst() {
            return first;
        }

        public SObject getRest() {
            return rest;
        }

        public void setFirst(SObject first) {
            this.first = first;
        }

        public void setRest(SObject rest) {
            this.rest = rest;
        }

#       include "Ops_SPair.inc"
    }

    // -------------------------------------------
    // Procedure
    // -------------------------------------------
    public sealed class Procedure : STagged {
        public CodeVector entrypoint;
        public Procedure parent;
        public SObject[] rib;
        public SVL constantvector;
        public SObject[] constants;

        public Procedure (CodeVector entrypoint,
			  SVL constantvector,
                          Procedure parent,
                          SObject [] rib) : base (Constants.PROC_TAG)
        {
	  this.entrypoint = entrypoint;
	  this.constantvector = constantvector;
	  this.constants = constantvector.elements;
	  this.parent = parent;
	  this.rib = rib;
	}

        public Procedure(CodeVector entrypoint,
                         SObject constantvector,
                         SObject[] rib) : base (Constants.PROC_TAG) {
            this.tag = Constants.PROC_TAG;
            this.entrypoint = entrypoint;
            this.constantvector = (SVL) constantvector;
            this.constants = this.constantvector.elements;
            if ((rib != null) && (rib.Length > 0))
                this.parent = rib [0] as Procedure;
            this.rib = rib;
        }

        public Procedure(CodeVector entrypoint, SVL constantvector)
            : this(entrypoint, constantvector, null, null) {}

        public Procedure(CodeVector entrypoint, SObject constantvector)
            : this(entrypoint, (SVL) constantvector, null, null) {}

        public Procedure(CodeVector entrypoint)
            : this(entrypoint, Factory.makeVector(1, Factory.False), null, null) {}

        public void setCode(SObject code) {
          CodeVector cv = code as CodeVector;
            if (cv != null) {
                this.entrypoint = cv;
            } else if (code == Factory.False) {
                this.entrypoint = CodeVector.NoCode;
            } else {
                Exn.internalError("procedure-set! 0 called, not a codevector: " + code);
            }
        }

        public SObject getCode() {
          DataCodeVector dcv = this.entrypoint as DataCodeVector;
          return (dcv == null) ? this.entrypoint : dcv.datum;
        }

        public void setConstants(SVL constantvector) {
            this.constantvector = constantvector;
            this.constants = constantvector.elements;
        }

       public CodeAddress InitialCodeAddress
       {
	 get {
	     return this.entrypoint.InitialCodeAddress;
             }
       }

        /** lookup
         * Look up (rib, slot) in lexical environment
         */
        public SObject lookup(int ri, int slot) {
	  Procedure proc;

	  for (proc = this; ri > 0; ri--)
	      proc = proc.parent;

	  return proc.rib [slot];
        }

        /** update
         * Mutate a lexically bound variable at (rib, slot) to new_value
         */
        public void update (int ri, int slot, SObject newValue)
        {
	  Procedure proc = this;

	  for (proc = this; ri > 0; ri--)
	      proc = proc.parent;

	  proc.rib [slot] = newValue;
	  if (slot == 0)
	      proc.parent = newValue as Procedure;
        }

        private string getName() {
            if (this.constants.Length >= 1) {
                SObject d = this.constants[0];
                SVL dsvl = d as SVL;
                if (dsvl != null) {
                    if (dsvl.elements != null && dsvl.elements.Length >= 1) {
                        return dsvl.elements[0].ToString();
                    }
                }
            }
            if (rib != null && rib.Length > 0) {
                return ((Procedure)rib[0]).getName();
            } else {
                return "<unknown>";
            }

        }
        public override void write(TextWriter w) {
            w.Write("#<PROCEDURE: ");
            w.Write(getName());
            w.Write(" = ");
            w.Write(entrypoint.name());
            w.Write(">");
        }

      //
      // This is ugly, but in order to create delegates of the correct
      // type, we have to match the signature exactly.  I expect that
      // this will change come version 2 of the CLR, so we can live
      // with this for now.
      //

        public void event_callback (Object sender, EventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

#if 0
        public void event_callback (Object sender, Microsoft.Win32.PowerModeChangedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, Microsoft.Win32.SessionEndedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, Microsoft.Win32.SessionEndingEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, Microsoft.Win32.TimerElapsedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, Microsoft.Win32.UserPreferenceChangedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, Microsoft.Win32.UserPreferenceChangingEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.AssemblyLoadEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.CancelEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.CollectionChangeEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.Design.ActiveDesignerEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.Design.ComponentChangedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.Design.ComponentChangingEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.Design.ComponentEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.Design.ComponentRenameEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.Design.DesignerEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.Design.DesignerTransactionCloseEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.Design.Serialization.ResolveNameEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.ListChangedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.PropertyChangedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ComponentModel.RefreshEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Configuration.Install.InstallEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Data.Common.RowUpdatedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Data.Common.RowUpdatingEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Data.DataColumnChangeEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Data.DataRowChangeEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Data.FillErrorEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Data.MergeFailedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Data.Odbc.OdbcInfoMessageEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Data.OleDb.OleDbInfoMessageEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

// Documented, but apparently doesn't exist.
//      public void event_callback (Object sender, System.Data.OracleClient.OracleInfoMessageEventArgs e) {
//          Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
//            }

        public void event_callback (Object sender, System.Data.SqlClient.SqlInfoMessageEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

// Documented, but apparently doesn't exist.
//      public void event_callback (Object sender, System.Data.SqlServerCe.SqlCeInfoMessageEventArgs e) {
//          Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
//            }

        public void event_callback (Object sender, System.Data.StateChangeEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Diagnostics.EntryWrittenEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Drawing.Design.PaintValueEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Drawing.Design.ToolboxComponentsCreatedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Drawing.Design.ToolboxComponentsCreatingEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Drawing.Printing.PrintPageEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.IO.ErrorEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.IO.FileSystemEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Management.ManagementEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Messaging.PeekCompletedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Messaging.ReceiveCompletedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.ResolveEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Threading.ThreadExceptionEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Timers.ElapsedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.UnhandledExceptionEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.Security.DefaultAuthenticationEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.Security.FormsAuthenticationEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.Security.PassportAuthenticationEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.Security.WindowsAuthenticationEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.UI.ImageClickEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.UI.WebControls.AdCreatedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.UI.WebControls.CommandEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.UI.WebControls.DataGridItemEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.UI.WebControls.DataGridPageChangedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.UI.WebControls.DataGridSortCommandEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.UI.WebControls.DataListItemEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.UI.WebControls.RepeaterItemEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Web.UI.WebControls.ServerValidateEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.ColumnClickEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.ContentsResizedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.ControlEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.ConvertEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.DateRangeEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.DragEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.DrawItemEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.GiveFeedbackEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.HelpEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.InputLanguageChangedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.InvalidateEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.ItemChangedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.ItemCheckEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.ItemDragEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.KeyEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.KeyPressEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.LabelEditEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.LayoutEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.LinkClickedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.LinkLabelLinkClickedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.MeasureItemEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.MouseEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.NavigateEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.NodeLabelEditEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.PaintEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.PropertyTabChangedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.PropertyValueChangedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.QueryAccessibilityHelpEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.QueryContinueDragEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.ScrollEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.SelectedGridItemChangedEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.SplitterEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.ToolBarButtonClickEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.TreeViewEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Windows.Forms.UICuesEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Xml.Schema.ValidationEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Xml.Serialization.UnreferencedObjectEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Xml.Serialization.XmlAttributeEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Xml.Serialization.XmlElementEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }

        public void event_callback (Object sender, System.Xml.Serialization.XmlNodeEventArgs e) {
            Call.callback (this, Factory.makeForeignBox (sender), Factory.makeForeignBox (e));
            }
#endif

#       include "Ops_Procedure.inc"
    }

    // -------------------------------------------
    // CodeVectors and ConstantVectors
    // -------------------------------------------

    // This should be abstract, but it causes Scheme to be an order of magnitude
    // slower when starting.
    public /* abstract */ class CodeVector : SObject
    {
      // Maximum number of labels to which jump index may refer.
      public const int CONTROL_POINT_LIMIT = 1024;

      public static readonly CodeVector NoCode = new DataCodeVector (Factory.False);

      public readonly CodeAddress [] controlPoints;

      public CodeVector (int controlPointCount)
      {
	if (controlPointCount > CONTROL_POINT_LIMIT)
	    throw new Exception ("Maximum number of control points exceeded: "+controlPointCount);

        this.controlPoints = new CodeAddress [controlPointCount];
        for (int i = 0; i < controlPointCount; ++i)
            this.controlPoints [i] = new CodeAddress (this, i);
      }

        /** call
         * Given a jump index (0 for entry point, NOT the same as label number),
         * start executing at the label corresponding to that code.
         */
        // This should be abstract, but see above.
        // public abstract void call(int jump_index);
      public virtual CodeAddress call (int jump_index)
      {
	throw new Exception ("Subclass of CodeVector did not override call method.");
      }

      public virtual CodeAddress InitialCodeAddress
      {
        get {
	    return this.controlPoints [0];
	    }
      }

      public CodeAddress Address (int i)
      {
	return this.controlPoints [i];
      }

        public virtual int id() { return 0; }
        public string name() {
            Type t = this.GetType();
            string ns = (string) Exn.namespaces[t.Namespace];
            if (ns == null) {
                ns = t.ToString();
            }
            int idn = id();
            return ns + " " + (idn >> 16) + ":" + (idn & 0xFFFF);
        }

        public override void write(TextWriter w) {
            w.Write("#<CodeVector ");
            w.Write(name());
            w.Write(">");
        }
    }

    public class DataCodeVector : CodeVector
    {
        public SObject datum;

        public DataCodeVector(SObject datum) : base (0) {
            this.datum = datum;
        }

        public override CodeAddress call (int ignored)
        {
           throw new Exception ("not a real codevector");
        }
    }

    /* ForeignBox
     * Holds foreign values; cooperates with ffi.
     */
    public class ForeignBox : SObject {
        public object value;
        public ForeignBox(object value) {
            this.value = value;
        }

       public override string ToString() {
         return this.value.ToString();
       }

    }
}

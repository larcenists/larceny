using System;
using System.Collections;
using System.IO;
using System.Text;
using Scheme.RT;
using Scheme.Rep;

namespace Scheme.Rep {

    // -------------------------------------------
    // Tags
    // -------------------------------------------
    public sealed class Tags {
        public static readonly int VectorTag = Constants.VEC_SUBTAG >>2;
        public static readonly int RectnumTag = Constants.RECT_SUBTAG >>2;
        public static readonly int RatnumTag = Constants.RAT_SUBTAG >>2;
        public static readonly int StructureTag = Constants.STRUCT_SUBTAG >>2;
        public static readonly int PortTag = 4; // FIXME: From Lib/Common/typetags.sch
        public static readonly int SymbolTag = Constants.SYM_SUBTAG >>2;

        public static readonly int ByteVectorTag = Constants.BVEC_SUBTAG >>2;
        public static readonly int StringTag = Constants.STR_SUBTAG >>2;
        public static readonly int CompnumTag = Constants.COMP_SUBTAG >>2;
        public static readonly int BignumTag = Constants.BIG_SUBTAG >>2;
        public static readonly int FlonumTag = Constants.FLO_SUBTAG >>2;
    }

    // -------------------------------------------
    // Factory
    // -------------------------------------------
    public sealed class Factory {
        public static readonly double PositiveInfinity = Double.PositiveInfinity;
        public static readonly double NegativeInfinity = Double.NegativeInfinity;
        public static readonly double Nan = Double.NaN;

        public static SImmediate makeBoolean(bool b) {
            return b ? SObject.True : SObject.False;
        }
        public static SObject makeNumber(int num) {
            if (SFixnum.inFixnumRange(num)) {
                return SFixnum.makeFixnum(num);
            } else {
                return makeBignum(num);
            }
        }
        public static SObject makeNumber(long num) {
            int a = (int) num;
            if (a == num) {
                return makeNumber(a);
            } else {
                if (num < 0) {
                    return makeBignum((ulong)-num, false);
                } else {
                    return makeBignum((ulong)num, true);
                }
            }
        }
        public static SFixnum makeFixnum(int num) {
            return SFixnum.makeFixnum(num);
        }
        public static SByteVL makeBignum(int num) {
			//Console.Out.WriteLine("## {0} ##", num);
            if (num < 0) {
                return Number.makeBignum((ulong)-num, false);
            } else {
                return Number.makeBignum((ulong)num, true);
            }
        }
        public static SByteVL makeBignum(ulong value, bool positive) {
            return Number.makeBignum(value, positive);
        }
        public static SByteVL makeBignum(short[] data, bool positive) {
            return Number.makeBignum(data, positive);
        }
        public static SVL makeRatnum(SObject num, SObject denom) {
            return new SVL(Tags.RatnumTag, new SObject[] {num, denom});
        }

        public static SVL makeRectnum(SObject real, SObject imag) {
            return new SVL(Tags.RectnumTag, new SObject[] {real, imag});
        }
        public static SByteVL makeFlonum(double num) {
            byte[] bvec = new byte[12];
            byte[] numbytes = System.BitConverter.GetBytes(num);
            for (int i = 0; i < numbytes.Length; ++i) {
                bvec[i + 4] = numbytes[i];
            }
            return new SByteVL(Tags.FlonumTag, bvec);
        }
        public static SByteVL makeFlonum(long num) {
            byte[] bvec = new byte[12];
            byte[] numbytes = System.BitConverter.GetBytes(num);
            for (int i = 0; i < numbytes.Length; ++i) {
                bvec[i + 4] = numbytes[i];
            }
            return new SByteVL(Tags.FlonumTag, bvec);
        }

        public static SByteVL makeCompnum(double real, double imag) {
            byte[] bvec = new byte[20];
            byte[] realbytes = System.BitConverter.GetBytes(real);
            byte[] imagbytes = System.BitConverter.GetBytes(imag);
            for (int i = 0; i < realbytes.Length; ++i) {
                bvec[4 + i] = realbytes[i];
            }
            for (int j = 0; j < imagbytes.Length; ++j) {
                bvec[12 + j] = imagbytes[j];
            }
            return new SByteVL(Tags.CompnumTag, bvec);
        }
        public static SByteVL makeCompnum(long real, long imag) {
            byte[] bvec = new byte[20];
            byte[] realbytes = System.BitConverter.GetBytes(real);
            byte[] imagbytes = System.BitConverter.GetBytes(imag);
            for (int i = 0; i < realbytes.Length; ++i) {
                bvec[4 + i] = realbytes[i];
            }
            for (int j = 0; j < imagbytes.Length; ++j) {
                bvec[12 + j] = imagbytes[j];
            }
            return new SByteVL(Tags.CompnumTag, bvec);
        }
        public static SChar makeChar(int val) {
            return SChar.makeChar(val);
        }
        public static SPair makePair(SObject first, SObject rest) {
            return new SPair(first, rest);
        }
        public static SPair makePairReversed(SObject rest, SObject first) {
            return new SPair(first, rest);
        }
        public static SVL makeVector(SObject[] objs) {
            return new SVL(Tags.VectorTag, objs);
        }
        public static SVL makeVector(int size, SObject fill) {
            return new SVL(Tags.VectorTag, size, fill);
        }
        
        public static SByteVL makeByteVector(int size, int fill) {
            return new SByteVL
                (Tags.ByteVectorTag, size, (byte)fill);
        }

        public static SVL makeSymbol(string str) {
            SVL sym = (SVL) Reg.internedSymbols[str];
            if (sym == null) {
                SObject name = makeString(str);
                SObject hash = makeFixnum(Number.stringHash(str));
                sym = new SVL(Tags.SymbolTag, 
                            new SObject[]{name, hash, SObject.Null});
                Reg.internedSymbols[str] = sym;
            }
            return sym;
        }

        public static SByteVL makeString(int length, char fill) {
            return new SByteVL
                (Tags.StringTag, length, (byte)fill);
        }
        public static SByteVL makeString(string s) {
            byte[] chars = new byte[s.Length];
            for (int i = 0; i < chars.Length; ++i) {
                chars[i] = (byte)s[i];
            }
            return new SByteVL(Tags.StringTag, chars);
        }

        public static SObject wrap(int n) {
            return makeNumber(n);
        }
        public static SObject wrap(string s) {
            return makeString(s);
        }
        public static SObject wrap(bool b) {
            return b ? SObject.True : SObject.False;
        }
    }

    // -------------------------------------------
    // SObject
    // -------------------------------------------
    public abstract class SObject {
        public static readonly SImmediate True 
            = new SImmediate("#t");
        public static readonly SImmediate False 
            = new SImmediate("#f");
        public static readonly SImmediate Null
            = new SImmediate("()");
        public static readonly SImmediate Eof 
            = new SImmediate("#<eof>");
        public static readonly SImmediate Unspecified
            = new SImmediate("#<unspecified>");
        public static readonly SImmediate Undefined
            = new SImmediate("#<undefined>");
        
        /* General Predicates */
        public virtual bool isNumber() {return false;}
        public virtual bool isFixnum() {return false;}
        public virtual bool isFlonum() {return false;}
        public virtual bool isBignum() {return false;}
        public virtual bool isRatnum() {return false;}
        public virtual bool isRectnum() {return false;}
        public virtual bool isCompnum() {return false;}

        public virtual bool isComplex() {return false;}
        public virtual bool isReal() {return false;}
        public virtual bool isRational() {return false;}
        public virtual bool isInteger() {return false;}
        public virtual bool isExact() {return false;}
        public virtual bool isInexact() {return false;}

        public virtual bool isImmediate() {return false;}
        public virtual bool isSymbol() {return false;}
        public virtual bool isChar() {return false;}
        public virtual bool isPair() {return false;}
        public virtual bool isProcedure() {return false;}
        public virtual bool isCell() { return false; }

        public virtual bool isVectorLike() {return false;}
        public virtual bool isVector() {return false;}
        public virtual bool isPort() {return false;}
        public virtual bool isStructure() {return false;}

        public virtual bool isByteVectorLike() {return false;}
        public virtual bool isByteVector() {return false;}
        public virtual bool isString() {return false;}
        
        public override string ToString() {
            System.IO.StringWriter sw = new System.IO.StringWriter(); // new StringBuilder(16, 4000));
            // try {
                this.write(sw);
            //} catch (Exception e) {
            //    System.Console.WriteLine("Exception:");
            //    System.Console.WriteLine(e);
            //}
            return sw.ToString();
        }
        public virtual void write(TextWriter w) {
            w.Write("#<object:");
            w.Write(this.GetType());
            w.Write(">");
        }
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

        // ------------------
        public override bool isImmediate() { return true; }
    }

    // -------------------------------------------
    // SChar
    // -------------------------------------------
    public sealed class SChar : SObject {
        public static readonly int CHAR_COUNT = 256;
        public static readonly SChar[] characters = 
            new SChar[CHAR_COUNT];

        public char val;
        private SChar(char c) {
            this.val = c;
        }
        static SChar() {
            for (int i = 0; i < CHAR_COUNT; ++i) {
                characters[i] = new SChar((char)i);
            }
        }
        public static SChar makeChar(int c) {
            if (c >= 0 && c < CHAR_COUNT) {
                return characters[c];
            } else {
                Exn.internalError("not a valid char");
                return new SChar((char)c);
            }
        }
        public override void write(TextWriter w) {
            w.Write("#\\");
            w.Write(val);
        }

        // ------------------
        public override bool isChar() { return true; }
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
     *     - compnum = SVL/compnum
     */

    // -------------------------------------------
    // SFixnum
    // -------------------------------------------
    public sealed class SFixnum : SObject {
        public readonly int value;
        public static readonly SFixnum[] pool;
        public static readonly int maxPreAlloc = 16000;
        public static readonly int MAX = (1 << 29) - 1;
        public static readonly int MIN = -(1 << 29);
        public static readonly uint BITS = 29;

        // Stores numbers -maxPreAlloc to +maxPreAlloc
        //          0 -> maxPreAlloc
        // -maxPreAlloc -> 0
        static SFixnum() {
            pool = new SFixnum[2 * maxPreAlloc + 1];
            for (int i = -maxPreAlloc; i <= maxPreAlloc; i++)
                pool[i + maxPreAlloc] = new SFixnum(i);                   
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
        public static bool inFixnumRange(int n) {
            int highbits = n >> 29;
            return (highbits == 0 || highbits == -1);
        }
        public static SFixnum makeFixnum(int val) {
//            if (val == 21845) {
//                throw new Exception();
//            }
            if (val >= -maxPreAlloc && val <= maxPreAlloc) {
                return pool[val + maxPreAlloc];
            } else
                return new SFixnum(val);
        }
        // ----------------
        
        public override bool isNumber() { return true; }
        public override bool isFixnum() { return true; }
        public override bool isComplex() { return true; }
        public override bool isReal() { return true; }
        public override bool isRational() { return true; }
        public override bool isInteger() { return true; }
        public override bool isExact() { return true; }
    }


    // -------------------------------------------
    // SVL (vector-like)
    // -------------------------------------------
    public sealed class SVL : SObject {
        public int tag;
        public readonly SObject[] elements;
        
        public SVL(int tag, int size, SObject fill) {
            this.tag = tag;
            this.elements = new SObject[size];
            for (int i = 0; i < size; ++i) {elements[i] = fill;};
        }
        public SVL(int tag, SObject[] vec) {
            this.tag = tag;
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
        
        // -------------------

        public override bool isNumber() {
            return this.isRatnum() || this.isRectnum();
        }
        public override bool isRatnum() {
            return this.tag == Tags.RatnumTag;
        }
        public override bool isRectnum() {
            return this.tag == Tags.RectnumTag;
        }
        public override bool isComplex() {
            return this.isRatnum() || this.isRectnum();
        }
        public override bool isReal() {
            return this.isRatnum();
        }
        public override bool isRational() {
            return this.isRatnum();
        }
        public override bool isInteger() {
            return this.isRatnum() && this.isIntegralRatnum();
        }
        public override bool isExact() {
            return this.isRatnum() || this.isRectnum();
        }
        
        public override bool isVectorLike() {
            return true;
        }
        public override bool isVector() {
            return this.tag == Tags.VectorTag;
        }
        public override bool isPort() {
            return this.tag == Tags.PortTag;
        }
        public override bool isStructure() {
            return this.tag == Tags.StructureTag;
        }
        public override bool isSymbol() {
            return this.tag == Tags.SymbolTag;
        }

        private bool isIntegralRatnum() {
            bool b = this.elements[1] == SFixnum.makeFixnum(1);
            //Exn.debug.WriteLine("isIntegralRatnum({0} / {1}): {2}",
            //                    this.elements[0],
            //                    this.elements[1],
            //                    b);
            return b;
        }

        // -----
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
    }
    
    // -------------------------------------------
    // SByteVL (bytevector-like)
    // -------------------------------------------
    public sealed class SByteVL : SObject {
        public int tag;
        public readonly byte[] elements;
        public static System.Text.Encoding stringEncoding 
            = new System.Text.ASCIIEncoding();

        public SByteVL(int tag, int size, byte fill) {
            this.tag = tag;
            this.elements = new byte[size];
            for (int i = 0; i < size; ++i) {elements[i] = fill;}
        }
        public SByteVL(int tag, byte[] vec) {
            this.tag = tag;
            this.elements = vec;
        }
                
        public int length() {
            return elements.Length;
        }

        public SFixnum elementAt(int index) {
            return SFixnum.makeFixnum(elements[index]);
        }

        public void setElementAt(int index, int element) {
            elements[index] = (byte)element;
        }

        public byte getByte(int index) {
            return elements[index];
        }
        public void setByte(int index, int b) {
            elements[index] = (byte)b;
        }
        
		public short getInt16(int index) {
            return System.BitConverter.ToInt16(elements, index*2);
        }
		public void setInt16(int index, int s) {
            byte[] bytes = System.BitConverter.GetBytes((short)s);
            elements[index*2] = bytes[0];
            elements[index*2 + 1] = bytes[1];
        }
        
        public ushort getUInt16(int index) {
            return System.BitConverter.ToUInt16(elements, index*2);
        }
        public void setUInt16(int index, int s) {
            byte[] bytes = System.BitConverter.GetBytes((ushort)s);
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

        public void fill(byte b) {
            for (int i = 0; i < elements.Length; ++i) {
                elements[i] = b;
            }
        }
        
        // unsafeAsDouble: interprets bytes as the bit representation 
        //     of a double value
        public double unsafeAsDouble(int steps) {
            return System.BitConverter.ToDouble(elements, 4 + steps * 8); // steps * sizeof(double)) + offset
        }
        public void unsafeSetDouble(double d) {
            byte[] b = System.BitConverter.GetBytes(d);
            for (int i = 0; i < 8; ++i) {
                elements[i+4] = b[i];
            }
        }

        // ----------------------

        public override bool isNumber() {
            return this.isBignum() || this.isFlonum() || this.isCompnum();
        } 
        public override bool isExact() {
            return this.isBignum();
        }
        public override bool isInexact() {
            return this.isFlonum() || this.isCompnum();
        }
        public override bool isBignum() {
            return this.tag == Tags.BignumTag;
        }
        public override bool isFlonum() {
            return this.tag == Tags.FlonumTag;
        }
        public override bool isCompnum() {
            return this.tag == Tags.CompnumTag;
        }
        public override bool isComplex() {
            return this.isBignum() || this.isFlonum() || this.isCompnum();
        }
        public override bool isReal() {
            return this.isBignum() || this.isFlonum();
        }
        public override bool isRational() {
            return this.isBignum() || this.isFlonum();
        }
        public override bool isInteger() {
            return this.isBignum() || (this.isFlonum() && this.isIntegralFlonum());
        }
        public override bool isByteVectorLike() {
            return true;
        }
        public override bool isByteVector() {
            return this.tag == Tags.ByteVectorTag;
        }
        public override bool isString() {
            return this.tag == Tags.StringTag;
        }

        // -----
        private bool isIntegralFlonum() {
            double v = this.unsafeAsDouble(0);
            // Exn.debug.Write("Testing for integral double " + v);
            bool b = (Math.Ceiling(v) == Math.Floor(v));
            // Exn.debug.WriteLine(b ? " yes" : " no");
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
                } else if (p.rest == SObject.Null) {
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

        // ------------------
        public override bool isPair() { return true; }
    }
    
    // -------------------------------------------
    // Procedure
    // -------------------------------------------
    public sealed class Procedure : SObject {
        public int tag;
        public CodeVector entrypoint;
        public SObject[] rib;
        public SVL constantvector;
        public SObject[] constants;

        public Procedure(CodeVector entrypoint, 
                         SObject constantvector,
                         SObject[] rib) {
            this.tag = Constants.PROC_TAG;
            this.entrypoint = entrypoint;
            this.constantvector = (SVL) constantvector;
            this.constants = this.constantvector.elements;
            this.rib = rib;
        }

        public Procedure(CodeVector entrypoint, SObject constantvector)
            : this(entrypoint, constantvector, new SObject[]{}) {}
        public Procedure(CodeVector entrypoint)
            : this(entrypoint, Factory.makeVector(1, SObject.False), new SObject[0]) {}

        public void setCode(SObject code) {
            if (code is CodeVector) {
                this.entrypoint = (CodeVector) code;
            } else if (code == SObject.False) {
                this.entrypoint = CodeVector.NoCode;
            } else {
                Exn.internalError("procedure-set! 0 called, not a codevector: " + code);
            }
        }
        public SObject getCode() {
            if (this.entrypoint is DataCodeVector) {
                return ((DataCodeVector)this.entrypoint).datum;
            } else { 
                return this.entrypoint;
            }
        }
        
        public void setConstants(SVL constantvector) {
            this.constantvector = constantvector;
            this.constants = constantvector.elements;
        }
        
        /** lookup
         * Look up (rib, slot) in lexical environment
         */    
        public SObject lookup(int ri, int slot) {
            SObject[] rib = this.rib;
            while (ri > 0) {
                rib = ((Procedure)rib[0]).rib;
                ri --;
            }
            return rib[slot];
        }
        
        /** update
         * Mutate a lexically bound variable at (rib, slot) to new_value
         */
        public void update(int ri, int slot, SObject newValue) {
            SObject[] rib = this.rib;
            while (ri > 0) {
                rib = ((Procedure)rib[0]).rib;
                ri --;
            }
            rib[slot] = newValue;
        }

        public override bool isProcedure() { return true; }

        public override void write(TextWriter w) {
            w.Write("#<PROCEDURE: ");
            if (this.constants.Length >= 1) {
                SObject d = this.constants[0];
                if (d is SVL) {
                    SVL dd = (SVL) d;
                    if (dd.elements != null && dd.elements.Length >= 1) {
                        w.Write(((SVL)d).elements[0]);
                        w.Write("=");
                    }
                }
            }
            w.Write(entrypoint.name());
            w.Write(">");
        }
    }

    // -------------------------------------------
    // CodeVectors and ConstantVectors
    // -------------------------------------------
    public abstract class CodeVector : SObject {
        public static readonly CodeVector NoCode = new DataCodeVector(SObject.False);

        /** call
         * Given a jump index (0 for entry point, NOT the same as label number),
         * start executing at the label corresponding to that code.
         */
        public abstract void call(int jump_index);
        
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

        public override string ToString() {
            return "CodeVector " + name();
        }
    }

    public class DataCodeVector : CodeVector {
        public SObject datum;

        public DataCodeVector(SObject datum) {
            this.datum = datum;
        }

        public override void call(int ignored) {
            throw new Exception("not a real codevector");
        }
    }
}

using System;
using System.Collections;
using System.IO;
using System.Text;
using Scheme.RT;
using Scheme.Rep;

namespace Scheme.Rep {
    
    // -------------------------------------------
    // Factory
    // -------------------------------------------
    public sealed class Factory {
        /* Constants */
        public static readonly double PositiveInfinity = Double.PositiveInfinity;
        public static readonly double NegativeInfinity = Double.NegativeInfinity;
        public static readonly double Nan = Double.NaN;

        // ===================
        //   Immediates
        // ===================
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
        
        // Used as "return value" in escaping procedures
        public static readonly SImmediate Impossible
            = new SImmediate("#<IMPOSSIBLE>");

        // ===================
        //   Booleans
        // ===================

        public static SImmediate makeBoolean(bool b) {
            return b ? Factory.True : Factory.False;
        }

        // ===================
        //   Numbers
        // ===================
        public static SFixnum makeFixnum(int num) {
            return SFixnum.makeFixnum(num);
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
        public static SByteVL makeBignum(int num) {
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

        // ===================
        //   Characters
        // ===================

        public static SChar makeChar(int val) {
            return SChar.makeChar(val);
        }

        // ===================
        //   Pairs/Vectors
        // ===================

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
        
        // ===================
        //  Symbols
        // ===================

        /* Symbols may only be created until (go ...) is called in 
         * the Scheme program. After that, symbol creation must be 
         * done in Scheme code.
         */
        private static Hashtable internedSymbols = new Hashtable();
        private static bool allowInternSymbol = true;

        public static SVL internSymbol(string str) {
            if (!allowInternSymbol) {
                Exn.internalError("interning symbols no longer allowed");
                return null;
            }
            SVL sym = (SVL) internedSymbols[str];
            if (sym == null) {
                sym = makeSymbolUninterned(str);
                internedSymbols[str] = sym;
            }
            return sym;
        }

        public static SVL makeSymbolUninterned(string str) {
            SObject name = makeString(str);
            SObject hash = makeNumber(stringHash(str));
            return new SVL(Tags.SymbolTag, new SObject[]{name, hash, Factory.Null});
        }
        
        // Duplicates string-hash in Lib/Common/string.sch
        private static int stringHash(string str) {
            /*
              (define (string-hash string)
                (define (loop s i h)
                  (if (< i 0)
                      h
                      (loop s
                            (- i 1)
                            (logand 65535 (+ (char->integer (string-ref s i)) h h h)))))
                (let ((n (string-length string)))
                  (loop string (- n 1) n)))
            */
            int hash = str.Length;
            for (int i = str.Length - 1; i >= 0; --i) {
                hash = 65535 & (str[i] + hash + hash + hash);
            }
            return hash;
        }

        public static SObject stopSymbolInterning() {
            SObject syms = Factory.Null;
            foreach (SObject arg in internedSymbols.Values) {
                syms = Factory.makePair(arg, syms);
            }
            internedSymbols = null;
            allowInternSymbol = false;
            return syms;
        }

        // ===================
        //   Strings, ByteVL
        // ===================

        public static SByteVL makeString(int length, char fill) {
            return new SByteVL(Tags.StringTag, length, (byte)fill);
        }
        public static SByteVL makeString(string s) {
            byte[] chars = new byte[s.Length];
            for (int i = 0; i < chars.Length; ++i) {
                chars[i] = (byte)s[i];
            }
            return new SByteVL(Tags.StringTag, chars);
        }

        public static SByteVL makeByteVector(int size, int fill) {
            return new SByteVL
                (Tags.ByteVectorTag, size, (byte)fill);
        }

        // ============
        //   Lists
        // ============
        public static SObject copyList(SObject list) {
            if (list == Factory.Null) {
                return list;
            } else {
                SPair copyhead = new SPair(null, null);
                SPair copyIntoCdr = copyhead;
                while (list is SPair) {
                    SPair p = (SPair) list;
                    SPair newpair = new SPair(p.first, null);
                    copyIntoCdr.rest = newpair;
                    copyIntoCdr = newpair;
                    list = p.rest;
                }
                copyIntoCdr.rest = list;
                return copyhead.rest;
            }
        }
        
        public static SObject arrayToList(SObject[] array, int start) {
            SObject list = Factory.Null;
            for (int i = start; i < array.Length; ++i) {
                list = Factory.makePair(array[i], list);
            }
            return list;
        }

        public static SObject makeForeign(object value) {
            return new Foreign(value);
        }
        public static SObject makeForeignF(object value) {
            if (value == null) {
                return Factory.False;
            } else {
                return new Foreign(value);
            }
        }
        // =================================================
        // Quick and Simple
        // =================================================

        public static SObject wrap(int n) {
            return makeNumber(n);
        }
        public static SObject wrap(long n) {
            return makeNumber(n);
        }
        public static SObject wrap(string s) {
            return makeString(s);
        }
        public static SObject wrap(bool b) {
            return b ? Factory.True : Factory.False;
        }
        public static SObject wrap(SObject[] items) {
            return makeVector(items);
        }
        public static SObject wrapAsList(SObject[] items) {
            return arrayToList(items, 0);
        }
    }
}

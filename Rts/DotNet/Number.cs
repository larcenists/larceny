using System;
using Scheme.RT;

namespace Scheme.Rep {
public class Number {
    // ============
    //   BIGNUMS
    // ============
    
    public static readonly int BIGIT_BITS = 16;
    public static readonly int BYTES_PER_BIGIT = 2;
    public static readonly uint BIGIT_MASK = 0xFFFF;
    public static readonly int BIGITS_PER_FIXNUM = 2;
    public static readonly int BIGITS_PER_LONG = 4;

    public static readonly int BIGNUM_POSITIVE = 0;
    public static readonly int BIGNUM_NEGATIVE = 1;
    
	#if !BIG_ENDIAN
    public static readonly int BIGNUM_LENGTH_OFFSET = 0;
    public static readonly int BIGNUM_SIGN_OFFSET = 1;
	#else
	public static readonly int BIGNUM_LENGTH_OFFSET = 1;
    public static readonly int BIGNUM_SIGN_OFFSET = 0;
	#endif
    public static readonly int BIGNUM_DATA_OFFSET = 2;

    public static SByteVL makeBignum(ulong value, bool positive) {
        int bigitc = 0;
        for (ulong v = value; v != 0; v = v >> BIGIT_BITS) {
            bigitc ++;
        }

        SByteVL b = allocBignum(bigitc);
        if (positive) {
            setBignumSign(b, BIGNUM_POSITIVE);
        } else {
            setBignumSign(b, BIGNUM_NEGATIVE);
        }
        
        for (int i = 0; i < bigitc; ++i) {
            bignumSet(b, i, (int)(value & BIGIT_MASK));
            value = value >> BIGIT_BITS;
        }
        return b;
    }

    public static SByteVL makeBignum(short[] bigits, bool positive) {
        int bigitc = bigits.Length;
        SByteVL b = allocBignum(bigitc);
        if (positive) {
            setBignumSign(b, BIGNUM_POSITIVE);
        } else {
            setBignumSign(b, BIGNUM_NEGATIVE);
        }
        for (int i = 0; i < bigitc; ++i) {
            bignumSet(b, i, bigits[i]);
        }
        return b;
    }

    public static SByteVL allocBignum(int bigitc) {
        int length = (bigitc*BYTES_PER_BIGIT + 3) & ~(int)3;
        SByteVL b = new SByteVL(Tags.BignumTag, length + 4, 0);
        setBignumLengthInBigits(b, bigitc);
        return b;
    }

    public static int getBignumLength(SByteVL b) {
        int bigitc = b.getUInt16(BIGNUM_LENGTH_OFFSET);
        return bigitc;
    }
    public static void setBignumLengthInBigits(SByteVL b, int bigitc) {
        int wordc = (bigitc + 1) >> 1;
        b.setUInt16(BIGNUM_LENGTH_OFFSET, wordc);
    }

    public static int getBignumSign(SByteVL b) {
        return b.getUInt16(BIGNUM_SIGN_OFFSET);
    }
    public static void setBignumSign(SByteVL b, int sign) {
        b.setUInt16(BIGNUM_SIGN_OFFSET, sign);
    }

    public static bool isZeroBignum(SByteVL b) {
        return b.getUInt16(BIGNUM_LENGTH_OFFSET) == 0;
    }

	#if !BIG_ENDIAN 
	// little endian
    public static void bignumSet(SByteVL b, int index, int value) {
        b.setInt16(index + BIGNUM_DATA_OFFSET, value);
    }
    public static int bignumRef(SByteVL b, int index) {
        return b.getInt16(index + BIGNUM_DATA_OFFSET);
    }
	#else 
	// big endian
	public static void bignumSet(SByteVL b, int index, int value) {
		int x;
		if ((index & 1) == 0) {
			x = index + 3;
		} else {
			x = index + 1;
		}
		b.setInt16(x, value);
	}
	public static int bignumRef(SByteVL b, int index) {
		int x;
		if ((index & 1) == 0) {
			x = index + 3;
		} else {
			x = index + 1;
		}
		return b.getInt16(x);
	}
	#endif

    // ============
    //   Complex
    // ============
    
    public static SObject rectRealPart(SVL n) {
        return n.elements[0];
    }
    public static SObject rectImagPart(SVL n) {
        return n.elements[1];
    }

    public static SObject compRealPart(SByteVL n) {
        return Factory.makeFlonum(n.unsafeAsDouble(0));
    }
    public static SObject compImagPart(SByteVL n) {
        return Factory.makeFlonum(n.unsafeAsDouble(1));
    }
}
}

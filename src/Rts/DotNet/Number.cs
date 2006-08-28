using System;
using Scheme.RT;

namespace Scheme.Rep {
public class Number {
    // ============
    //   BIGNUMS
    // ============
    
    public const int BIGIT_BITS = 16;
    public const int BYTES_PER_BIGIT = 2;
    public const ushort BIGIT_MASK = 0xFFFF;
    public const int BIGITS_PER_FIXNUM = 2;
    public const int BIGITS_PER_LONG = 4;

    public const ushort BIGNUM_POSITIVE = 0;
    public const ushort BIGNUM_NEGATIVE = 1;
    
	#if !BIG_ENDIAN
    public const int BIGNUM_LENGTH_OFFSET = 0;
    public const int BIGNUM_SIGN_OFFSET = 1;
	#else
    public const int BIGNUM_LENGTH_OFFSET = 1;
    public const int BIGNUM_SIGN_OFFSET = 0;
	#endif
    public const int BIGNUM_DATA_OFFSET = 2;

    public static SByteVL makeBignum(ulong value, bool positive) {
        ushort bigitc = 0;
        for (ulong v = value; v != 0; v = v >> BIGIT_BITS) {
            bigitc ++;
        }

        SByteVL b = allocBignum(bigitc);
        setBignumSign (b, positive);
        
        for (int i = 0; i < bigitc; ++i) {
            bignumSet (b, i, (ushort)(value & BIGIT_MASK));
            value = value >> BIGIT_BITS;
        }
        return b;
    }

    public static SByteVL makeBignum(short[] bigits, bool positive) {
        int bigitc = bigits.Length;
        if (bigitc > UInt16.MaxValue)
            throw new Exception ("Internal error:  bignum too large");
        else {

            SByteVL b = allocBignum((ushort) bigitc);
            setBignumSign (b, positive);

            // Bignums use a sign + magnitude representation,
            // so the digits in a bignum are positive numbers in the
            // half-open interval [0, 65536)

            // But someone, somewhere wants to send us the digits in
            // a vector of signed shorts (in two's complement), which
            // lie in the open interval [-32768, 32767) so we have to convert.

            // For digits in the range [0, 32767), the two's complement
            // and the unsigned magnitude are the same, but the digits in the
            // range [32768, 65536) are negative numbers in two's complement.
            // We cannot simply add 65536 to the digit because the types don't
            // match, and we cannot simply negate the digit and subtract because
            // the negative number -32768 has no positive number in the range
            // of representable shorts.  Therefore, we add one to the digit before
            // negating it, convert that to a unsigned short, then subtract it
            // from the Max unsigned short.

            // Why not simply cast the thing?  That isn't a technically correct
            // operation because it *assumes* that the underlying representation
            // of shorts is two's complement of the appropriate width.  The way we
            // do it here doesn't depend on the machine representation.

            for (int i = 0; i < bigitc; ++i) {
                short bigit = bigits[i];
                bignumSet(b, i, (bigit < 0) ? (ushort)(UInt16.MaxValue - ((ushort)(- (bigit + 1)))) : (ushort) bigit);
                }

            return b;
        }
    }

    public static SByteVL allocBignum(ushort bigitc) {
        int length = (bigitc*BYTES_PER_BIGIT + 3) & ~(int)3;
        SByteVL b = new SByteVL(Tags.BignumTag, length + 4, 0);
        setBignumLengthInBigits(b, bigitc);
        return b;
    }

    // getBignumLength returns the number of data words in the bignum
    public static ushort getBignumLength(SByteVL b) {
        return b.getUInt16(BIGNUM_LENGTH_OFFSET);
    }
    public static void setBignumLengthInBigits(SByteVL b, ushort bigitc) {
        ushort wordc = (ushort) ((bigitc + 1) >> 1);
        b.setUInt16(BIGNUM_LENGTH_OFFSET, wordc);
    }

    public static bool getBignumSign(SByteVL b) {
        return b.getUInt16(BIGNUM_SIGN_OFFSET) == BIGNUM_POSITIVE;
    }
    public static void setBignumSign(SByteVL b, bool sign) {
        b.setUInt16(BIGNUM_SIGN_OFFSET, sign ? BIGNUM_POSITIVE : BIGNUM_NEGATIVE);
    }

    public static bool isZeroBignum(SByteVL b) {
        return b.getUInt16(BIGNUM_LENGTH_OFFSET) == 0;
    }

        #if !BIG_ENDIAN
        // little endian
    public static void bignumSet(SByteVL b, int index, ushort value) {
        b.setUInt16(index + BIGNUM_DATA_OFFSET, value);
    }
    public static ushort bignumRef(SByteVL b, int index) {
        return b.getUInt16(index + BIGNUM_DATA_OFFSET);
    }
        #else
        // big endian
        public static void bignumSet(SByteVL b, int index, ushort value) {
                int x;
                if ((index & 1) == 0) {
                        x = index + 3;
                } else {
                        x = index + 1;
                }
                b.setUInt16(x, value);
        }
        public static ushort bignumRef(SByteVL b, int index) {
                int x;
                if ((index & 1) == 0) {
                        x = index + 3;
                } else {
                        x = index + 1;
                }
                return b.getUInt16(x);
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

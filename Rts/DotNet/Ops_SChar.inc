// Ops for SChar

        PREDICATE_OVERRIDE_TRUE(isChar, charp)

        OP2_CHAIN(char_lt)
        OP2_CHAIN(char_le)
        OP2_CHAIN(char_gt)
        OP2_CHAIN(char_ge)
        OP2_CHAIN(char_equals)
        OP2_OVERRIDE_REVERSED(char_lt, SChar) {
            return Factory.wrap(arg1.val < this.val);
        }
        OP2_OVERRIDE_REVERSED(char_le, SChar) {
            return Factory.wrap(arg1.val <= this.val);
        }
        OP2_OVERRIDE_REVERSED(char_gt, SChar) {
            return Factory.wrap(arg1.val > this.val);
        }
        OP2_OVERRIDE_REVERSED(char_ge, SChar) {
            return Factory.wrap(arg1.val >= this.val);
        }
        OP2_OVERRIDE_REVERSED(char_equals, SChar) {
            return Factory.wrap(arg1.val == this.val);
        }

        OP1_OVERRIDE(char2integer) {
            return Factory.wrap(this.val);
        }

        OP2_OVERRIDE_REVERSED(make_string, SFixnum) {
            return Factory.makeString(arg1.value, this.val);
        }

        // Special Operations
        SPECIALOP2_OVERRIDE_REVERSED_CASE(eqvp_not_eq, char, SChar) {
            Reg.Result = Factory.wrap(arg1.val == this.val);
        }

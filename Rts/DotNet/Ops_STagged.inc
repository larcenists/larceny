// Ops for STagged (ops common to SVL, SByteVL)

        OP1_OVERRIDE(typetag) { return Factory.wrap(this.tag); }
        OP2_CHAIN(typetag_set)


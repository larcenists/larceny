// Ops for STagged (ops common to SVL, SByteVL)

        OP1_OVERRIDE(typetag) { return Factory.makeFixnum (this.tag); }
        OP2_CHAIN(typetag_set)


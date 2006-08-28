// Ops for SPair

        PREDICATE_OVERRIDE_TRUE(isPair, pairp)
        OP1_OVERRIDE(cell_ref) { return this.first; }
        OP2_OVERRIDE(cell_set) {
            this.first = arg2;
            return Factory.Unspecified;
        }
        OP2_OVERRIDE(cell_set_nwb) {
            this.first = arg2;
            return Factory.Unspecified;
        }
        OP1_OVERRIDE(car) { return this.first; }
        OP1_OVERRIDE(car_pair) { return this.first; }
        OP1_OVERRIDE(cdr) { return this.rest; }
        OP1_OVERRIDE(cdr_pair) { return this.rest; }

        OP2_OVERRIDE(set_car) { this.first = arg2; return Factory.Unspecified; }
        OP2_OVERRIDE(set_car_pair) { this.first = arg2; return Factory.Unspecified; }
        OP2_OVERRIDE(set_cdr) { this.rest = arg2; return Factory.Unspecified; }
        OP2_OVERRIDE(set_cdr_pair) { this.rest = arg2; return Factory.Unspecified; }

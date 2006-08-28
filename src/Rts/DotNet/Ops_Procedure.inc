// Ops for Procedure

        PREDICATE_OVERRIDE_TRUE(isProcedure, procedurep)
        OP1_OVERRIDE(procedure_length) {
            return Factory.makeNumber (this.rib.Length + 2);
        }
        OP2_CHAIN(procedure_ref)
        OP3_CHAIN(procedure_set)

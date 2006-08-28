.SUFFIXES: .sch .fasl $(SUFFIXES)

.sch.fasl:
	echo '(begin (compile-file "$<") (exit 0))' | larceny

all: dummy.fasl dynamic.fasl grow.fasl gcbench0.fasl lattice.fasl \
	nbody.fasl nboyer.fasl nucleic2.fasl permsort.fasl sboyer.fasl


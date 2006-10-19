ASCIIDOC_HOME=../../../vendor/asciidoc-8.0.0
ASCIIDOC=asciidoc
A2X=JAVACMD=$(shell which java) a2x

STD_ASCIIDOC_OPTS=--conf-file=larcenydoc.conf --section-numbers
STD_XSLTPROC_OPTS=--stringparam section.autolabel 1
STD_A2X_OPTS=--asciidoc-opts="${STD_ASCIIDOC_OPTS}" --xsltproc-opts="${STD_XSLTPROC_OPTS}" --doctype=article --destination-dir=. 

OTHERFILES=larcenydoc.conf $(wildcard UserManual/*.txt)
LNOTEFILES=larcenydoc.conf $(wildcard LarcenyNotes/*.txt)

all: docs notes sourceindex.html builddate.html

docs:  user-manual.pdf user-manual.html user-manual.chunked/index.html user-manual-alt.html sourceindex.html

notes: larceny-notes.html larceny-notes-alt.html

user-manual.pdf: UserManual/user-manual.txt ${OTHERFILES}
	${A2X} ${STD_A2X_OPTS}  --format=pdf UserManual/user-manual.txt 

user-manual.html: UserManual/user-manual.txt ${OTHERFILES}
	${A2X} ${STD_A2X_OPTS}  --format=xhtml UserManual/user-manual.txt 

user-manual-alt.html: UserManual/user-manual.txt ${OTHERFILES}
	${ASCIIDOC} ${STD_ASCIIDOC_OPTS}  --backend=xhtml11 --out-file=$@ UserManual/user-manual.txt

user-manual.chunked/index.html: UserManual/user-manual.txt ${OTHERFILES}
	${A2X} ${STD_A2X_OPTS}  --format=chunked UserManual/user-manual.txt 

# Don't do this one unless we find a way to conditionally include on 
# PDFs (would also need to shift the relative headings for each note
# down to a chapter...
larceny-notes.pdf: LarcenyNotes/larceny-notes.txt ${LNOTEFILES}
	${A2X} ${STD_A2X_OPTS}  --format=pdf LarcenyNotes/larceny-notes.txt 

larceny-notes.html: LarcenyNotes/larceny-notes.txt ${LNOTEFILES}
	${A2X} ${STD_A2X_OPTS}  --format=xhtml LarcenyNotes/larceny-notes.txt 

larceny-notes-alt.html: LarcenyNotes/larceny-notes.txt ${LNOTEFILES}
	${ASCIIDOC} ${STD_ASCIIDOC_OPTS}  --backend=xhtml11 --out-file=$@ LarcenyNotes/larceny-notes.txt

larceny-notes.chunked/index.html: LarcenyNotes/larceny-notes.txt ${LNOTEFILES}
	${A2X} ${STD_A2X_OPTS}  --format=chunked LarcenyNotes/larceny-notes.txt 





sourceindex.html: ${OTHERFILES}
	echo "<HTML><HEAD><TITLE>UserManual</TITLE><BODY><UL>"                     > sourceindex.html
	echo $(foreach file,${OTHERFILES},"<LI><A HREF=$(file)>$(file)</A></LI>") >> sourceindex.html
	echo "</UL></BODY></HEAD></HTML>"                                         >> sourceindex.html

builddate.html: alwaysdo
	echo "<HTML><HEAD><TITLE>UserManual</TITLE><BODY>"                         > builddate.html
	echo "Built on "                                                          >> builddate.html
	date                                                                      >> builddate.html
	echo "</BODY></HEAD></HTML>"                                              >> builddate.html

# Empty target so that things like builddate.html are always remade
alwaysdo:

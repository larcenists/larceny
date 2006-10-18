ASCIIDOC_HOME=../../../vendor/asciidoc-8.0.0
ASCIIDOC=asciidoc
A2X=JAVACMD=$(shell which java) a2x

STD_ASCIIDOC_OPTS=--conf-file=larcenydoc.conf --section-numbers
STD_XSLTPROC_OPTS=--stringparam section.autolabel 1
STD_A2X_OPTS=--asciidoc-opts="${STD_ASCIIDOC_OPTS}" --xsltproc-opts="${STD_XSLTPROC_OPTS}" --doctype=article --destination-dir=. 

OTHERFILES=$(wildcard UserManual/*.txt)

all: docs sourceindex.html builddate.html

docs:  user-manual.pdf user-manual.html user-manual.chunked/index.html user-manual-alt.html sourceindex.html

user-manual.pdf: UserManual/user-manual.txt ${OTHERFILES}
	${A2X} ${STD_A2X_OPTS}  --format=pdf UserManual/user-manual.txt 

user-manual.html: UserManual/user-manual.txt ${OTHERFILES}
	${A2X} ${STD_A2X_OPTS}  --format=xhtml UserManual/user-manual.txt 

user-manual-alt.html: UserManual/user-manual.txt ${OTHERFILES}
	${ASCIIDOC} ${STD_ASCIIDOC_OPTS}  --backend=xhtml11 --out-file=$@ UserManual/user-manual.txt

user-manual.chunked/index.html: UserManual/user-manual.txt ${OTHERFILES}
	${A2X} ${STD_A2X_OPTS}  --format=chunked UserManual/user-manual.txt 

sourceindex.html: ${OTHERFILES}
	echo "<HTML><HEAD><TITLE>UserManual</TITLE><BODY><UL>"                     > sourceindex.html
	echo $(foreach file,${OTHERFILES},"<LI><A HREF=$(file)>$(file)</A></LI>") >> sourceindex.html
	echo "</UL></BODY></HEAD></HTML>"                                         >> sourceindex.html

builddate.html: 
	echo "<HTML><HEAD><TITLE>UserManual</TITLE><BODY>"                         > builddate.html
	echo "Built on "                                                          >> builddate.html
	date                                                                      >> builddate.html
	echo "</BODY></HEAD></HTML>"                                              >> builddate.html

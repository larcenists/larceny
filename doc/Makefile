STD_ASCIIDOC_OPTS=--conf-file=larcenydoc.conf --section-numbers
STD_XSLTPROC_OPTS=--stringparam section.autolabel 1
STD_A2X_OPTS=--asciidoc-opts="${STD_ASCIIDOC_OPTS}" --xsltproc-opts="${STD_XSLTPROC_OPTS}" --doctype=article --destination-dir=. 

OTHERFILES=$(wildcard UserManual/*.txt)

all: user-manual.pdf user-manual.html user-manual.chunked

user-manual.pdf: UserManual/user-manual.txt ${OTHERFILES}
	a2x ${STD_A2X_OPTS}  --format=pdf UserManual/user-manual.txt 

user-manual.html: UserManual/user-manual.txt ${OTHERFILES}
	a2x ${STD_A2X_OPTS}  --format=xhtml UserManual/user-manual.txt 

user-manual-alt.html: UserManual/user-manual.txt ${OTHERFILES}
	asciidoc ${STD_ASCIIDOC_OPTS}  --backend=xhtml11 --out-file=$@ UserManual/user-manual.txt

user-manual.chunked: UserManual/user-manual.txt ${OTHERFILES}
	a2x ${STD_A2X_OPTS}  --format=chunked UserManual/user-manual.txt 

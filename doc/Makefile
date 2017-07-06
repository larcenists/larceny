# This Makefile is known to work
# for the target(s): artichoke 
# and for the users: pnkfelix henchman 
#
# It would be nice if it works on other targets, but Felix found it
# hard to get things running on his Mac.
#
# FIXME: user-manual-alt.html is being built okay, but the others fail,
# apparently because a2x (which is now written in Python, not Java)
# uses asciidoc to produce an XML document, which a2x rejects unless
# xmllint says it's valid, and it isn't valid because
#
#     asciidoc produces unrecognized <emphasis> tags
#     the linkend attribute of the link tag doesn't allow
#         funny characters such as ?, !, *, and so on
#
# We could work around the second problem, but Will doesn't see any
# obvious way to work around the first.

ASCIIDOC=asciidoc
#A2X=JAVACMD=$(shell which java) a2x
A2X=a2x

STD_ASCIIDOC_OPTS=--conf-file=larcenydoc.conf --section-numbers
TOC_OPTS=-a toc -a toclevels=1
STD_XSLTPROC_OPTS=--stringparam section.autolabel 1
STD_A2X_OPTS=--asciidoc-opts="${STD_ASCIIDOC_OPTS}" --xsltproc-opts="${STD_XSLTPROC_OPTS}" --doctype=article --destination-dir=. 

OTHERFILES=larcenydoc.conf $(wildcard UserManual/*.txt)
LNOTEFILES=larcenydoc.conf $(wildcard LarcenyNotes/*.txt)

LOGFILE=buildlog.html

all: log-start docs notes sourceindex.html builddate.html log-finis

#docs:  user-manual.pdf user-manual.html user-manual.chunked/index.html user-manual-alt.html sourceindex.html
docs:  user-manual-alt.html sourceindex.html

notes: larceny-notes.html larceny-notes-alt.html

user-manual.pdf: UserManual/user-manual.txt ${OTHERFILES}
	-${A2X} ${STD_A2X_OPTS}  --format=pdf UserManual/user-manual.txt >> ${LOGFILE} 2>&1

user-manual.html: UserManual/user-manual.txt ${OTHERFILES}
	-${A2X} ${STD_A2X_OPTS}  --format=xhtml UserManual/user-manual.txt >> ${LOGFILE} 2>&1

user-manual-alt-old-style.html: UserManual/user-manual.txt ${OTHERFILES}
	-${ASCIIDOC} ${STD_ASCIIDOC_OPTS} ${TOC_OPTS} --backend=xhtml11 --out-file=$@ UserManual/user-manual.txt >> ${LOGFILE} 2>&1

user-manual-alt.html: UserManual/user-manual.txt ${OTHERFILES}
	-${ASCIIDOC} ${STD_ASCIIDOC_OPTS} ${TOC_OPTS} --backend=html5 --out-file=$@ UserManual/user-manual.txt >> ${LOGFILE} 2>&1

user-manual.chunked/index.html: UserManual/user-manual.txt ${OTHERFILES}
	-${A2X} ${STD_A2X_OPTS}  --format=chunked UserManual/user-manual.txt >> ${LOGFILE} 2>&1

# Don't do this one unless we find a way to conditionally include on 
# PDFs (would also need to shift the relative headings for each note
# down to a chapter...
larceny-notes.pdf: LarcenyNotes/larceny-notes.txt ${LNOTEFILES}
	-${A2X} ${STD_A2X_OPTS}  --format=pdf LarcenyNotes/larceny-notes.txt >> ${LOGFILE} 2>&1

larceny-notes.html: LarcenyNotes/larceny-notes.txt ${LNOTEFILES}
	-${A2X} ${STD_A2X_OPTS}  --format=xhtml LarcenyNotes/larceny-notes.txt >> ${LOGFILE} 2>&1

larceny-notes-alt.html: LarcenyNotes/larceny-notes.txt ${LNOTEFILES}
	-${ASCIIDOC} ${STD_ASCIIDOC_OPTS}  --backend=xhtml11 --out-file=$@ LarcenyNotes/larceny-notes.txt >> ${LOGFILE} 2>&1

larceny-notes.chunked/index.html: LarcenyNotes/larceny-notes.txt ${LNOTEFILES}
	-${A2X} ${STD_A2X_OPTS}  --format=chunked LarcenyNotes/larceny-notes.txt >> ${LOGFILE} 2>&1

sourceindex.html: ${OTHERFILES}
	echo "<HTML><HEAD><TITLE>UserManual</TITLE><BODY><UL>"                     > sourceindex.html
	echo $(foreach file,${OTHERFILES},"<LI><A HREF=$(file)>$(file)</A></LI>") >> sourceindex.html
	echo "</UL></BODY></HEAD></HTML>"                                         >> sourceindex.html

builddate.html: alwaysdo
	echo "<HTML><HEAD><TITLE>UserManual</TITLE></HEAD><BODY>"                         > builddate.html
	echo "Built on "                                                          >> builddate.html
	date                                                                      >> builddate.html
	echo "</BODY></HTML>"                                              >> builddate.html

log-start: 
	echo "<HTML><HEAD>"                            > ${LOGFILE}
	echo "<TITLE>Documentation Build Log</TITLE>" >> ${LOGFILE}
	echo "</HEAD><BODY>"                          >> ${LOGFILE}
	echo "Started on "                            >> ${LOGFILE}
	date                                          >> ${LOGFILE}
	echo "<PRE>"                                  >> ${LOGFILE}
log-finis:
	echo "</PRE>"                                 >> ${LOGFILE}
	echo "Finished on "                           >> ${LOGFILE}
	date                                          >> ${LOGFILE}
	echo "</BODY></HTML>"                         >> ${LOGFILE}
# Empty target so that things like builddate.html are always remade
alwaysdo:

Copyright 2007 William D Clinger

$Id$

ParseGen grammar for R6RS Scheme as extended by SRFI 38.
Used with r6rsTokens.sch.

The parser never sees whitespace or comments.
#| ... |# are handled by magic in the scanner.
#!r6rs comments are similar to #| ... |# comments.
#; <datum> comments are handled by having the scanner
call the parser.

*terminals

eofobj
miscflag
id
boolean
number
character
string
lparen
rparen
lbracket
rbracket
vecstart
bvecstart
quote
backquote
comma
splicing
period
syntax
quasisyntax
unsyntax
unsyntaxsplicing
sharingdef
sharinguse

*productions

<outermost-datum> ::=  eofobj                                   #makeEOF
                  ::=  <datum>                                  #identity
<datum>           ::=  <udatum>                                 #identity
                  ::=  <sharingdef> <udatum>                    #makeSharingDef
                  ::=  <sharinguse>                             #makeSharingUse
<udatum>          ::=  boolean                                  #makeBool
                  ::=  number                                   #makeNum
                  ::=  character                                #makeChar
                  ::=  string                                   #makeString
                  ::=  id                                       #makeSym
                  ::=  miscflag                                 #makeFlag
                  ::=  <location> <structured>                  #makeStructured
<structured>      ::=  <list>                                   #identity
                  ::=  <vector>                                 #identity
                  ::=  <bytevector>                             #identity
<string>          ::=  string                                   #makeString
<symbol>          ::=  id                                       #makeSym
<list>            ::=  lparen <list2>                           #identity
                  ::=  lbracket <blst2>                         #identity
                  ::=  <abbreviation>                           #identity
<list2>           ::=  rparen                                   #emptyList
                  ::=  <datum> <list3>                          #cons
<list3>           ::=  <data> <list4>                           #pseudoAppend
<list4>           ::=  rparen                                   #emptyList
                  ::=  period <datum> rparen                    #identity
<blst2>           ::=  rbracket                                 #emptyList
                  ::=  <datum> <blst3>                          #cons
<blst3>           ::=  <data> <blst4>                           #pseudoAppend
<blst4>           ::=  rbracket                                 #emptyList
                  ::=  period <datum> rbracket                  #identity

<abbreviation>    ::=  <abbrev-prefix> <datum>            #list
<abbrev-prefix>   ::=  quote                              #symQuote
                  ::=  backquote                          #symBackquote
                  ::=  comma                              #symUnquote
                  ::=  splicing                           #symSplicing
                  ::=  syntax                             #symSyntax
                  ::=  quasisyntax                        #symQuasisyntax
                  ::=  unsyntax                           #symUnsyntax
                  ::=  unsyntaxsplicing                   #symUnsyntax-splicing
<vector>          ::=  vecstart <data> rparen             #list2vector
<bytevector>      ::=  bvecstart <octets> rparen          #list2bytevector

<data>            ::=                                           #emptyList
                  ::=  <datum> <data>                           #cons
<octets>          ::=                                           #emptyList
                  ::=  <octet> <octets>                         #cons
<octet>           ::=  number                                   #makeOctet

<location>        ::=                                           #sourceLocation

<sharingdef>      ::=  sharingdef                               #sharingDef
<sharinguse>      ::=  sharinguse                               #sharingUse

*end

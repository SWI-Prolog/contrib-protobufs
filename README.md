---+ Google's Protocol Buffers

Protocol  buffers  are  Google's    language-neutral,  platform-neutral,
extensible mechanism for serializing structured data   –  think XML, but
smaller, faster, and simpler. You define how   you  want your data to be
structured once. This takes the form of   a  template that describes the
data structure. You use this template  to   encode  and decode your data
structure into wire-streams that may be sent-to or read-from your peers.
The underlying wire stream is platform independent, lossless, and may be
used to interwork with a variety of  languages and systems regardless of
word size or endianness. 

This document was produced using PlDoc, with sources found in protobufs.pl
and protobufs_overview.txt.

@see http://code.google.com/apis/protocolbuffers
@author: Jeffrey Rosenwald (JeffRose@acm.org)
@license: LGPL
@compat: SWI-Prolog

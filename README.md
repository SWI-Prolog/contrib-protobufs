# Google's Protocol Buffers

Protocol  buffers  are  Google's    language-neutral,  platform-neutral,
extensible mechanism for serializing structured data   –  think XML, but
smaller, faster, and simpler. You define how   you  want your data to be
structured once. This takes the form of   a  template that describes the
data structure. You use this template  to   encode  and decode your data
structure into wire-streams that may be sent-to or read-from your peers.
The underlying wire stream is platform independent, lossless, and may be
used to interwork with a variety of  languages and systems regardless of
word size or endianness.

This document was produced using PlDoc, with sources found in
protobufs.pl and protobufs_overview.md. There is a simple example
(assuming you've installed `swi-prolog-doc`) in
`/usr/share/swi-prolog/doc/packages/examples/protobufs/interop/addressbook.pl`:
```
protoc -I/usr/share/doc/protobuf-compiler/examples -I/usr/include --swipl_out=/tmp/ \
    --plugin=protoc-gen-swipl=/usr/lib/swi-prolog/library/protobufs/protoc-gen-swipl \
    /usr/share/swi-prolog/doc/packages/examples/protobufs/interop/addressbook.pl
```

This generates `/tmp/addressbook_pb.pl` … you should change the
`--swipl-out=` specification to be the directory you want (e.g.,
`interop`).

Note that `protoc` requires an `-I` item that encompasses the `.proto`
file, so this also works:
```
protoc -I -I/usr/include --swipl_out=/tmp/ \
    -I/usr/share/swi-prolog/doc/packages/examples/protobufs/interop \
    --plugin=protoc-gen-swipl=/usr/lib/swi-prolog/library/protobufs/protoc-gen-swipl \
    addressbook.proto
```
and, if `/usr/lib/swi-prolog/library/protobufs` is in your `PATH`, you can
leave out the `--plugin` option:
```
EXPORT PATH=/usr/lib/swi-prolog/library/protobufs:$PATH
protoc -I/usr/share/swi-prolog/doc/packages/examples/protobufs/interop -I/usr/include --swipl_out=/tmp/ addressbook.proto
```

It’s possible that your installation puts files in a different place; you can find the addressbook example with:
```
dpkg -L protobuf-compiler | grep addressbook
```
To test this, run the following:
```
swipl -g test_write -g test_write -g test_read -t halt addressbook.pl  # outputs to addressbook.wire
protoc --decode=tutorial.AddressBook addressbook.proto <addressbook.wire
```

@see https://developers.google.com/protocol-buffers
@author Jeffrey Rosenwald (JeffRose@acm.org), Peter Ludemann (peter.ludemann@gmail.com)
@license BSD-2

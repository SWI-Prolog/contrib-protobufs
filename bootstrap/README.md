# Bootstrap for the protoc plugin

## Installing protobuf (on Ubuntu)

You can use the Ubuntu package `protobuf-compiler`, but it's dated Jul
31, 2018.  Instead, you can clone from
https://github.com/protocolbuffers/protobuf and build using the
instructions in `protobuf/src/README.md`. For the `./configure`
command you may wish to use `./configure --prefix=$HOME/.local` and
`make -j4` (where "4" should be replaced by the number of cores on
your machine).

There are some additional notes on this in the `Makefile`.


## descriptor.proto and friends

Naming conventions:

| x.proto      | protobuf definition: used as input to protoc                         |
| x.wire       | wire-format encoding of data (e.g., produced by SerializeToString()) |
| x.proto.wire | `protoc --descriptor_set_out=x.proto.wire`                  |
| x_pb2.py     | generated by protoc from x.proto (for Python)                        |
| x.pb.{h,cc}  | generated by protoc from x.proto (for C++)                           |
| x.wiredump   | readable form of x.wire, using `protoc --decode` and the appropriate .proto file |
| x.wirerawdump | readable from of x.wire, using `protoc --decode_raw` (no .proto file) |
| x.segment    | Prolog term that contains a segmentation of the data in a .wire file (see also x.wiredump) |
| descriptor.proto.parse | Output from `parse_descriptor_proto_dump.pl`, which is hand-edited into `descriptor_proto.pl` (`descriptor_proto/1`).  |

For a regular .proto file, the path is short. E.g., for `addressbook.proto` (which imports
`ddressbook2.proto`, which imports `timestamp.proto`):

* Generate `addressbook.proto.wire` (using `protoc`).
* Generate `addressbook.proto.segment` (using `descriptor_proto.pl`).
  The two imported files are included in the result.

`descriptor.proto` can be used to encode all `.proto` files created by
the protobuf compiler (`protoc`):

* `descriptor.proto.wire` contains a protobuf encoding of
  `descriptor.proto`; it was generated by
  `protoc --descriptor_set_out=descriptor.proto.wire`
* `descriptor.proto.wiredump` is generated by `protoc
  --decode=FileDescriptorSet descriptor.proto.wire`
  It is included here, to avoid a submodule dependency on
  git@github.com:protocolbuffers/protobuf.git
* `parse_descriptor_proto_dump.pl` is used to process `descriptor.proto.wiredump`
  into a Prolog term.
* `descriptor_proto.pl` is the term created by `parse_descriptor_proto_dump.pl`,
  in the clause `descriptor_proto/1`.
* `descriptor.proto.segment` is the "golden" output from rule `descriptor.segment`.
  TODO: delete this file.

TODO: use `protobuf\_segment\_message/2` to process
`descriptor.proto.wire` and any other `descriptor\_set\_out`, and then
generate Prolog code for easy handling of protobufs.

## protoc plugin

We can create a plugin by having an executable named `protoc-gen-swipl`
that is in the `PATH`, which reads a `CodeGeneratorRequest` from stdin
(defined in `src/protobuf/src/google/protobuf/compiler/plugin.proto`)
and outputs to stdout a `CodeGeneratorResponse` ... to get this
plugin, use `protoc --swipl_out=DIR`.

See also
[https://chromium.googlesource.com/external/github.com/protocolbuffers/protobuf/+/refs/heads/master/docs/implementing_proto3_presence.md](How To Implement Field Presence for Proto3).
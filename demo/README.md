# Examples for handling Google's Protocol Buffers

## Makefile

The Makefile contains a grab-bag of simple rules, to make running some
of the examples easier. The default goal is `check`. Other convenience
goals exist - see the `.PHONY` rule to see what they are

## vector_demo.pl

Contains code snippets that correspond to the documentation
in ../protobufs_overview.md. There are also some basic tests
in here.

TODO: separate out the tests into a proper test suite.

## descriptor.proto and friends

`descriptor.proto` can be used to encode all `.proto` files created by
the protobuf compiler (`protoc`).

* `descriptor.proto.msg` contains a protobuf encoding of
  `descriptor.proto`; it was generated by `protoc
  --descriptor_set_out=descriptor.proto.msg`
* `descriptor.proto.dump` is generated by `protoc
  --decode=FileDescriptorSet descriptor.proto.msg`
* `parse_descriptor_proto_dump.pl` is used to process `descriptor.proto.dump`
  into a Prolog term.
* `descriptor_proto.pl` is the term created by `parse_descriptor_proto_dump.pl`,
  in the clause `descriptor_proto/1`.

TODO: use `protobuf\_segment\_message/2` to process
`descriptor.proto.msg` and any other `descriptor\_set\_out`, and then
generate Prolog code for easy handling of protobufs.
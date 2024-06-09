# Protobufs meta-data for plugin.proto, descriptor.proto (used by protoc-gen-swipl)

The two files here (`google/protobuf/compiler/plugin_pb.pl` and
`google/protobuf/descriptor_pb.pl` can be generated using
`protoc-gen-swipl` as part of the bootstrap process.

The files were actually generated using
`parse_descriptor_proto_dump.pl` and extracting the term expansion,
plus a bit of editing to make them easier to read (for debugging).

Here's the result of running "make" in the parent directory
(`protobufs/bootstrap`) on `plugin.proto` (`descriptor.proto` is
similar):

```
$HOME/src/protobuf/src/protoc -I. -I$HOME/src/protobuf/src -I$HOME/src/protobuf/src/google/protobuf -I$HOME/src/protobuf/src/google/protobuf/compiler --include_imports --descriptor_set_out=plugin.proto.wire plugin.proto
$HOME/src/protobuf/src/protoc -I. -I$HOME/src/protobuf/src -I$HOME/src/protobuf/src/google/protobuf -I$HOME/src/protobuf/src/google/protobuf/compiler \
	--decode=google.protobuf.FileDescriptorSet \
	descriptor.proto \
	<plugin.proto.wire >plugin.proto.wiredump
$HOME/src/swipl-devel/build/src/swipl -g "parse_wiredump('plugin.proto.wiredump')" \
	-g halt parse_descriptor_proto_dump.pl >plugin.proto.parse
```

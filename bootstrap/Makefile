# Bootstrap the protoc plugin

# README.md for more details.

# Assumes that this is a submodule of
# git@github.com:SWI-Prolog/swipl-devel.git and that the swipl
# executable has been built (see ../../../CMAKE.md).

# Assumes that you have downloaded
# git@github.com:protocolbuffers/protobuf.git and built it following
# the instructions. If you prefer to download (from
# https://developers.google.com/protocol-buffers/docs/downloads) or
# use the PPA (package `protobuf-compiler`, that will probably work
# with some tweaking of things like PROTOC. (Note that the PPA was
# fairly old at the time of writing this and I don't know whether that
# matters or not.)

# Assumption: protoc and swipl are in your $PATH ... you can override
# these by setting PROTOC= or SWIPL= when envoking "make".

# This has been run on Ubuntu 20.0.4. It will probably run on other Linuxes
# but trying to run it on Windows (except as Windows Subsystem for Linux)
# will likely end in tears.

# Many of the rules at the end of this Makefile are for development
# and will eventually be removed.

# Naming conventions - see README.md section "descriptor.proto and friends"

include common.mk

.SUFFIXES: .proto .pl .py .wire .wiredump .wirerawdump .segment

.DELETE_ON_ERROR: # Any non-zero return code deletes the target file(s)

.PHONY: FORCE all clean docs test_segment_messages bootstrap

.DEFAULT_GOAL=all

test: test_segment_messages descriptor.proto.segment

all: plugin.proto.wiredump plugin.proto.wire plugin.proto.parse plugin.proto.segment \
	descriptor.proto.wiredump descriptor.proto.wire descriptor.proto.parse descriptor.proto.segment \
	bootstrap

.PHONY: bootstrap
# The *_pb.qlf files aren't used (just the *_pb.pl files)
# but are generated for the "--undefined" check.
bootstrap: protoc_gen_prolog_pb/plugin_pb.qlf \
	   protoc_gen_prolog_pb/descriptor_pb.qlf
	$(RM) protoc_gen_prolog_pb/plugin_pb.qlf \
	      protoc_gen_prolog_pb/descriptor_pb.qlf

%_pb.pl: %.proto protoc-gen-swipl
	@# PATH=".:$$PATH" $(PROTOC) $(PROTOC_I) --swipl_out=. $<
	@# TODO: s/OUT_PARAMETER/all/ ? (see comment in protoc-gen-swipl
	@#                              with Request.parameter)
	PATH=$(PATH_WITH_SWIPL) $(PROTOC) $(PROTOC_I) --swipl_out='OUT_PARAMETER':. \
		--plugin=protoc-gen-swipl=$(realpath protoc-gen-swipl) $<

%.qlf: %.pl
	@# --undefined is for double-checking
	$(SWIPL) --undefined=error --verbose=false -o $*.qlf -c <$*.pl

protoc_gen_prolog_pb/plugin_pb.pl: $(PLUGIN_PROTO) protoc-gen-swipl
	PATH=$(PATH_WITH_SWIPL) $(PROTOC) $(PROTOC_I) \
	--plugin=protoc-gen-swipl=$(realpath protoc-gen-swipl) \
	--swipl_out=protoc_gen_prolog_pb plugin.proto

protoc_gen_prolog_pb/descriptor_pb.pl: $(DESCRIPTOR_PROTO) protoc-gen-swipl
	PATH=$(PATH_WITH_SWIPL) $(PROTOC) $(PROTOC_I) \
	--plugin=protoc-gen-swipl=$(realpath protoc-gen-swipl) \
	--swipl_out=protoc_gen_prolog_pb descriptor.proto

# Protobuf code generator for Python  # TODO: delete when no longer needed

%_pb2.py: %.proto
	$(PROTOC) $(PROTOC_I) --python_out=. $<

plugin_pb2.py: $(PLUGIN_PROTO) $(DESCRIPTOR_PROTO)
	$(PROTOC) $(PROTOC_I) --python_out=. $(notdir $<)

descriptor_pb2.py: $(DESCRIPTOR_PROTO) 
	$(PROTOC) $(PROTOC_I) --python_out=. $(notdir $<)

# Bootstrap rule for prolog plugin

# Make a binary protobuf msg file (see $(DESCRIPTOR_PROTO)) with a
# FileDescriptorSet message, describing the .proto and all its
# dependencies:
%.proto.wire: %.proto
	$(PROTOC) $(PROTOC_I) --include_imports --descriptor_set_out=$@ $<

descriptor.proto.wire: $(DESCRIPTOR_PROTO)
	$(PROTOC) $(PROTOC_I) --include_imports --descriptor_set_out=$@ $(notdir $<)

plugin.proto.wire: $(PLUGIN_PROTO)
	$(PROTOC) $(PROTOC_I) --include_imports --descriptor_set_out=$@ $(notdir $<)

# Run the protobuf_segment_messages test.
# See also rules descriptor.proto.wirerawdump, descriptor.proto.wiredump
test_segment_messages: descriptor.proto.wire tests.pl FORCE
	$(SWIPL) -g test_segment_messages -g halt tests.pl

%.proto.wiredump: %.proto.wire
	$(PROTOC) $(PROTOC_I) \
		--decode=google.protobuf.FileDescriptorSet \
		descriptor.proto \
		<$< >$@

%.proto.segment: descriptor_proto.pl descriptor_proto_expand.pl %.proto.wire
	$(SWIPL) descriptor_proto.pl <$*.proto.wire >$@

%.proto.parse: %.proto.wiredump parse_descriptor_proto_dump.pl
	$(SWIPL) -g "parse_wiredump('$<')" \
		-g halt parse_descriptor_proto_dump.pl >$@

descriptor.proto.wirerawdump: descriptor.proto.wire
	$(PROTOC) --decode_raw <descriptor.proto.wire >$@

clean:
	@# TODO: special handling for descriptor.* files
	$(RM) -r foo *.tmp *.o *.pb.cc *.pb.h *_pb2.py *_pb.pl *_pb0.pl doc/ ../TAGS \
		*.proto.wire *.proto.wirerawdump *.proto.wiredump *.proto.parse __pycache__ \
		descriptor.proto.segment plugin.proto.segment
	git clean -ndxf  # Should find nothing.

# For testing: this is what the build does.

.PHONY: ctest_protobufs
ctest_protobufs:
	@# The top level runs protobufs:protobufs:
	@# $(SWIPL) "-p" "foreign=" "-f" "none" "--no-packs" "-s" ../test_protobufs.pl "-g" "test_protobufs" "-t" "halt"
	@# And ../CMakeLists.txt runs the others
	@# The following command runs everything (assuming that directory "build" exists):
	cd ../../../build && cmake -G Ninja .. && ninja && ctest  -j5 -R protobufs

.PHONY: test_protobufs
test_protobufs:
	cd .. && $(SWIPL) -g run_tests -t halt test_protobufs.pl

# For looking at the generated documentation.  This assumes that
# you're working in ~/src/contrib-protobufs and that there's also
# ~/src/swipl-devel (and you might wish to stash
# ~/src/swipl-devel/packages/protobufs somewhere).
# Also, cmake doesn't seem to play nicely with symlinks,
# so we can't just do
#    ln -s $(HOME)/src/contrib-protobufs $(HOME)/src/swipl-devel/packages/protobufs

.PHONY: rebuild rebuild_clean rebuild other_tests

rebuild_clean:
	cd ../../.. && git clean -dxf
	$(MAKE) rebuild

rebuild:
	cd ../../.. && \
		mkdir -p build && \
		cd build && \
		cmake -G Ninja .. && \
		ninja && \
		ctest -j8
	@# gio open $(HOME)/src/swipl-devel/build/packages/protobufs/protobufs.html

rebuild0: # same as rebuild but without the test
	cd ../../.. && \
		mkdir -p build && \
		cd build && \
		cmake -G Ninja .. && \
		ninja

# Generate the documentation from ../protobufs.pl
# The result is in ../doc/protobufs.html
docs:
	cd .. && $(SWIPL) -g 'use_module(library(doc_files))' \
		-g 'doc_save(.,[])' -g halt protobufs.pl

.PHONY: tar
tar: # TODO: remove this
	$(MAKE) clean
	$(MAKE) -C ../interop clean
	$(MAKE) -C ../demo clean
	cd ../../.. && tar --create --bzip2 --file ~/Downloads/contrib-protobufs-$$(date +%Y-%m-%d-%H-%M).tbz2 packages/protobufs
	-$(MAKE)
	-$(MAKE) -C ../demo
	-$(MAKE) -C ../interop
	$(MAKE) etags

.PHONY: etags
etags:	../TAGS
../TAGS: ../*.pl ./*.pl ../interop/*.pl
	etags -l prolog -o $@ ../*.pl ./*.pl ../interop/*.pl

.PHONY: tkdiff
tkdiff:
	git difftool --tool=tkdiff --no-prompt

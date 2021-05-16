# Demo of protobufs.

# To run this, you must install protobufs first. The easiest way is:
#    sudo apt install protobuf-compiler
# Or you can get the latest source and buid it:
#    https://github.com/protocolbuffers/protobuf/blob/master/src/README.md

# The demo is run by:
#   make check

# Assumption: protoc and swipl are in your $PATH ... you can override
# these by setting PROTOC= or SWIPL= when envoking "make".

# If you're having problems with the gcc flags, you might need to set
# PKGCONFIG_PATH. For example, if you install the protobuf compiler
# in $HOME/.local (./configure --prefix=$HOME/.local), then set
# PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig

# You might also need to set LD_LIBRARY_PATH or change the rule for
# "foo" to be: $(CXX) -static -o $@ ...

# This demo was tested with protoc versions 3.6.1 and 3.15.8
# on Ubuntu 20.04.2.

# Many of the rules at the end of this Makefile are for development
# and will eventually be removed.

# Naming conventions - see README.md section "descriptor.proto and friends"

.SUFFIXES: .proto .cpp .cc .o .pl .py .wire .wiredump .wirerawdump .segment

.DELETE_ON_ERROR: # Any non-zero return code deletes the target file(s)

.PHONY: FORCE check clean docs
.PHONY: test_all test_basic_usage test_segment_messages test_send_command test_send_precompiled_command

.DEFAULT_GOAL=test_all

test_all: check test_basic_usage test_segment_messages test_send_command test_send_precompiled_command other_tests \
	addressbook.proto.segment descriptor.proto.segment

# SHELL:=/bin/bash
# PROTOC=$(shell type -p protoc)
PROTOC=protoc
# SWIPL=swipl # DO NOT SUBMIT
# DO NOT SUBMIT:
SWIPL=$(HOME)/src/swipl-devel/build/src/swipl

# The following correspond to the default rules in GNU Make 4.2.1
# (There are no *.c files, only C++)
# CXX=g++
RM=rm -f

# The following is essentially GNU Make's built-in rule::
# %o: %.cpp
# 	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $^

# -Wall produces warnings from the generated *.pb.cc files
CFLAGS=-O3
CXXFLAGS=-O3
# flags from pkg-config: -pthread -lprotobuf -lpthread
CPPFLAGS:=$(shell pkg-config --cflags protobuf)
LDFLAGS:=$(shell pkg-config --libs protobuf)

# SRC_PROTOBUF is where you've downloaded the protobuf sources
# (e.g., by git clone). This is not needed for "make check".
SRC_PROTOBUF=$(HOME)/src/protobuf

# These files don't need to be kept, but can be useful for debugging:
.PRECIOUS: %.pb.cc %.pb.h %_pb2.py

# Protobuf code generator for C++
%.pb.cc %.pb.h: %.proto
	$(PROTOC) --cpp_out=. $?

# Protobuf code generator for Python
%_pb2.py: %.proto
	$(PROTOC) --python_out=. $?

# Make a binary protobuf msg file (see
# $(SRC_PROTOBUF)/src/google/protobuf/descriptor.proto) with a
# FileDescriptorSet message, describing the .proto and all its
# dependencies:
%.proto.wire: %.proto
	$(PROTOC) --include_imports --descriptor_set_out=$@ $?

descriptor.proto.wire: $(SRC_PROTOBUF)/src/google/protobuf/descriptor.proto
	$(PROTOC) --include_imports --descriptor_set_out=$@ \
		-I$(SRC_PROTOBUF)/src/google/protobuf \
		descriptor.proto

foo.o: pb_vector.pb.h foo.cpp

# foo: foo.cpp pb_vector.pb.cc pb_vector.pb.h
foo: pb_vector.pb.o foo.o
	@# To run, you might require setting LD_LIBRARY_PATH or specify -static
	$(CXX) -o $@ $^ $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)

tmp99.tmp: vector_demo.pl ../eventually_implies.pl
	$(SWIPL) -s vector_demo.pl -g make_tmp99 --

check: foo tmp99.tmp FORCE
	./foo <tmp99.tmp
	$(PROTOC) --decode_raw <tmp99.tmp
	$(PROTOC) --decode=Vector pb_vector.proto <tmp99.tmp

# Run the basic_usage example
test_basic_usage: vector_demo.pl FORCE
	$(SWIPL) -g test_basic_usage -g halt vector_demo.pl

# Run the protobuf_segment_messages test.
# See also rules  descriptor.proto.wirerawdump, descriptor.proto.wiredump

test_segment_messages: descriptor.proto.wire vector_demo.pl FORCE
	$(SWIPL) -g test_segment_messages -g halt vector_demo.pl

test_send_command: vector_demo.pl FORCE
	$(SWIPL) -g test_send_command -g halt vector_demo.pl

test_send_precompiled_command: vector_demo.pl FORCE
	$(SWIPL) -g test_send_precompiled_command -g halt vector_demo.pl

clean:
	@# TODO: special handling for descriptor.* files
	$(RM) -r foo *.tmp *.o *.pb.cc *.pb.h *_pb2.py doc/ ../TAGS \
		*.proto.wire *.proto.wirerawdump *.proto.wiredump descriptor.proto.parse
	git clean -dxf  # Should find nothing.

# Generate the documentation from ../protobufs.pl
# The result is in ../doc/protobufs.html
docs:
	cd .. && $(SWIPL) -g 'use_module(library(doc_files))' \
		-g 'doc_save(.,[])' -g halt protobufs.pl

# TODO: remove the following

SRC_PROTOBUF=$(HOME)/src/protobuf

%.proto.wiredump: %.proto.wire
	$(PROTOC) -I. -I$(SRC_PROTOBUF)/src/google/protobuf \
		--decode=google.protobuf.FileDescriptorSet \
		descriptor.proto \
		<$? >$@

# make SWIPL=$HOME/src/swipl-devel/build/src/swipl -C ~/src/swipl-devel/packages/protobufs/demo rebuild0 addressbook.segment # descriptor.segment
%.proto.segment: descriptor_proto.pl descriptor_proto_expand.pl %.proto.wire
	$(SWIPL) descriptor_proto.pl <$*.proto.wire >$@

descriptor.proto.parse: descriptor.proto.wiredump parse_descriptor_proto_dump.pl FORCE
	$(SWIPL) -g "parse_file('descriptor.proto.wiredump')" \
		-g halt parse_descriptor_proto_dump.pl >$@

descriptor.proto.wirerawdump: descriptor.proto.wire
	$(PROTOC) --decode_raw <descriptor.proto.wire >$@

# Assume you've cloned git@github.com:protocolbuffers/protobuf.git (fetch) to $(HOME)/src.
# Note that ../golden_message.2.5.0 is the same as $(HOME)/src/protobuf/python/compatibility_tests/v2.5.0/tests/google/protobuf/internal/golden_message
dump_golden:
	$(PROTOC) -I$(HOME)/src/protobuf/src \
		--decode=protobuf_unittest.TestAllTypes \
		google/protobuf/unittest.proto \
		<../golden_message.2.5.0

# For testing: this is what the build does:

.PHONY: test_protobufs
test_protobufs:
	$(SWIPL) "-p" "foreign=" "-f" "none" "--no-packs" "-s" ../test_protobufs.pl "-g" "test_protobufs" "-t" "halt"

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

other_tests:
	@# "ctest -V -R protobufs" is envoked by "test_protobufs"
	cd ../../../build && ninja && ctest -V -R protobufs
	$(MAKE) -C ../interop test

.PHONY: testx
testx:
	cd ../../../build && ninja
	make SWIPL=$(HOME)/src/swipl-devel/build/src/swipl $(HOME)/src/swipl-devel/packages/protobufs/demo test_all

.PHONY: tar
tar:
	cd ../../.. && tar --create --bzip2 --file ~/Downloads/contrib-protobufs-$$(date +%Y-%m-%d-%H-%M).tjz packages/protobufs

.PHONY: etags
etags:	../TAGS
../TAGS: ../*.pl ./*.pl ../interop/*.pl
	etags -l prolog -o $@ ../*.pl ./*.pl ../interop/*.pl

.PHONY: tkdiff
tkdiff:
	git difftool --tool=tkdiff --no-prompt

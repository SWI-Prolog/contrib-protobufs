# Demo of protobufs.

# To run this, you must install protobufs first. The easiest way is:
#    sudo apt install protobuf-compiler
# Or you can get the latest source and buid it:
#    https://github.com/protocolbuffers/protobuf/blob/master/src/README.md

# The demo is run by:
#   make check

# Assumption: protoc is in your $PATH

# If you're having problems with the gcc flags, you might need to set
# PKGCONFIG_PATH. For example, if you install the protobuf compiler
# in $HOME/.local (./configure --prefix=$HOME/.local), then set
# PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig

# You might also need to set LD_LIBRARY_PATH or change the rule for
# "foo" to be: $(CXX) -static -o $@ ...

# This demo was tested with protoc versions 3.6.1 and 3.15.8
# on Ubuntu 20.04.2.

.SUFFIXES: .proto .cpp .cc .o .proto.msg .py

.PHONY: all check FORCE

.DEFAULT_GOAL=check

PROTOC=protoc
SWIPL=swipl

# The following correspond to the default rules in GNU Make 4.2.1
# CC=gcc
# CXX=g++
RM=rm -f

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
.PRECIOUS: %.pb.cc %.pb.h

%.pb.cc %.pb.h: %.proto
	$(PROTOC) --cpp_out=. $?

%_pb2.py: %.proto
	$(PROTOC) --python_out=. $?

# special case for Python, which doesn't allow "-" in imports
pb_vector_pb2.py: pb-vector.proto
	$(PROTOC) --python_out=. $?

# A rule to make a binary file (see $(SRC_PROTOBUF)/src/google/protobuf/descriptor.proto)
# with a FileDescriptorSet message, that describes the .proto and all its dependencies:
%.proto.msg: %.proto
	$(PROTOC) --include_imports --descriptor_set_out=$@ $?

descriptor.proto.msg: $(SRC_PROTOBUF)/src/google/protobuf/descriptor.proto
	$(PROTOC) --include_imports --descriptor_set_out=$@ -I$(SRC_PROTOBUF)/src/google/protobuf descriptor.proto

# foo: foo.cpp pb-vector.pb.cc pb-vector.pb.h
foo: pb-vector.pb.o foo.o
	@# -static might not be needed, but you might require setting LD_LIBRARY_PATH
	$(CXX) -o $@ $^ $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)

tmp99.tmp: vector_demo.pl ../eventually_implies.pl
	$(SWIPL) -s vector_demo.pl -g make_tmp99 --

check: foo tmp99.tmp FORCE
	./foo <tmp99.tmp
	protoc --decode=Vector pb-vector.proto <tmp99.tmp 

clean:
	$(RM) foo *.o *.pb.cc *.pb.h *.tmp *_pb2.py *.proto.msg

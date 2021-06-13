# Protobuf interoperability tests

# See comments in ../demo/Makefile about PKG_CONFIG_PATH

.DEFAULT_GOAL=test
.PHONY=test clean FORCE run_test run_test_cc
# SHELL:=/bin/bash
# These files don't need to be kept, but can be useful for debugging:
.PRECIOUS: %.pb.cc %.pb.h %_pb.h

# PROTOC=$(shell type -p protoc)
# PYTHON=$(shell type -p python3)
PROTOC=protoc
PYTHON=python3
SWIPL=swipl

# -Wall produces warnings from the generated *.pb.cc files
CFLAGS=-O0
CXXFLAGS=-O0
# flags from pkg-config: -pthread -lprotobuf -lpthread
CPPFLAGS:=$(shell pkg-config --cflags protobuf)
LDFLAGS:=$(shell pkg-config --libs protobuf)

.SUFFIXES: .proto .py .pl .wire

FORCE:

test: scalars1_wire test_read_prolog test_read_cc test_read_python

# Protobuf code generator for Python and C++
%_pb2.py %.pb.h %.pb.cc: %.proto
	$(PROTOC) --python_out=. --cpp_out=. $?

.PHONY: test_read_prolog
test_read_prolog: test_read.pl scalars1b_from_cc.wire scalars1b_from_cc.wire \
			scalars1b_from_python.wire scalars1b_from_python.wire 
	$(SWIPL) test_read.pl

.PHONY: test_read_python
test_read_python: test_read.py scalars1a_from_prolog.wire scalars1b_from_prolog.wire
	$(PYTHON) test_read.py

.PHONY: test_read_cc
test_read_cc: test_read scalars1a_from_prolog.wire scalars1b_from_prolog.wire
	./test_read

.PHONY: scalars1_wire
scalars1_wire: scalars1a_from_prolog.wire scalars1b_from_prolog.wire \
	scalars1a_from_cc.wire scalars1b_from_cc.wire \
	scalars1a_from_python.wire scalars1b_from_python.wire
	diff scalars1a_from_prolog.wire scalars1a_from_cc.wire
	diff scalars1a_from_prolog.wire scalars1a_from_cc.wire
	diff scalars1a_from_python.wire scalars1a_from_python.wire
	diff scalars1b_from_prolog.wire scalars1b_from_cc.wire
	diff scalars1b_from_prolog.wire scalars1b_from_python.wire
	diff scalars1b_from_python.wire scalars1b_from_cc.wire

scalars1a_from_prolog.wire scalars1b_from_prolog.wire: test_write.pl test_templates.pl
	$(RM) scalars1a_from_prolog.wire scalars1b_from_prolog.wire
	$(SWIPL) ./test_write.pl
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1a_from_prolog.wire
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1b_from_prolog.wire

scalars1a_from_cc.wire scalars1b_from_cc.wire: test_write
	$(RM) scalars1a_from_cc.wire scalars1b_from_cc.wire
	./test_write
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1a_from_cc.wire
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1b_from_cc.wire

scalars1a_from_python.wire scalars1b_from_python.wire \
		repeated1a_from_python.wire packed1a_from_python.wire: \
			test_write.py test_pb2.py
	$(RM) scalars1a_from_python.wire scalars1b_from_python.wire
	$(PYTHON) test_write.py
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1a_from_python.wire
	@# $(PROTOC) -I. --decode=test.Scalars1 test.proto <scalars1b_from_python.wire

# Can use type -p with bash:
PROTOC_WHICH:=$(shell which protoc)
PROTOC_DIR:=$(shell dirname $(PROTOC_WHICH))
PROTOC_LIB:=$(shell realpath $(PROTOC_DIR)/../lib)

test_write: test.pb.o test_write.o
	@# See comment about -static with "foo" rule in ../demo/Makefile
	@# for non-static: -Wl,-rpath=$(PROTOC_LIB)
	$(CXX) -static -o $@ $^ $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)

test_write.o: test.pb.h test.pb.cc test_write.cc

test_read: test.pb.o test_read.o
	@# See comment about -static with "foo" rule in ../demo/Makefile
	@# for non-static: -Wl,-rpath=$(PROTOC_LIB)
	$(CXX) -static -o $@ $^ $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)

test_read.o: test.pb.h test.pb.cc test_read.cc

clean:
	$(RM) -r *_pb2.py *.wire *.o *.pb.h *.pb.cc test_write test_read __pycache__

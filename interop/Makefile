# Protobuf interoperability tests

.DEFAULT_GOAL=test
.PHONY=test clean FORCE
# SHELL:=/bin/bash

# PROTOC=$(shell type -p protoc)
# PYTHON=$(shell type -p python3.7)
PROTOC=protoc
PYTHON=python3.7

.SUFFIXES: .proto .py .pl .wire

FORCE:

test: test1

# Protobuf code generator for Python
%_pb2.py: %.proto
	$(PROTOC) --python_out=. $?

test1: test1_pb2.py test1.py test1.pl FORCE
	$(PYTHON) test1.py

clean:
	$(RM) -r *_pb2.py *.wire

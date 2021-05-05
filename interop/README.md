# Inter-operatbility

This directory contains some tests for verifying inter-operability
between Prolog protobufs and other languages. The initial version of
this uses Python, but it is anticipated that other languages will be
added in the future (e.g., C++, JavaScript).

The tests are *not* part of the standard release process because they
require having the protobuf compiler (`protoc`) installed and also a
version of Python (currently, Python 3.7, but this should be changed
to 3.9 or newer).

To run the tests:
```
make test
```
You can specify these additional variables:
```
make protoc=/path/to/protoc python=/path/to/python3 test
```

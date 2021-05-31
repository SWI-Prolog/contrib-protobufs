cmake_minimum_required(VERSION 3.5)
project(swipl-protobufs)

include("../cmake/PrologPackage.cmake")

AC_CHECK_HEADERS(sys/types.h)

configure_file(config.h.cmake config.h)

swipl_plugin(
    protobufs
    C_SOURCES protobufs.c
    PL_LIBS protobufs.pl)

# TODO: the following doesn't do anything):
install(FILES bootstrap/protoc-gen-swipl DESTINATION ${SWIPL_INSTALL_PREFIX})

# TODO: remove the following
swipl_examples(bootstrap/protoc-gen-swipl
               SUBDIR protoc
               )

swipl_examples(
               some_message.proto
               some_message.py
               eventually_implies.pl
               )
# TODO: addressbook.proto.segment
#       descriptor.proto.segment
#       although they can be computed, so maybe not
swipl_examples(
               demo/Makefile
               demo/README.md
               demo/addressbook.proto
               demo/addressbook2.proto
               demo/foo.cpp
               demo/pb_vector.proto
               demo/vector_demo.pl
               SUBDIR demo
               )
swipl_examples(
               interop/Makefile
               interop/README.md
               interop/scalar_test_common.pl
               interop/scalar_test.proto
               interop/scalar_test_read.cc
               interop/scalar_test_read.pl
               interop/scalar_test_read.py
               interop/scalar_test_write.cc
               interop/scalar_test_write.pl
               interop/scalar_test_write.py
               SUBDIR interop
              )

test_libs(protobufs
          TEST_FILES
              golden_message.2.3.0 golden_message.2.5.0
              eventually_implies.pl)

pkg_doc(protobufs
        SECTION
            protobufs_overview.md
            SOURCE protobufs.pl protobufspl.tex)

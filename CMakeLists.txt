cmake_minimum_required(VERSION 3.5)
project(swipl-protobufs)

include("../cmake/PrologPackage.cmake")

AC_CHECK_HEADERS(sys/types.h)

configure_file(config.h.cmake config.h)

swipl_plugin(
    protobufs
    C_SOURCES protobufs.c
    PL_LIBS protobufs.pl)

swipl_examples(
               some_message.proto
               some_message.py
               )
swipl_examples(
               demo/Makefile
               demo/README.md
               demo/addressbook.proto
               demo/addressbook2.proto
               demo/descriptor.proto.dump
               demo/descriptor_proto.pl
               demo/descriptor_proto_expand.pl
               demo/foo.cpp
               demo/parse_descriptor_proto_dump.pl
               demo/pb-vector.proto
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

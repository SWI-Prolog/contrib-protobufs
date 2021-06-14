cmake_minimum_required(VERSION 3.5)
project(swipl-protobufs)

include("../cmake/PrologPackage.cmake")

AC_CHECK_HEADERS(sys/types.h)

configure_file(config.h.cmake config.h)

swipl_plugin(
    protobufs
    C_SOURCES protobufs.c
    PL_LIBS protobufs.pl)

swipl_plugin(
    protoc-gen-swipl
    PL_LIB_SUBDIR protobufs
    PL_LIBS bootstrap/protoc-gen-swipl)

# TODO: the following doesn't do anything:
# install(PROGRAMS bootstrap/protoc-gen-swipl
# 	DESTINATION ${SWIPL_INSTALL_PREFIX}/bin)
# TODO: the following causes errors:
# install(TARGETS bootstrap/protoc-gen-swipl
#         BUNDLE DESTINATION .
#         RUNTIME DESTINATION ${SWIPL_INSTALL_ARCH_EXE})

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
               interop/test.proto
               interop/test_read.cc
               interop/test_read.pl
               interop/test_read.py
               interop/test_templates.pl
               interop/test_write.cc
               interop/test_write.pl
               interop/test_write.py
               SUBDIR interop
              )

test_libs(protobufs
          TEST_FILES
              golden_message.2.3.0 golden_message.2.5.0
              eventually_implies.pl)

find_program(PROTOC protoc)
find_program(PKG_CONFIG pkg-config)

# TODO: check minimal versions of protoc, python3, pip protobuf

exec_program("python3 -c 'from google.protobuf import descriptor_pb2'"
             RETURN_VALUE python3_protobuf
             OUTPUT_VARIABLE python3_protobuf_output) # swallow stderr

if(PROTOC AND PKG_CONFIG AND "${python3_protobuf}" EQUAL "0")
    add_test(NAME "${SWIPL_PKG}:bootstrap"
             COMMAND make SWIPL=${PROG_SWIPL} -C ${CMAKE_CURRENT_SOURCE_DIR}/bootstrap clean test)
    add_test(NAME "${SWIPL_PKG}:interop"
             COMMAND make SWIPL=${PROG_SWIPL} -C ${CMAKE_CURRENT_SOURCE_DIR}/interop clean test)
    add_test(NAME "${SWIPL_PKG}:demo"
             COMMAND make SWIPL=${PROG_SWIPL} -C ${CMAKE_CURRENT_SOURCE_DIR}/demo clean test)
else(PROTOC)
    message(WARNING, "Some ${SWIPL_PKG} tests not run because of missing pkg-config, protoc, or python/protobuf.")
endif()

pkg_doc(protobufs
        SECTION
            protobufs_overview.md
            SOURCE protobufs.pl protobufspl.tex)

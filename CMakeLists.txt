cmake_minimum_required(VERSION 3.5)
project(swipl-protobufs)

include("../cmake/PrologPackage.cmake")

AC_CHECK_HEADERS(sys/types.h)

configure_file(config.h.cmake config.h)

swipl_plugin(
    protobufs
    C_SOURCES protobufs.c
    PL_LIBS protobufs.pl)

# The following doesn't work because protoc-gen-swipl has to be marked
# executable. So, just create this "swipl_plugin" by hand:
#    swipl_plugin(
#        protoc-gen-swipl
#        PL_LIB_SUBDIR protobufs
#        PL_LIBS bootstrap/protoc-gen-swipl)

# string(REPLACE "/" "_" src_target "plugin_${name}_${sd}_pl_libs")
install_src(plugin_protoc-gen-swipl_protobufs_pl_libs
            PROGRAMS bootstrap/protoc-gen-swipl
            DESTINATION ${SWIPL_INSTALL_LIBRARY}/protobufs)
add_index(protobufs bootstrap/protoc-gen-swipl)

# See packages/xpce/CMakeLists for how to add .qlf files
# - probably not needed; it turned out that the slow "consults" were
#   due to the debug facts being quite expensive (0.33 secs seconds to
#   load both *_pb.pl files); they've been removed -- without the
#   debug facts, this dropped to 0.04 sec (half of this in library(protobufs).

swipl_plugin(
    protoc-gen-swipl-subdir
    PL_LIB_SUBDIR protobufs/gen_pb/google/protobuf
    PL_LIBS       bootstrap/gen_pb/google/protobuf/descriptor_pb.pl)

swipl_plugin(
    protoc-gen-swipl-subdir2
        PL_LIB_SUBDIR protobufs/gen_pb/google/protobuf/compiler
        PL_LIBS       bootstrap/gen_pb/google/protobuf/compiler/plugin_pb.pl)

swipl_examples(
               some_message.proto
               some_message.py
               eventually_implies.pl
               )
swipl_examples(
               demo/README.md
               demo/Makefile
               demo/foo.cpp
               demo/pb_vector.proto
               demo/vector_demo.pl
               SUBDIR demo
               )
swipl_examples(
               interop/README.md
               interop/Makefile
               interop/addressbook.proto
               interop/addressbook2.proto
               interop/addressbook.pl
               interop/test.proto
               interop/test2.proto
               interop/test2b.proto
               interop/test_read.cc
               interop/test_read.py
               interop/test_interop.pl
               interop/test_templates.pl
               interop/test_write.cc
               interop/test_write.py
               # TODO: the following should go into a subdir (see following stanza):
                 interop/google/protobuf/unittest_import.proto
                 interop/google/protobuf/unittest.proto
                 interop/google/protobuf/unittest_import_public.proto
               SUBDIR interop
              )
# TODO: the following gets an error due to target name containing a "/":
# swipl_examples(
#                interop/google/protobuf/unittest_import.proto
#                interop/google/protobuf/unittest.proto
#                interop/google/protobuf/unittest_import_public.proto
#                SUBDIR interop/google/protobuf
#               )
swipl_examples(
                bootstrap/README.md
                bootstrap/common.mk
                # The following aren't included because they're mainly
                #   for creating protoc and may disappear when the full
                #   bootstrap is operational:
                # bootstrap/descriptor_proto_expand.pl
                # bootstrap/descriptor_proto.pl
                # bootstrap/Makefile
                # bootstrap/parse_descriptor_proto_dump.pl
                # # bootstrap/gen_pb/** (subdir)
                # bootstrap/protoc-gen-swipl
                # bootstrap/tests.pl
                SUBDIR bootstrap
               )

test_libs(protobufs
          TEST_FILES
              golden_message.2.3.0 golden_message.2.5.0
              eventually_implies.pl)

find_program(PROTOC protoc)
find_program(PKG_CONFIG pkg-config)

# TODO: check minimal versions of protoc, python3, pip protobuf

execute_process(COMMAND python3 -c "from google.protobuf import descriptor_pb2"
		RESULT_VARIABLE python3_protobuf
		OUTPUT_VARIABLE python3_protobuf_output
		ERROR_QUIET)

# TODO: The protobuf tests leave stuff in the source directories
#       These can be removed by:
#           for d in interop demo bootstrap; do make -C $d clean; done
#          (also: "git clean -dxf", but that's dangerous)
if(BUILD_TESTING AND TEST_PROTOBUFS_PROTOC)
  if(PROTOC AND PKG_CONFIG AND "${python3_protobuf}" EQUAL "0")
      add_test(NAME "${SWIPL_PKG}:bootstrap"
               COMMAND make SWIPL=${PROG_SWIPL} -C ${CMAKE_CURRENT_SOURCE_DIR}/bootstrap clean test)
      if(USE_GMP)
          add_test(NAME "${SWIPL_PKG}:interop"
                   COMMAND make SWIPL=${PROG_SWIPL} -C ${CMAKE_CURRENT_SOURCE_DIR}/interop clean test run_addressbook)
      else()
          message(WARNING "${SWIPL_PKG}:interop skipped because it needs GMP")
      endif()
      add_test(NAME "${SWIPL_PKG}:demo"
               COMMAND make SWIPL=${PROG_SWIPL} -C ${CMAKE_CURRENT_SOURCE_DIR}/demo clean test)
  else()
      message(WARNING "Some ${SWIPL_PKG} tests not run because of missing pkg-config, protoc, or python/protobuf")
      # message(WARNING "See demo/README.md for how to set up the build and test environment")
      # message("pkg-config: ${PKG_CONFIG}")
      # message("protoc: ${PROTOC}")
      # message("python3_protobuf: ${python3_protobuf}")
  endif()
else()
  if(BUILD_TESTING)
    # message("Some ${SWIPL_PKG} tests not run because TEST_PROTOBUFS_PROTOC not set")
  endif()
endif()

pkg_doc(protobufs
        SECTION
            protobufs_overview.md
            SOURCE protobufs.pl protobufspl.tex)

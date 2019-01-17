cmake_minimum_required(VERSION 3.5)
project(swipl-protobufs)

include("../cmake/PrologPackage.cmake")

AC_CHECK_HEADERS(sys/types.h)

configure_file(config.h.cmake config.h)

swipl_plugin(
    protobufs
    C_SOURCES protobufs.c
    PL_LIBS protobufs.pl)

swipl_examples(demo/foo.cpp demo/Makefile demo/pb-vector.proto
	       demo/vector_demo.pl)

test_libs(protobufs
          TEST_FILES
	      golden_message.2.3.0 golden_message.2.5.0
	      eventually_implies.pl)

pkg_doc(protobufs
	SECTION
	    protobufs_overview.md
	    SOURCE protobufs.pl protobufspl.tex)

cmake_minimum_required(VERSION 2.8.12)
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

test_libs(protobufs)

pkg_doc(protobufs
	SECTION
	    protobufs_overview.md
	    SOURCE protobufs.pl protobufspl.tex)

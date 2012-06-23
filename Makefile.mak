################################################################
# Build the SWI-Prolog protobufs package for MS-Windows
#
# Author: Jeffrey Rosenwald
#
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
EXDIR=$(PKGDOC)\examples\protobufs
CFLAGS=$(CFLAGS) /D__SWI_PROLOG__
PKGDLL=protobufs

OBJS=		protobufs.obj
EXAMPLES=	foo.cpp Makefile pb-vector.proto vector_demo.pl

all:		$(PKGDLL).dll

$(PKGDLL).dll:	$(OBJS)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJS) $(PLLIB)

install:	idll ilib

################################################################
# Testing
################################################################

check::
		$(PUBLICPL) -q -s test_protobufs.pl -g test_protobufs,halt -t 'halt(1)'

################################################################
# Installation
################################################################

idll::
		copy $(PKGDLL).dll "$(BINDIR)"
!IF "$(PDB)" == "true"
		copy $(PKGDLL).pdb "$(BINDIR)"
!ENDIF

ilib::
		copy protobufs.pl "$(PLBASE)\library"
		$(MAKEINDEX)

uninstall::
		del "$(BINDIR)\$(PKGDLL).dll"
		del "$(PLBASE)\library\protobufs.pl"
		$(MAKEINDEX)

html-install::	install-examples
		copy protobufs.html "$(PKGDOC)"

install-examples::
		if not exist "$(EXDIR)\$(NULL)" $(MKDIR) "$(EXDIR)"
		cd demo & @for %f in ($(EXAMPLES)) do @copy %f "$(EXDIR)"

xpce-install::

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.ilk *.pdb 2>nul

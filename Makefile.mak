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
CFLAGS=$(CFLAGS) /D__SWI_PROLOG__
PKGDLL=protobufs

OBJS=	protobufs.obj

all:		$(PKGDLL).dll

$(PKGDLL).dll:	$(OBJS)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJS) $(PLLIB)

!IF "$(CFG)" == "rt"
install:	all idll
!ELSE
install:	all idll ilib
!ENDIF

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
		copy protobufs_overview.txt "$(PLBASE)\library"
		$(MAKEINDEX)

uninstall::
		del "$(BINDIR)\$(PKGDLL).dll"
		del "$(PLBASE)\library\protobufs.pl"
		del "$(PLBASE)\library\protobufs_overview.txt"
		$(MAKEINDEX)

html-install::
		copy protobufs.html "$(PKGDOC)"

xpce-install::

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.ilk *.pdb 2>nul

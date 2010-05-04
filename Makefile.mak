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

PROTOBOBJ=	protobufs.obj

all:		protobufs.dll

protobufs.dll:	$(PROTOBOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(PROTOBOBJ) $(PLLIB)

$(SBLIB):
		chdir libstemmer_c & $(MAKE)

!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idll ilib
!ENDIF

################################################################
# Testing
################################################################

check::
	$(PUBLICPL) -q -s protobufs_check.pl -g protobuf_check --

################################################################
# Installation
################################################################

idll::
		copy protobufs.dll "$(BINDIR)"

ilib::
		copy protobufs.pl "$(PLBASE)\library"
		copy protobufs_overview.txt "$(PLBASE)\library"
		$(MAKEINDEX)

uninstall::
		del "$(BINDIR)\protobufs.dll"
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

#-------------------------------------------------------------------------------
# package creation for JEDI Visual Component Library (and JVCLX)
#-------------------------------------------------------------------------------

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif

!ifndef JCLROOT
JCLROOT = ..\..\..\jcl
!endif

!ifndef BPLDIR
BPLDIR = $(ROOT)\Projects\bpl
!endif

!ifndef DCPDIR
DCPDIR = $(ROOT)\Projects\bpl
!endif

!ifndef LIBDIR
LIBDIR = $(ROOT)\Projects\lib
!endif

#-------------------------------------------------------------------------------

!ifndef EDITION
!error You must specify a EDITION: make -DEDITION=d6 or make -DEDITION=d5p (Packages Generator)
!endif

!ifndef VERSION
!error You must specify a VERSION: make -DVERSION=6
!endif

!ifndef PKGDIR
!error You must specify a PKGDIR: make -DPKGDIR=d6 or make -DPKGDIR=d5std (\packages\* Packages.bpg)
!endif

!ifndef UNITOUTDIR
!error You must specify a UNITOUTDIR: make -DUNITOUTDIR=C:\jvclfolder\lib\d7
!endif

#-------------------------------------------------------------------------------

MAKE = "$(ROOT)\bin\make.exe" -l+
# -$(MAKEFLAGS)

#-------------------------------------------------------------------------------

RUN = ..\..\run
QRUN = ..\..\qrun
JVEXVCLSRC = ..\..\devtools\JvExVCL\src

JVEXVCLSRC_DEP1 = $(JVEXVCLSRC)\JvExControls.macros
JVEXVCLSRC_DEP = $(JVEXVCLSRC_DEP1) $(JVEXVCLSRC)\JvExControls.pas
#-------------------------------------------------------------------------------

default: \
	JvExVCL

JvExVCLFiles = \
	$(RUN)\JvExButtons.pas \
	$(RUN)\JvExCheckLst.pas \
	$(QRUN)\JvExComboEdits.pas \
	$(RUN)\JvExComCtrls.pas \
	$(RUN)\JvExControls.pas \
	$(RUN)\JvExDBGrids.pas \
	$(RUN)\JvExExtCtrls.pas \
	$(RUN)\JvExForms.pas \
	$(RUN)\JvExGrids.pas \
	$(RUN)\JvExMask.pas \
	$(RUN)\JvExStdCtrls.pas \
	$(JVEXVCLSRC_DEP)

################################################################################

JvExVCL: $(JvExVCLFiles)

$(RUN)\JvExButtons.pas: $(JVEXVCLSRC)\JvExButtons.pas $(JVEXVCLSRC_DEP)
	cd ..\..\devtools\JvExVCL
	preprocess.bat $&
	cd ..\..\packages\bin
$(RUN)\JvExCheckLst.pas: $(JVEXVCLSRC)\JvExCheckLst.pas $(JVEXVCLSRC_DEP)
	cd ..\..\devtools\JvExVCL
	preprocess.bat $&
	cd ..\..\packages\bin
$(QRUN)\JvExComboEdits.pas: $(JVEXVCLSRC)\JvExComboEdits.pas $(JVEXVCLSRC_DEP)
	cd ..\..\devtools\JvExVCL
	preprocess.bat $&
	cd ..\..\packages\bin
$(RUN)\JvExComCtrls.pas: $(JVEXVCLSRC)\JvExComCtrls.pas $(JVEXVCLSRC_DEP)
	cd ..\..\devtools\JvExVCL
	preprocess.bat $&
	cd ..\..\packages\bin
$(RUN)\JvExControls.pas: $(JVEXVCLSRC)\JvExControls.pas $(JVEXVCLSRC_DEP1)
	cd ..\..\devtools\JvExVCL
	preprocess.bat $&
	cd ..\..\packages\bin
$(RUN)\JvExDBGrids.pas: $(JVEXVCLSRC)\JvExDBGrids.pas $(JVEXVCLSRC_DEP)
	cd ..\..\devtools\JvExVCL
	preprocess.bat $&
	cd ..\..\packages\bin
$(RUN)\JvExExtCtrls.pas: $(JVEXVCLSRC)\JvExExtCtrls.pas $(JVEXVCLSRC_DEP)
	cd ..\..\devtools\JvExVCL
	preprocess.bat $&
	cd ..\..\packages\bin
$(RUN)\JvExForms.pas: $(JVEXVCLSRC)\JvExForms.pas $(JVEXVCLSRC_DEP)
	cd ..\..\devtools\JvExVCL
	preprocess.bat $&
	cd ..\..\packages\bin
$(RUN)\JvExGrids.pas: $(JVEXVCLSRC)\JvExGrids.pas $(JVEXVCLSRC_DEP)
	cd ..\..\devtools\JvExVCL
	preprocess.bat $&
	cd ..\..\packages\bin
$(RUN)\JvExMask.pas: $(JVEXVCLSRC)\JvExMask.pas $(JVEXVCLSRC_DEP)
	cd ..\..\devtools\JvExVCL
	preprocess.bat $&
	cd ..\..\packages\bin
$(RUN)\JvExStdCtrls.pas: $(JVEXVCLSRC)\JvExStdCtrls.pas $(JVEXVCLSRC_DEP)
	cd ..\..\devtools\JvExVCL
	preprocess.bat $&
	cd ..\..\packages\bin

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

!ifndef HPPDIR
HPPDIR = $(ROOT)\Include\Vcl
#HPPDIR = $(JVCLROOT)\lib\c6
!endif

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

JVCLPACKAGEDIR = ..
JVCLROOT = $(JVCLPACKAGEDIR)\..
DEVTOOLS = $(JVCLROOT)\devtools
DEVTOOLS_BACK = ..\packages\bin

!ifndef DCCOPT
DCCOPT=-Q -M
!endif

MAKE = "$(ROOT)\bin\make.exe" -$(MAKEFLAGS)
DCC = "$(ROOT)\bin\dcc32.exe" $(DCCOPT)

#-------------------------------------------------------------------------------

default: \
  BuildJCLdcpFiles \
  pg.exe \
  Compile \
  Clean


################################################################################
BuildJCLdcpFiles:
        # for C++ targets compile JCL .dcp files
        IF EXIST "$(ROOT)\bin\bcc32.exe" IF NOT EXIST "$(DCPDIR)\CJclVClx.dcp" $(MAKE) -s -f MakeJCLDcp4BCB.mak

################################################################################
Bpg2Make.exe:
        @echo [Compiling: Bpg2Make.exe]
        cd $(DEVTOOLS)
        $(MAKE) -f makefile.mak -s Bpg2Make.exe
        cd $(DEVTOOLS_BACK)
        $(DEVTOOLS)\bin\Bpg2Make.exe "..\$(PKGDIR) Packages.bpg"

################################################################################
GeneratePackages:
        @echo [Compiling: pg.exe]
        cd $(DEVTOOLS)
        #-SET C5PFLAGS=
        #if $(VERSION)==5 SET C5PFLAGS=-LUvcl50
        $(MAKE) -f makefile.mak -s pg.exe
        #-SET C5PFLAGS=
        cd $(DEVTOOLS_BACK)
        echo [Generating: Delphi Packages]
        $(DEVTOOLS)\bin\pg.exe -m=JVCL -p="$(JVCLPACKAGEDIR)" -t=$(EDITION) -x=$(DEVTOOLS)\bin\pgEdit.xml
        IF NOT $(MASTEREDITION)! == ! $(DEVTOOLS)\bin\pg.exe -m=JVCL -p="$(JVCLPACKAGEDIR)" -t=$(MASTEREDITION) -x=$(DEVTOOLS)\bin\pgEdit.xml

################################################################################
pg.exe: Templates GeneratePackages
        # do nothing

################################################################################
Templates:
        @echo [Generating: Templates]
        @type &&|
-Q
-U"$(ROOT)\Lib;$(ROOT)\Lib\Obj"
-R"$(ROOT)\Lib"
-I"$(ROOT)\Include;$(ROOT)\Include\Vcl"
-U"$(DCPDIR);$(LIBDIR);$(BPLDIR)"
-LE"$(BPLDIR)"
-LN"$(DCPDIR)"

-I"$(JCLROOT)\source;$(JCLROOT)\source\common"
-U"$(JCLROOT)\source\common;$(JCLROOT)\source\windows;$(JCLROOT)\source\vcl;$(JCLROOT)\source\visclx"
-U"$(ROOT)\Lib\Obj"

-I"$(JVCLROOT)\common"
-U"$(UNITOUTDIR);$(JVCLROOT)\common;(JVCLROOT)\run;$(JVCLROOT)\design;$(LIBDIR)"
-R"$(JVCLROOT)\Resources"

-N"$(UNITOUTDIR)"
-N1"$(HPPDIR)"
-N2"$(UNITOUTDIR)\obj"
| > ..\$(PKGDIR)\template.cfg
        @IF NOT $(MASTEREDITION)! == ! @copy ..\$(PKGDIR)\template.cfg $(PKGDIR_MASTEREDITION)\template.cfg 

################################################################################
Compile: Bpg2Make.exe CompilePackages

################################################################################
CompilePackages:
        @echo [Compiling: Packages]
        cd $(JVCLPACKAGEDIR)

        # create temporary batch file that calls "make"
        @type &&|
@echo off
IF NOT $(JCLROOT)!==! SET ADDFLAGS=-U$(JCLROOT)\dcu
SET BPLDIR=$(BPLDIR)
SET BPILIBDIR=$(LIBDIR)
SET PATH=$(ROOT)\bin;$(LIBDIR);(BPLDIR);%PATH%
SET LIBDIR=$(LIBDIR)
SET HPPDIR=$(HPPDIR)
SET DCCOPT=$(DCCOPT)
SET DCC=$(DCC)
$(MAKE) -f "$(PKGDIR) Packages.mak" $(TARGETS)
| >tmp.bat
        tmp.bat
        -del tmp.bat >NUL

################################################################################
Clean:
        @echo [Cleaning...]
        -del /f /q "$(PKGDIR) Packages.mak" 2>NUL
        -del /f /q "$(PKGDIR)\*.cfg" "$(PKGDIR)\*.mak" 2>NUL

